module Task.Script.Validator
  ( validate
  ) where

import Preload
import Data.HashMap as HashMap
import Task.Script.Checker (check, match, needBasic, outofBasic, outofRecord, outofReference, outofTask, unite, intersect, wrapValue)
import Task.Script.Context (Typtext, Context)
import Task.Script.Error (Checked(..), Error(..), Unchecked(..), bury, expand, extract, fail, lift, pass, sink, withTypeOf)
import Task.Script.Syntax (Expression, Label, PrimType(..), Row_, Task(..), Type_(..), ofBasic)

---- Validator -----------------------------------------------------------------
validate :: Typtext -> Context -> Unchecked Task -> Checked Task
validate s g (Unchecked i) = case i of
  ---- Editors
  Enter n m -> lift g i (Enter n m) (HashMap.lookup n s |> note (UnknownTypeName n) ||> ofBasic ||> wrapValue)
  Update m e -> lift g i (Update m e) (check s g e ||= needBasic ||> wrapValue)
  Change m e -> lift g i (Change m e) (check s g e ||= outofReference ||> wrapValue)
  View m e -> lift g i (View m e) (check s g e ||= needBasic ||> wrapValue)
  Watch m e -> lift g i (Watch m e) (check s g e ||= outofReference ||> wrapValue)
  Lift e -> lift g i (Lift e) (check s g e ||= outofRecord ||> TTask)
  ---- Combinators
  Pair us -> sink g (Pair cs) (traverse outofBranch cs ||= unite ||> TTask)
    where
    cs = map validate1 us
  Choose us -> sink g (Choose cs) (traverse outofBranch cs ||= intersect ||> TTask)
    where
    cs = map validate1 us
  Branch us -> sink g (Branch cs) (traverse (snd .> outofBranch) cs ||= intersect ||> TTask)
    where
    cs = map validate2 us
  Select us -> sink g (Select cs) (traverse (snd .> snd .> outofBranch) cs ||= intersect ||> TTask)
    where
    cs = map validate3 us
  Step m u1@(Unchecked i1) u2@(Unchecked i2) ->
    withTypeOf c1 (Step m c1 c2) \t1 -> case t1 of
      TTask r1 -> case match m (TRecord r1) of
        Left e2 -> fail g i e2
        Right d ->
          withTypeOf c2' (Step m c1 c2') \t2 -> case t2 of
            TTask r2 -> pass g' (Step m c1 c2') (TTask r2)
            _ -> bury (Step m c1 (fail g' i2 <| TaskNeeded t2))
          where
          c2' = validate s g' u2

          g' = g \/ d
      _ -> bury (Step m (fail g i1 <| TaskNeeded t1) c2)
    where
    c1 = validate s g u1

    c2 = validate s g u2 --NOTE: this is a first approximation, complete checking is in `c2'`
  ---- Builtins
  Execute n a ->
    lift g i (Execute n a) do
      t_n <- HashMap.lookup n g |> note (UnknownVariable n)
      case t_n of
        TFunction r' t -> do
          t_a <- check s g a
          t_a' <- expand s t_a
          r'' <- expand s r'
          if r'' == t_a' then
            done t
          else
            throw <| ArgumentError r'' t_a'
        _ -> throw <| FunctionNeeded t_n
  Hole _ -> fail g i (HoleFound g) --TODO: how to handle holes?
  Share e ->
    lift g i (Share e) do
      t <- check s g e
      b <- outofBasic t
      done <| wrapValue (TReference b)
  Assign e1 e2 ->
    lift g i (Assign e1 e2) do
      t1 <- check s g e1
      b1 <- outofReference t1
      b2 <- check s g e2
      if b1 == b2 then
        done <| TRecord HashMap.empty
      else
        throw <| AssignError b1 b2
  where
  validate1 :: Unchecked Task -> Checked Task
  validate1 u@(Unchecked i') = case validate s g u of --XXX crashes when eta expanded
    Pass g t j -> case t of
      TTask _ -> pass g j t
      _ -> fail g i' <| TaskNeeded t
    c -> c

  -- | Validate a branch consisting of a boolean expression and an unchecked task.
  --
  -- Note that errors in the expression are atachted to the task instead of the whole branch.
  -- This is due to the data type: errors can only be saved in a `Fail` constructor of a `Checked Task`,
  -- not in the expression.
  validate2 :: Expression * Unchecked Task -> Expression * Checked Task
  validate2 (e ~> u@(Unchecked i')) =
    e
      ~> case check s g e of
          Right (TPrimitive TBool) -> validate1 u
          Right t -> fail g i' <| BoolNeeded t
          Left x -> fail g i' x

  validate3 :: Label * Expression * Unchecked Task -> Label * Expression * Checked Task
  validate3 (l ~> e ~> u) = l ~> validate2 (e ~> u)

---- Helpers -------------------------------------------------------------------
outofBranch :: Checked Task -> Error + Row_ Type_
outofBranch = extract >=> outofTask

infixr 5 HashMap.union as \/
