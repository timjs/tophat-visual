module Task.Script.Validator
  ( validate
  ) where

import Preload
import Data.HashMap as HashMap
import Task.Script.Checker (check, match, needBasic, outofBasic, outofRecord, outofReference, outofTask, unite, intersect, wrapValue)
import Task.Script.Context (Context)
import Task.Script.Error (Checked(..), Error(..), Unchecked(..), bury, sink, fail, extract, lift, pass)
import Task.Script.Syntax (Expression, Label, PrimType(..), Row, Task(..), Type(..), ofBasic)

---- Validator -----------------------------------------------------------------
validate :: Context -> Unchecked Task -> Checked Task
validate g (Unchecked i) = case i of
  ---- Editors
  Enter b m -> pass (Enter b m) (ofBasic b |> wrapValue)
  Update m e -> lift i (Update m e) (check g e |= needBasic ||> wrapValue)
  Change m e -> lift i (Change m e) (check g e |= outofReference ||> wrapValue)
  View m e -> lift i (View m e) (check g e |= needBasic ||> wrapValue)
  Watch m e -> lift i (Watch m e) (check g e |= outofReference ||> wrapValue)
  Lift e -> lift i (Lift e) (check g e |= outofRecord ||> TTask)
  ---- Combinators
  Pair us -> sink (Pair cs) (traverse outofBranch cs |= unite ||> TTask)
    where
    cs = map validate1 us
  Choose us -> sink (Choose cs) (traverse outofBranch cs |= intersect ||> TTask)
    where
    cs = map validate1 us
  Branch us -> sink (Branch cs) (traverse (snd >> outofBranch) cs |= intersect ||> TTask)
    where
    cs = map validate2 us
  Select us -> sink (Select cs) (traverse (snd >> snd >> outofBranch) cs |= intersect ||> TTask)
    where
    cs = map validate3 us
  Step m u1@(Unchecked i1) u2@(Unchecked i2) -> case c1 of
    Pass t1 _ -> case t1 of
      TTask r1 -> case match m (TRecord r1) of
        Left e2 -> fail i e2
        Right d -> case c2' of
          Pass t2 _ -> case t2 of
            TTask r2 -> pass (Step m c1 c2') (TTask r2)
            _ -> bury (Step m c1 (fail i2 <| TaskNeeded t2))
          _ -> bury (Step m c1 c2')
          where
          c2' = validate (g ++ d) u2
      _ -> bury (Step m (fail i1 <| TaskNeeded t1) c2)
    _ -> bury (Step m c1 c2)
    where
    c1 = validate g u1

    c2 = validate g u2 --NOTE: this is a first approximation, complete checking is in `c2'`
  ---- Builtins
  Execute n a ->
    lift i (Execute n a) do
      t_n <- HashMap.lookup n g |> note (UnknownVariable n)
      case t_n of
        TFunction r' t -> do
          t_a <- check g a
          if r' == t_a then
            done t
          else
            throw <| ArgumentError r' t_a
        _ -> throw <| FunctionNeeded t_n
  Hole _ -> fail i (HoleFound g) --TODO: how to handle holes?
  Share e ->
    lift i (Share e) do
      t <- check g e
      b <- outofBasic t
      done <| wrapValue (TReference b)
  Assign e1 e2 ->
    lift i (Assign e1 e2) do
      t1 <- check g e1
      b1 <- outofReference t1
      b2 <- check g e2
      if b1 == b2 then
        done <| TRecord neutral
      else
        throw <| AssignError b1 b2
  where
  validate1 :: Unchecked Task -> Checked Task
  validate1 u@(Unchecked i') = case validate g u of --XXX crashes when eta expanded
    Pass t j -> case t of
      TTask _ -> pass j t
      _ -> fail i' <| TaskNeeded t
    c -> c

  -- | Validate a branch consisting of a boolean expression and an unchecked task.
  --
  -- Note that errors in the expression are atachted to the task instead of the whole branch.
  -- This is due to the data type: errors can only be saved in a `Fail` constructor of a `Checked Task`,
  -- not in the expression.
  validate2 :: Expression ** Unchecked Task -> Expression ** Checked Task
  validate2 (e ** u@(Unchecked i')) =
    e
      ** case check g e of
          Right (TPrimitive TBool) -> validate1 u
          Right t -> fail i' <| BoolNeeded t
          Left x -> fail i' x

  validate3 :: Label ** Expression ** Unchecked Task -> Label ** Expression ** Checked Task
  validate3 (l ** e ** u) = l ** validate2 (e ** u)

---- Helpers -------------------------------------------------------------------
outofBranch :: Checked Task -> Error ++ Row Type
outofBranch = extract >=> outofTask
