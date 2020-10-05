module Task.Script.Validator
  ( validate
  ) where

import Preload
import Data.HashMap as HashMap
import Task.Script.Syntax (Task(..), Type(..), ofBasic)
import Task.Script.Error (Checked(..), Context, Error(..), Unchecked(..), bury, fail, lift, pass)
import Task.Script.Checker (check, match, needBasic, outofBasic, outofRecord, outofReference, wrapValue)

---- Validator -----------------------------------------------------------------
validate :: Context -> Unchecked Task -> Checked Task
validate g (Unchecked u) = case u of
  ---- Editors
  Enter b m -> pass (Enter b m) (ofBasic b |> wrapValue)
  Update m e -> lift u (Update m e) (check g e |= needBasic ||> wrapValue)
  Change m e -> lift u (Change m e) (check g e |= outofReference ||> wrapValue)
  View m e -> lift u (View m e) (check g e |= needBasic ||> wrapValue)
  Watch m e -> lift u (Watch m e) (check g e |= outofReference ||> wrapValue)
  Lift e -> lift u (Lift e) (check g e |= outofRecord ||> TTask)
  ---- Combinators
  -- Pair bs -> traverse outofBranch bs' |= unite ||> TTask ||> (**) (Pair bs')
  --   where
  --   bs' = map validate1 bs
  -- Choose bs -> traverse outofBranch bs' |= intersect ||> TTask ||> (**) (Choose bs')
  --   where
  --   bs' = map validate1 bs
  -- Branch bs -> traverse (snd >> outofBranch) bs' |= intersect ||> TTask ||> (**) (Branch bs')
  --   where
  --   bs' = map validate2 bs
  -- Select bs -> traverse (snd >> snd >> outofBranch) bs' |= intersect ||> TTask ||> (**) (Select bs')
  --   where
  --   bs' = map validate3 bs
  Step m u1@(Unchecked i1) u2@(Unchecked i2) -> case c1 of
    Pass t1 _ -> case t1 of
      TTask r1 -> case match m (TRecord r1) of
        Left e2 -> fail u e2
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
    lift u (Execute n a) do
      t_n <- HashMap.lookup n g |> note (UnknownVariable n)
      case t_n of
        TFunction r' t -> do
          t_a <- check g a
          if r' == t_a then
            done t
          else
            throw <| ArgumentError r' t_a
        _ -> throw <| FunctionNeeded t_n
  Hole _ -> fail u (HoleFound g) --TODO: how to handle holes?
  Share e ->
    lift u (Share e) do
      t <- check g e
      b <- outofBasic t
      done <| wrapValue (TReference b)
  Assign e1 e2 ->
    lift u (Assign e1 e2) do
      t1 <- check g e1
      b1 <- outofReference t1
      b2 <- check g e2
      if b1 == b2 then
        done <| TRecord neutral
      else
        throw <| AssignError b1 b2
  _ -> undefined
  where
  validate1 :: Unchecked Task -> Checked Task
  validate1 u' = validate g u' --XXX crashes when eta expanded

-- validate2 :: Expression ** Unchecked Task -> Expression ** Checked Task
-- validate2 (e ** u') =
--   e
--     ** annotate u' \i -> do
--         t_e <- check g e
--         case t_e of
--           TPrimitive TBool -> validate g u'
--           _ -> throw <| BoolNeeded t_e
-- validate3 :: Label ** Expression ** Unchecked Task -> Label ** Expression ** Checked Task
-- validate3 (l ** e ** u') = l ** validate2 (e ** u')
---- Helpers -------------------------------------------------------------------
-- outofBranch :: Checked Task -> Error ++ Row Type
-- outofBranch = extract >> map outofTask >> join
---- General helpers
keys :: forall k v. Hashable k => HashMap k v -> HashSet k
keys = HashMap.keys >> from

infixl 6 HashMap.difference as \\
