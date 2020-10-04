module Task.Script.Checker
  ( Unchecked(..)
  , Checked(..)
  , Error
  , class Check
  , check
  , validate
  , match
  ) where

import Preload
import Data.HashMap as HashMap
import Data.HashSet as HashSet
import Task.Script.Syntax (Argument(..), BasicType, Constant(..), Expression(..), Label, Labels, Match(..), Name, PrimType(..), Row, Task(..), Type(..), isBasic, ofBasic, ofRecord, ofReference, ofTask, ofType, showLabels)

---- Errors --------------------------------------------------------------------
data Unchecked f
  = Unchecked (f (Unchecked f))

--NOTE: below instance is infinite, we create one for Tasks only
-- instance showUnchecked :: Show (f (Unchecked f)) => Show (Unchecked f) where
instance showUnchecked :: Show (Unchecked Task) where
  show (Unchecked x) = show x

data Checked f
  = Fail Error (f (Unchecked f))
  | Pass Type (f (Checked f))

instance showChecked :: Show (Checked Task) where
  show = case _ of
    Fail e x -> unwords [ "(!", show e, "==>", show x, "!)" ]
    Pass _ x -> unwords [ "(o)", show x ]

extract :: Checked Task -> Error ++ Type
extract = case _ of
  Fail e _ -> Left e
  Pass t _ -> Right t

annotate :: Unchecked Task -> (Task (Unchecked Task) -> Error ++ (Task (Checked Task) ** Type)) -> Checked Task
annotate (Unchecked u) f = case f u of
  Left x -> Fail x u
  Right (c ** t) -> Pass t c

-- replace :: Error -> Checked Task -> Checked Task
-- replace x = case _ of
--   Fail _ c -> Fail x c
--   Pass _ c -> Pass x c
data Error
  = UnknownVariable Name
  | UnknownLabel Label Type
  | ArgumentError Type Type
  | VariantError Label Type Type
  | BranchError Type Type
  | BranchesError (Row Type)
  | AssignError Type Type
  | ListError Type Type
  | FunctionNeeded Type
  | BoolNeeded Type
  | RecordNeeded Type
  | VariantNeeded Type
  | ListNeeded Type
  | ReferenceNeeded Type
  | TaskNeeded Type
  | BasicNeeded Type
  | UnknownLabels Labels Labels
  | DoubleLabels Labels Labels
  | EmptyCase
  | EmptyChoice
  | HoleFound Context
  | RecordMismatch (Row Match) Type
  | UnpackMismatch Type

instance showError :: Show Error where
  show = case _ of
    UnknownVariable x -> unwords [ "Unknown variable", x |> quote ]
    UnknownLabel l t -> unwords [ "Unknown label", l |> quote, "in variant type", show t ]
    -- VariableMismatch x t_exp t_act -> unwords ["Variable", quote x, "has type", show t_act, ", but it is expected to be a(n)", show t_exp]
    -- ConstantMismatch c t_exp t_act -> unwords ["Constant", quote (show c), "has type", show t_act, "but it is expected to be a(n)", show t_exp]
    ArgumentError t_exp t_act -> unlines [ "This function needs it argument to be of type", show t_exp |> quote |> indent 2, ", but it is of type", show t_act |> quote |> indent 2 ]
    VariantError l t_exp t_act -> unwords [ "This variant with label", show l |> quote, "needs it argument to be of type", show t_exp |> quote, ", but it is of type", show t_act |> quote ]
    BranchError t_then t_else -> unlines [ "This conditional's then-branch has type", show t_then |> quote |> indent 2, ", while the else-branch has type", show t_else |> quote |> indent 2 ]
    BranchesError ts -> unwords [ "This case expression has branches of different types", show ts |> quote ]
    AssignError t_ref t_val -> unlines [ "This assignment tries to store something of type", show t_val |> quote |> indent 2, "into a reference of type", show t_ref |> quote |> indent 2 ]
    ListError t_head t_tail -> unlines [ "This element has type", show t_head |> quote |> indent 2, "but the list if of type", show t_tail |> quote |> indent 2 ]
    FunctionNeeded t_bad -> unwords [ "Cannot use", show t_bad |> quote, "as a function" ]
    -- BindNeeded t_bad -> unwords ["Cannot use", show t_bad |> quote, "as a a function from row to task"]
    RecordNeeded t_bad -> unwords [ "Cannot use", show t_bad |> quote, "as a record" ]
    VariantNeeded t_bad -> unwords [ "Cannot use", show t_bad |> quote, "as a variant" ]
    ListNeeded t_bad -> unwords [ "Cannot use", show t_bad |> quote, "as a list" ]
    BoolNeeded t_bad -> unwords [ "Cannot use", show t_bad |> quote, "as a boolean" ]
    ReferenceNeeded t_bad -> unwords [ "Cannot use", show t_bad |> quote, "as a reference" ]
    TaskNeeded t_bad -> unwords [ "Cannot use", show t_bad |> quote, "as a task" ]
    BasicNeeded t_bad -> unwords [ "Cannot use", show t_bad |> quote, "as a basic type" ]
    UnknownLabels r_diff r_orig -> unwords [ "Labels", showLabels r_diff, "are not part of row", showLabels r_orig ]
    DoubleLabels r_double r_orig -> unwords [ "Double occurence of label", showLabels r_double, "in row", showLabels r_orig ]
    EmptyCase -> unwords [ "This case expression has no branches" ]
    EmptyChoice -> unwords [ "This choice task has no branches" ]
    HoleFound g -> unlines [ "Found hole of type _ in context", show g |> indent 2 ]
    RecordMismatch ms t -> unwords [ "Matching against", show ms, "needs", show t, "to be a record type" ]
    UnpackMismatch t -> unwords [ "Unpacking needs", show t, "to be a record type" ]

---- Checker -------------------------------------------------------------------
type Context
  = HashMap Name Type

class Check a where
  check :: Context -> a -> Error ++ Type

instance checkExpression :: Check Expression where
  check g = case _ of
    ---- Basics
    Variable x -> HashMap.lookup x g |> note (UnknownVariable x)
    Lambda m t e -> do
      d <- match m t
      t' <- check (g ++ d) e
      done <| TFunction t t'
    Apply e1 e2 -> do
      t1 <- check g e1
      case t1 of
        TFunction t' t -> do
          t2 <- check g e2
          if t' == t2 then
            done t
          else
            throw <| ArgumentError t' t2
        _ -> throw <| FunctionNeeded t1
    ---- Branches
    IfThenElse e1 e2 e3 -> do
      t1 <- check g e1
      case t1 of
        TPrimitive TBool -> do
          t2 <- check g e2
          t3 <- check g e3
          if t2 == t3 then
            done t2
          else
            throw <| BranchError t2 t3
        _ -> throw <| BoolNeeded t1
    Case e0 bs -> do
      t0 <- check g e0
      case t0 of
        TVariant r -> do
          bs' <- merge r bs --NOTE be aware of order: r is a subset of bs
          ts <-
            for bs' \(t ** m ** e) -> do
              d <- match m t
              check (g ++ d) e
          smash ts
        _ -> throw <| VariantNeeded t0
    ---- Records & Variants
    Record es -> traverse (check g) es ||> TRecord
    Variant l e t -> case t of
      TVariant r -> do
        t_e <- check g e
        case HashMap.lookup l r of
          Just t' ->
            if t_e == t' then
              done t
            else
              throw <| VariantError l t' t_e
          Nothing -> throw <| UnknownLabel l t
      _ -> throw <| VariantNeeded t
    ---- Lists
    Nil t -> done <| TList t
    Cons e1 e2 -> do
      t1 <- check g e1
      t2 <- check g e2
      case t2 of
        TList t' ->
          if t2 == t' then
            done t2
          else
            throw <| ListError t1 t2
        _ -> throw <| ListNeeded t2
    ---- Constants
    Constant (B _) -> done <| TPrimitive TBool
    Constant (I _) -> done <| TPrimitive TInt
    Constant (S _) -> done <| TPrimitive TString

instance checkArgument :: Check Argument where
  check g (ARecord es) = traverse (check g) es ||> TRecord

---- Validator -----------------------------------------------------------------
validate :: Context -> Unchecked Task -> Checked Task
validate g = flip annotate go
  where
  go :: Task (Unchecked Task) -> Error ++ (Task (Checked Task) ** Type)
  go = case _ of
    Enter b m -> done (ofBasic b) ||> wrapValue ||> (**) (Enter b m)
    Update m e -> check g e |= needBasic ||> wrapValue ||> (**) (Update m e)
    Change m e -> check g e |= outofReference ||> wrapValue ||> (**) (Change m e)
    View m e -> check g e |= needBasic ||> wrapValue ||> (**) (View m e)
    Watch m e -> check g e |= outofReference ||> wrapValue ||> (**) (Watch m e)
    Lift e -> check g e |= outofRecord ||> TTask ||> (**) (Lift e)
    Pair bs -> traverse outofBranch bs' |= unite ||> TTask ||> (**) (Pair bs')
      where
      bs' = map validate1 bs
    Choose bs -> traverse outofBranch bs' |= intersect ||> TTask ||> (**) (Choose bs')
      where
      bs' = map validate1 bs
    Branch bs -> traverse (snd >> outofBranch) bs' |= intersect ||> TTask ||> (**) (Branch bs')
      where
      bs' = map validate2 bs
    Select bs -> traverse (snd >> snd >> outofBranch) bs' |= intersect ||> TTask ||> (**) (Select bs')
      where
      bs' = map validate3 bs
    Step m u1 u2 -> do
      let
        u1' = validate g u1
      t_u1 <- extract u1'
      case t_u1 of
        TTask r1 -> do
          d <- match m (TRecord r1)
          let
            u2' = validate (g ++ d) u2
          t_u2 <- extract u2'
          case t_u2 of
            TTask r2 -> done <| Step m u1' u2' ** TTask r2
            _ -> throw <| TaskNeeded t_u2
        _ -> throw <| TaskNeeded t_u1
    Execute n a -> do
      t_n <- HashMap.lookup n g |> note (UnknownVariable n)
      case t_n of
        TFunction r' t -> do
          t_a <- check g a
          if r' == t_a then
            done (Execute n a ** t)
          else
            throw <| ArgumentError r' t_a
        _ -> throw <| FunctionNeeded t_n
    Hole _ -> throw <| HoleFound g --TODO: how to handle holes?
    Share e -> check g e |= outofBasic ||> TReference ||> wrapValue ||> (**) (Share e)
    Assign e1 e2 -> do
      t1 <- check g e1
      b1 <- outofReference t1
      b2 <- check g e2
      if b1 == b2 then
        done <| Assign e1 e2 ** TRecord neutral
      else
        throw <| AssignError b1 b2

  validate1 :: Unchecked Task -> Checked Task
  validate1 u = validate g u --XXX crashes when eta expanded

  validate2 :: Expression ** Unchecked Task -> Expression ** Checked Task
  validate2 (e ** u) =
    e
      ** annotate u \i -> do
          t_e <- check g e
          case t_e of
            TPrimitive TBool -> go i
            _ -> throw <| BoolNeeded t_e

  validate3 :: Label ** Expression ** Unchecked Task -> Label ** Expression ** Checked Task
  validate3 (l ** e ** u) = l ** validate2 (e ** u)

---- Matcher -------------------------------------------------------------------
match :: Match -> Type -> Error ++ Context
match m t = case m of
  MIgnore -> done neutral
  MBind x -> done <| from [ x ** t ]
  MRecord ms -> do
    case t of
      TRecord r -> merge ms r |= traverse (uncurry match) |= unite
      _ -> throw <| RecordMismatch ms t
  MUnpack -> do
    case t of
      TRecord r -> done r
      _ -> throw <| UnpackMismatch t

---- Helpers -------------------------------------------------------------------
---- Row helpers
-- | Unite multiple rows into one.
--
-- Throws an error on double lables.
unite :: forall t a. Foldable t => t (Row a) -> Error ++ Row a
unite = gather go neutral
  where
  go :: Row a -> Row a -> Error ++ Row a
  go acc r =
    if null is then
      done <| acc ++ r
    else
      throw <| DoubleLabels is (keys r)
    where
    is = keys r `HashSet.intersection` keys acc

-- | Intersect multiple rows into one.
--
-- Throws if the intersection is empty.
intersect :: forall t a. Foldable t => t (Row a) -> Error ++ Row a
intersect rs = foldr1 HashMap.intersection rs |> note EmptyChoice

-- | Merge the values of a second row into the first row with the same labels.
--
-- Throws if `r1` has labels not in `r2`.
merge :: forall a b. Row a -> Row b -> Error ++ Row (a ** b)
merge r1 r2 =
  if HashMap.isEmpty ds then
    done <| HashMap.intersectionWith (**) r1 r2
  else
    throw <| UnknownLabels (keys ds) (keys r2)
  where
  ds = r1 \\ r2

-- | Smash a row of types together into one type.
--
-- Throws if the row is empty or if labels have different types.
smash :: Row Type -> Error ++ Type
smash r = case HashMap.values r |> uncons of
  Nothing -> throw <| EmptyCase
  Just { head, tail } ->
    if all (_ == head) tail then
      done head
    else
      throw <| BranchesError r

---- Type helpers
needBasic :: Type -> Error ++ Type
needBasic t
  | isBasic t = done t
  | otherwise = throw <| BasicNeeded t

outofBasic :: Type -> Error ++ BasicType
outofBasic t
  | Just b <- ofType t = done b
  | otherwise = throw <| BasicNeeded t

outofRecord :: Type -> Error ++ Row Type
outofRecord t
  | Just r <- ofRecord t = done r
  | otherwise = throw <| RecordNeeded t

outofReference :: Type -> Error ++ Type
outofReference t
  | Just b <- ofReference t = done <| ofBasic b
  | otherwise = throw <| ReferenceNeeded t

outofTask :: Type -> Error ++ Row Type
outofTask t
  | Just r <- ofTask t = done r
  | otherwise = throw <| TaskNeeded t

outofBranch :: Checked Task -> Error ++ Row Type
outofBranch = extract >> map outofTask >> join

wrapValue :: Type -> Type
wrapValue t = TTask <| from [ "value" ** t ]

---- General helpers
keys :: forall k v. Hashable k => HashMap k v -> HashSet k
keys = HashMap.keys >> from

infixl 6 HashMap.difference as \\
