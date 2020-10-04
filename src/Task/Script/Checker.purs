module Task.Script.Checker
  ( Error
  , class Check
  , check
  , match
  ) where

import Preload
import Data.HashMap as HashMap
import Data.HashSet as HashSet
-- import Run (Run, extract)
-- import Run.Except (EXCEPT, note, runExcept, throw)
import Task.Script.Syntax (Argument(..), BasicType, Constant(..), Expression(..), Label, Labels, Match(..), Name, PrimType(..), Row, Task(..), Type(..), isBasic, ofBasic, ofRecord, ofReference, ofTask, ofType, showLabels)

---- Errors --------------------------------------------------------------------
data Unchecked f
  = Unchecked (f (Unchecked f))

--NOTE: below instance is infinite, we create one for Tasks only
-- instance showUnchecked :: Show (f (Unchecked f)) => Show (Unchecked f) where
instance showUnchecked :: Show (Unchecked Task) where
  show (Unchecked x) = show x

data Checked f
  = Fail Error (f (Checked f))
  | Pass Type (f (Checked f))

instance showChecked :: Show (Checked Task) where
  show = case _ of
    Fail e x -> unwords [ "(!", show e, "!)", show x ]
    Pass _ x -> unwords [ "(o)", show x ]

extract :: Checked Task -> Error ++ Type
extract = case _ of
  Fail e _ -> Left e
  Pass t _ -> Right t

annotate :: Task (Checked Task) -> Error ++ Type -> Checked Task
annotate c = case _ of
  Left e -> Fail e c
  Right t -> Pass t c

replace :: Error -> Checked Task -> Checked Task
replace e = case _ of
  Fail _ c -> Fail e c
  Pass _ c -> Fail e c

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

-- class Check' f where
--   check' :: Context -> f (Unchecked Task) -> f (Checked Task)
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

-- checkStatement :: Context -> Statement (Unchecked Task) -> Statement (Checked Task)
-- checkStatement g x = case x of
--   Step' m t s -> ?s1
--   Task' t -> Task' ?s2
check' :: Context -> Unchecked Task -> Checked Task
check' g (Unchecked t) = case t of
  Enter b m -> done (ofBasic b |> returnValue) |> annotate (Enter b m)
  Update m e -> check g e |= needBasic ||> returnValue |> annotate (Update m e)
  Change m e -> check g e |= outofReference ||> returnValue |> annotate (Change m e)
  View m e -> check g e |= needBasic ||> returnValue |> annotate (View m e)
  Watch m e -> check g e |= outofReference ||> returnValue |> annotate (Watch m e)
  Lift e -> check g e |= outofRecord ||> TTask |> annotate (Lift e)
  Pair bs -> traverse outofBranch bs' |= unite ||> TTask |> annotate (Pair bs')
    where
    bs' = map subcheck1 bs
  Choose bs -> traverse outofBranch bs' |= intersect ||> TTask |> annotate (Choose bs')
    where
    bs' = map subcheck1 bs
  Branch bs -> traverse (snd >> outofBranch) bs' |= intersect ||> TTask |> annotate (Branch bs')
    where
    bs' = map subcheck2 bs
  Select bs -> traverse (snd >> snd >> outofBranch) bs' |= intersect ||> TTask |> annotate (Select bs')
    where
    bs' = map subcheck3 bs
  _ -> undefined
  -- Step m t u -> do
  --   t_t <- check g t
  --   case t_t of
  --     TTask r -> do
  --       d <- match m (TRecord r)
  --       check (g ++ d) u
  --     _ -> throw <| TaskNeeded t_t
  -- Execute x a -> do
  --   t_x <- HashMap.lookup x g |> note (UnknownVariable x)
  --   case t_x of
  --     TFunction r' t -> do
  --       t_a <- check g a
  --       if r' == t_a then
  --         done t
  --       else
  --         throw <| ArgumentError r' t_a
  --     _ -> throw <| FunctionNeeded t_x
  -- Hole _ -> throw <| HoleFound g --TODO: how to handle holes?
  -- Share e -> check g e |= outofBasic ||> TReference |= returnValue
  -- Assign e1 e2 -> do
  --   t1 <- check g e1
  --   b1 <- outofReference t1
  --   b2 <- check g e2
  --   if b1 == b2 then
  --     done (TRecord neutral)
  --   else
  --     throw (AssignError b1 b2)
  where
  subcheck1 :: Unchecked Task -> Checked Task
  subcheck1 = check' g

  subcheck2 :: Expression ** Unchecked Task -> Expression ** Checked Task
  subcheck2 (e ** u) = case check g e of
    Right (TPrimitive TBool) -> e ** u'
    Right t_e -> e ** replace (BoolNeeded t_e) u'
    Left x -> e ** replace x u'
    where
    u' = subcheck1 u

  subcheck3 :: Label ** Expression ** Unchecked Task -> Label ** Expression ** Checked Task
  subcheck3 (l ** e ** u) = l ** subcheck2 (e ** u)

-- check' g (Unchecked x) = case check g x |> execute of
--   Left e -> Fail e ?t1
--   Right t -> Pass t ?t2
---- Rows ----------------------------------------------------------------------
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

---- Types
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
outofBranch = extract >> map outofRecord >> join

returnValue :: Type -> Type
returnValue t = TTask <| from [ "value" ** t ]

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

---- Executing -----------------------------------------------------------------
-- execute :: forall a.  Error ++a ->  Error ++a
-- execute = runExcept >> extract
---- Helpers -------------------------------------------------------------------
keys :: forall k v. Hashable k => HashMap k v -> HashSet k
keys = HashMap.keys >> from

infixl 6 HashMap.difference as \\
