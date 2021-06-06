module Task.Script.Checker
  ( class Check
  , check
  , match
  -- # Helpers
  , wrapValue
  , needBasic
  , outofBasic
  , outofRecord
  , outofReference
  , outofTask
  -- # Rows
  , unite
  , intersect
  , merge
  , smash
  ) where

import Preload
import Data.Array as Array
import Data.HashMap as HashMap
import Data.HashSet as HashSet
import Task.Script.Context (Context, Typtext)
import Task.Script.Error (Error(..), Unchecked(..))
import Task.Script.Syntax (Arguments(..), BasicType, Constant(..), Expression(..), Match(..), PrimType(..), Row, Task(..), Type(..), isBasic, ofBasic, ofRecord, ofReference, ofTask, ofType)

---- Checker -------------------------------------------------------------------
class Check a where
  check :: Typtext -> Context -> a -> Error ++ Type

instance checkExpression :: Check Expression where
  check s g = case _ of
    ---- Basics
    Variable x -> HashMap.lookup x g |> note (UnknownVariable x)
    Lambda m t e -> do
      d <- match m t
      t' <- check s (g \/ d) e
      done <| TFunction t t'
    Apply e1 e2 -> do
      t1 <- check s g e1
      case t1 of
        TFunction t' t -> do
          t2 <- check s g e2
          if t' == t2 then
            done t
          else
            throw <| ArgumentError t' t2
        _ -> throw <| FunctionNeeded t1
    ---- Branches
    IfThenElse e1 e2 e3 -> do
      t1 <- check s g e1
      case t1 of
        TPrimitive TBool -> do
          t2 <- check s g e2
          t3 <- check s g e3
          if t2 == t3 then
            done t2
          else
            throw <| BranchError t2 t3
        _ -> throw <| BoolNeeded t1
    Case e0 bs -> do
      t0 <- check s g e0
      case t0 of
        TVariant r -> do
          bs' <- merge r bs --NOTE be aware of order: r is a subset of bs
          ts <-
            for bs' \(t ** m ** e) -> do
              d <- match m t
              check s (g \/ d) e
          smash ts
        _ -> throw <| VariantNeeded t0
    ---- Records & Variants
    Record es -> traverse (check s g) es ||> TRecord
    Variant l e t -> case t of
      TVariant r -> do
        t_e <- check s g e
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
      t1 <- check s g e1
      t2 <- check s g e2
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

instance checkArgument :: Check Arguments where
  check s g (ARecord es) = traverse (check s g) es ||> TRecord

instance checkUnchecked :: Check (Unchecked Task) where
  check s g (Unchecked i) = check s g i

instance checkTask :: Check t => Check (Task t) where
  check s g = case _ of
    Enter b _ -> HashMap.lookup b s |> note (UnknownTypeName b) ||> ofBasic ||> wrapValue
    Update _ e -> check s g e |= needBasic ||> wrapValue
    Change _ e -> check s g e |= outofReference ||> wrapValue
    View _ e -> check s g e |= needBasic ||> wrapValue
    Watch _ e -> check s g e |= outofReference ||> wrapValue
    Lift e -> check s g e |= outofRecord ||> TTask
    Pair ss -> traverse subcheck ss |= unite ||> TTask
    Choose ss -> traverse subcheck ss |= intersect ||> TTask
    Branch bs -> traverse subcheck' bs |= intersect ||> TTask
    Select bs -> traverse subcheck'' bs |= intersect ||> TTask
    Step m u1 u2 -> do
      t_u1 <- check s g u1
      case t_u1 of
        TTask r -> do
          d <- match m (TRecord r)
          check s (g \/ d) u2
        _ -> throw <| TaskNeeded t_u1
    Execute x a -> do
      t_x <- HashMap.lookup x g |> note (UnknownVariable x)
      case t_x of
        TFunction r' t -> do
          t_a <- check s g a
          if r' == t_a then
            done t
          else
            throw <| ArgumentError r' t_a
        _ -> throw <| FunctionNeeded t_x
    Hole _ -> throw <| HoleFound g --TODO: how to handle holes?
    Share e -> check s g e |= outofBasic ||> TReference ||> wrapValue
    Assign e1 e2 -> do
      t1 <- check s g e1
      b1 <- outofReference t1
      b2 <- check s g e2
      if b1 == b2 then
        done <| TRecord HashMap.empty
      else
        throw <| AssignError b1 b2
    where
    subcheck t = check s g t |= outofTask

    subcheck' (e ** t) = do
      t_e <- check s g e
      case t_e of
        TPrimitive TBool -> subcheck t
        _ -> throw <| BoolNeeded t_e

    subcheck'' (_ ** e ** t) = subcheck' (e ** t)

---- Matcher -------------------------------------------------------------------
match :: Match -> Type -> Error ++ Context
match m t = case m of
  MIgnore -> done HashMap.empty
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
unite = gather go HashMap.empty
  where
  go :: Row a -> Row a -> Error ++ Row a
  go acc r =
    if null is then
      done <| acc \/ r
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
smash r = case HashMap.values r |> Array.uncons of
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

wrapValue :: Type -> Type
wrapValue t = TTask <| from [ "value" ** t ]

---- General helpers
keys :: forall k v. Hashable k => HashMap k v -> HashSet k
keys = HashMap.keys >> from

infixl 6 HashMap.difference as \\

infixr 5 HashMap.union as \/
