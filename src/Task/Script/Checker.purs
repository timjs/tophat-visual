module Task.Script.Checker
  ( TypeError
  , class Check
  , check
  , match
  , execute
  ) where

import Preload hiding (note)
import Run (Run, extract)
import Run.Except (EXCEPT, note, runExcept, throw)
import Task.Script.Syntax (Argument(..), BasicTy, Constant(..), Expression(..), Label, Labels, Match(..), Name, PrimTy(..), Row, Statement(..), Task(..), Ty(..), isBasic, ofBasic, ofRecord, ofReference, ofTask, ofType)
import Data.HashMap as HashMap
import Data.HashSet as HashSet

---- Errors --------------------------------------------------------------------
data TypeError
  = UnknownVariable Name
  | UnknownLabel Label Ty
  | ArgumentError Ty Ty
  | VariantError Label Ty Ty
  | BranchError Ty Ty
  | BranchesError (Row Ty)
  | AssignError Ty Ty
  | ListError Ty Ty
  | FunctionNeeded Ty
  | BoolNeeded Ty
  | RecordNeeded Ty
  | VariantNeeded Ty
  | ListNeeded Ty
  | ReferenceNeeded Ty
  | TaskNeeded Ty
  | BasicNeeded Ty
  | UnknownLabels Labels Labels
  | DoubleLabels Labels Labels
  | EmptyCase
  | EmptyChoice
  | HoleFound Context
  | RecordMismatch (Row Match) Ty
  | UnpackMismatch Ty

instance showTypeError :: Show TypeError where
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
    UnknownLabels r_diff r_orig -> unwords [ "Labels", show r_diff |> quote, "are not part of row", show r_orig |> quote ]
    DoubleLabels r_double r_orig -> unwords [ "Double occurence of label", show r_double |> quote, "in row", show r_orig |> quote ]
    EmptyCase -> unwords [ "This case expression has no branches" ]
    EmptyChoice -> unwords [ "This choice task has no branches" ]
    HoleFound g -> unlines [ "Found hole of type _ in context", show g |> indent 2 ]
    RecordMismatch ms t -> unwords [ "Matching against", show ms, "needs", show t, "to be a record type" ]
    UnpackMismatch t -> unwords [ "Unpacking needs", show t, "to be a record type" ]

---- Checker -------------------------------------------------------------------
type Context
  = HashMap Name Ty

class Check a where
  check :: Context -> a -> Run ( except :: EXCEPT TypeError ) Ty

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
            for bs' \(t /\ m /\ e) -> do
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

instance checkStatement :: Check Statement where
  check g = case _ of
    Step m t s -> do
      t_t <- check g t
      case t_t of
        TTask r -> do
          d <- match m (TRecord r)
          check (g ++ d) s
        _ -> throw <| TaskNeeded t_t
    Task t -> check g t

instance checkTask :: Check Task where
  check g = case _ of
    Enter b _ -> ofBasic b |> returnValue
    Update _ e -> check g e |= needBasic |= returnValue
    Change _ e -> check g e |= outofReference |= returnValue
    View _ e -> check g e |= needBasic |= returnValue
    Watch _ e -> check g e |= outofReference |= returnValue
    Lift e -> check g e |= outofRecord ||> TTask
    Pair ss -> traverse subcheck ss |= unite ||> TTask
    Choose ss -> traverse subcheck ss |= intersect ||> TTask
    Branch bs -> traverse subcheck' bs |= intersect ||> TTask
    Select bs -> traverse subcheck'' bs |= intersect ||> TTask
    Execute x a -> do
      t_x <- HashMap.lookup x g |> note (UnknownVariable x)
      case t_x of
        TFunction r' t -> do
          t_a <- check g a
          if r' == t_a then
            done t
          else
            throw <| ArgumentError r' t_a
        _ -> throw <| FunctionNeeded t_x
    Hole _ -> throw <| HoleFound g --TODO: how to handle holes?
    Share e -> check g e |= outofBasic ||> TReference |= returnValue
    Assign e1 e2 -> do
      t1 <- check g e1
      b1 <- outofReference t1
      b2 <- check g e2
      if b1 == b2 then
        done (TRecord neutral)
      else
        throw (AssignError b1 b2)
    where
    subcheck s = check g s |= outofTask

    subcheck' (e /\ s) = do
      t_e <- check g e
      case t_e of
        TPrimitive TBool -> subcheck s
        _ -> throw <| BoolNeeded t_e

    subcheck'' (_ /\ e /\ s) = subcheck' (e /\ s)

unite :: forall t a. Foldable t => t (Row a) -> Run ( except :: EXCEPT TypeError ) (Row a)
unite = gather go neutral
  where
  go :: Row a -> Row a -> Run ( except :: EXCEPT TypeError ) (Row a)
  go acc r =
    if null is then
      done <| acc ++ r
    else
      throw <| DoubleLabels is (keys r)
    where
    is = keys r `HashSet.intersection` keys acc

intersect :: forall t a. Foldable t => t (Row a) -> Run ( except :: EXCEPT TypeError ) (Row a)
intersect rs = foldr1 (^^) rs |> note EmptyChoice

merge :: forall a b. Row a -> Row b -> Run ( except :: EXCEPT TypeError ) (Row (a /\ b))
merge r1 r2 =
  if HashMap.isEmpty ds then
    done <| HashMap.intersectionWith (/\) r1 r2
  else
    throw <| UnknownLabels (keys ds) (keys r2)
  where
  ds = r1 \\ r2

smash :: Row Ty -> Run ( except :: EXCEPT TypeError ) Ty
smash r = case HashMap.values r |> uncons of
  Nothing -> throw <| EmptyCase
  Just { head, tail } ->
    if all (_ == head) tail then
      done head
    else
      throw <| BranchesError r

needBasic :: Ty -> Run ( except :: EXCEPT TypeError ) Ty
needBasic t
  | isBasic t = done t
  | otherwise = throw <| BasicNeeded t

outofBasic :: Ty -> Run ( except :: EXCEPT TypeError ) BasicTy
outofBasic t
  | Just b <- ofType t = done b
  | otherwise = throw <| BasicNeeded t

outofRecord :: Ty -> Run ( except :: EXCEPT TypeError ) (Row Ty)
outofRecord t
  | Just r <- ofRecord t = done r
  | otherwise = throw <| RecordNeeded t

outofReference :: Ty -> Run ( except :: EXCEPT TypeError ) Ty
outofReference t
  | Just b <- ofReference t = done <| ofBasic b
  | otherwise = throw <| ReferenceNeeded t

outofTask :: Ty -> Run ( except :: EXCEPT TypeError ) (Row Ty)
outofTask t
  | Just r <- ofTask t = done r
  | otherwise = throw <| TaskNeeded t

returnValue :: forall m. Monad m => Ty -> m Ty
returnValue t = done <| TTask <| HashMap.fromFoldable [ "value" /\ t ]

---- Matcher -------------------------------------------------------------------
match :: Match -> Ty -> Run ( except :: EXCEPT TypeError ) Context
match m t = case m of
  MIgnore -> done neutral
  MBind x -> done <| HashMap.fromFoldable [ x /\ t ]
  MRecord ms -> do
    case t of
      TRecord r -> merge ms r |= traverse (uncurry match) |= unite
      _ -> throw <| RecordMismatch ms t
  MUnpack -> do
    case t of
      TRecord r -> done r
      _ -> throw <| UnpackMismatch t

---- Executing -----------------------------------------------------------------
execute :: forall a. Run ( except :: EXCEPT TypeError ) a -> Either TypeError a
execute = runExcept >> extract

---- Helpers -------------------------------------------------------------------
keys :: forall k v. Hashable k => HashMap k v -> HashSet k
keys = HashMap.keys >> HashSet.fromArray

infixl 6 HashMap.intersection as ^^

infixl 6 HashMap.difference as \\
