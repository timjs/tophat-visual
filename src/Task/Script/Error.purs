module Task.Script.Error
  -- # Checked or Unchecked
  ( Unchecked(..)
  , Checked(..)
  , lift
  , sink
  , pass
  , fail
  , bury
  , extract
  , annotate
  -- # Errors
  , Error(..)
  ) where

import Preload
import Data.Doc (class Display, display)
import Data.Doc as Doc
import Task.Script.Syntax (Label, Labels, Match, Name, Row, Task, Type, showLabels)
import Task.Script.Context (Context)

---- Checked or Unchecked ------------------------------------------------------
data Unchecked f
  = Unchecked (f (Unchecked f))

--NOTE: below instance is infinite, we create one for Tasks only
-- instance showUnchecked :: Show (f (Unchecked f)) => Show (Unchecked f) where
instance displayUnchecked :: Display (Unchecked Task) where
  display (Unchecked x) = display x

instance showUnchecked :: Show (Unchecked Task) where
  show = display >> Doc.render

data Checked f
  = Fail Error (f (Unchecked f))
  | Bury (f (Checked f))
  | Pass Type (f (Checked f))

instance displayChecked :: Display (Checked Task) where
  display = case _ of
    Fail e u -> Doc.words [ Doc.text "(!", display u, Doc.text ":", Doc.show e, Doc.text "!)" ]
    Bury c -> Doc.words [ Doc.text "(?", display c, Doc.text "?)" ]
    Pass a c -> Doc.words [ Doc.text "(", display c, Doc.text ":", Doc.show a, Doc.text ")" ]

lift :: forall f. f (Unchecked f) -> f (Checked f) -> Error ++ Type -> Checked f
lift u c = case _ of
  Left e -> Fail e u
  Right a -> Pass a c

sink :: forall f. f (Checked f) -> Error ++ Type -> Checked f
sink c = case _ of
  Left e -> Bury c
  Right a -> Pass a c

fail :: forall f. f (Unchecked f) -> Error -> Checked f
fail u e = Fail e u

pass :: forall f. f (Checked f) -> Type -> Checked f
pass c a = Pass a c

bury :: forall f. f (Checked f) -> Checked f
bury = Bury

extract :: Checked Task -> Error ++ Type
extract = case _ of
  Pass t _ -> Right t
  _ -> Left UndeterminedType

annotate :: Unchecked Task -> (Task (Unchecked Task) -> Error ++ (Task (Checked Task) ** Type)) -> Checked Task
annotate (Unchecked u) f = case f u of
  Left x -> Fail x u
  Right (c ** t) -> Pass t c

-- replace :: Error -> Checked Task -> Checked Task
-- replace x = case _ of
--   Fail _ c -> Fail x c
--   Pass _ c -> Pass x c
---- Errors --------------------------------------------------------------------
data Error
  = UnknownVariable Name
  | UnknownTypeName Name
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
  | UndeterminedType
  | EmptyCase
  | EmptyChoice
  | HoleFound Context
  | RecordMismatch (Row Match) Type
  | UnpackMismatch Type

instance showError :: Show Error where
  show = case _ of
    UnknownVariable x -> unwords [ "Unknown variable", x |> quote ]
    UnknownTypeName n -> unwords [ "Unknown type name", n |> quote ]
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
    DoubleLabels r_double r_orig -> unwords [ "Double occurence of labels", showLabels r_double, "in row", showLabels r_orig ]
    UndeterminedType -> "The type of this part could not be determined due to a type check failure"
    EmptyCase -> unwords [ "This case expression has no branches" ]
    EmptyChoice -> unwords [ "This choice task has no branches" ]
    HoleFound g -> unlines [ "Found hole of type _ in context", show g |> indent 2 ]
    RecordMismatch ms t -> unwords [ "Matching against", show ms, "needs", show t, "to be a record type" ]
    UnpackMismatch t -> unwords [ "Unpacking needs", show t, "to be a record type" ]
