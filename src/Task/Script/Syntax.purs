module Task.Script.Syntax
  -- # Expressions
  ( Expression(..)
  , Arguments(..)
  , Constant(..)
  -- # Matches
  , Match(..)
  -- # Statements
  , Branches
  , LabeledBranches
  , Task(..)
  ) where

import Preload

import Data.Doc as Doc
import Data.Doc (class Display, display)
import Data.HashMap as HashMap

import Task.Script.Context (isOperator, unreadable)
import Task.Script.Label (Label, Labeled, Name, showFields)
import Task.Script.Type (FullType)

---- Expressions ---------------------------------------------------------------

data Expression
  = Lambda Match FullType Expression
  | Apply Expression Expression
  | Variable Name
  | IfThenElse Expression Expression Expression
  | Case Expression (Labeled (Match * Expression))
  | Record (Labeled Expression)
  | Wildcard
  | Variant Label Expression FullType
  | Nil FullType
  | Cons Expression Expression
  | Constant Constant

derive instance Eq Expression

instance Show Expression where
  show = case _ of
    Lambda m t e -> unwords [ show m, ":", show t, ".", show e ]
    Apply (Apply (Variable n1) e2) e3 -> -- special case for operators
      if isOperator n1 then unwords [ show e2, n1, show e3 ]
      else unwords [ unreadable n1, show e2, show e3 ]
    Apply e1 e2 -> unwords [ "(", show e1, show e2, ")" ] --FIXME: parens needed?
    Variable n -> unreadable n
    IfThenElse e1 e2 e3 ->
      unlines
        [ unwords [ "if", show e1 ]
        , unwords [ "then", show e2 ] |> indent 2
        , unwords [ "else", show e3 ] |> indent 2
        ]
    Case e0 ms ->
      unlines
        [ unwords [ "case", show e0, "of" ]
        , unlines (HashMap.toArrayBy (\m e -> unwords [ show m, "|->", show e ] |> indent 2) ms)
        ]
    Record es -> showFields "=" es
    Wildcard -> "{..}"
    Variant l e t -> unwords [ l, show e, "as", show t ]
    Nil t -> unwords [ "[]", "as", show t ]
    Cons e1 e2 -> unwords [ show e1, "::", show e2 ]
    Constant c -> show c

data Arguments
  = ARecord (Labeled Expression)

derive instance Eq Arguments

instance Show Arguments where
  show (ARecord es) = showFields "=" es

data Constant
  = B Bool
  | I Int
  | S String

derive instance Eq Constant

instance Show Constant where
  show = case _ of
    B true -> "True"
    B false -> "False"
    I i -> show i
    S s -> show s

---- Matches -------------------------------------------------------------------

data Match
  = MIgnore
  | MBind Name
  | MRecord (Labeled Match)
  | MUnpack

derive instance Eq Match

instance Show Match where
  show = case _ of
    MIgnore -> "_"
    MBind x -> x
    MRecord ms -> showFields "≈" ms
    MUnpack -> "{..}"

---- Statements ----------------------------------------------------------------

-- data Statement t
--   = Step' Match t (Statement t)
--   | Task' t
-- derive instance Eq t => Eq (Statement t)
-- instance Show t => Show (Statement t) where
--   show = case _ of
--     Step' m t s -> unlines [ unwords [ show m, "<-", show t ], show s ]
--     Task' t -> show t

type Branches t = Array (Expression * t)

type LabeledBranches t = Array (Label * Expression * t)

-- NOTE:
-- Be aware of the INVARIANT: Branch and Select need to be inside a Step.
-- The official gramar reflects this, but the grammar defined here is flatened,
-- to parametrise it as a functor and tie the knot using custom annotations.
-- (See Task.Script.Annotation.)
data Task t
  -- Steps
  = Step Match t t
  | Branch (Branches t)
  | Select (LabeledBranches t)
  -- Editors
  | Enter Name
  | Update Expression
  | Change Expression
  | View Expression
  | Watch Expression
  -- Combinators
  | Lift Expression
  | Pair (Array t)
  | Choose (Array t)
  -- Extras
  | Execute Name Arguments
  | Hole Arguments
  -- Shares
  | Share Expression
  | Assign Expression Expression

derive instance Eq t => Eq (Task t)

derive instance Functor Task

instance Display t => Show (Task t) where
  show = display >> Doc.render

instance Display t => Display (Task t) where
  display = case _ of
    Step m t s -> Doc.lines [ Doc.words [ Doc.show m, Doc.text "<-", display t ], display s ]
    Branch [ Constant (B true) ~ t ] -> display t --Doc.lines [ Doc.text "branch", inner' bs ]
    Branch bs -> Doc.lines [ Doc.text "branch", inner' bs ]
    Select bs -> Doc.lines [ Doc.text "select", inner'' bs ]
    Enter t -> Doc.words [ Doc.text "enter", Doc.text t ]
    Update e -> Doc.words [ Doc.text "update", Doc.show e ]
    Change e -> Doc.words [ Doc.text "change", Doc.show e ]
    View e -> Doc.words [ Doc.text "view", Doc.show e ]
    Watch e -> Doc.words [ Doc.text "watch", Doc.show e ]
    Lift e -> Doc.words [ Doc.text "done", Doc.show e ]
    Pair ss -> inner "and" ss --Doc.lines [ Doc.text "pair", inner ss ]
    Choose ss -> inner "or" ss --Doc.lines [ Doc.text "choose", inner ss ]
    Execute n as -> Doc.words [ Doc.text (unreadable n), Doc.show as ]
    Hole as -> Doc.words [ Doc.text "?", Doc.show as ]
    Share e -> Doc.words [ Doc.text "share", Doc.show e ]
    Assign e1 e2 -> Doc.words [ Doc.show e1, Doc.text ":=", Doc.show e2 ]
    where
    -- inner :: Array t -> Doc
    inner comb =
      map (\s -> Doc.lines [ Doc.text comb, Doc.indent (display s) ])
        >> Doc.lines

    inner' =
      map (\(e ~ s) -> Doc.lines [ Doc.words [ Doc.show e, Doc.text "->" ], Doc.indent (display s) ])
        >> Doc.lines
        >> Doc.indent

    inner'' =
      map (\(l ~ e ~ s) -> Doc.lines [ Doc.words [ Doc.text l, Doc.text "|", Doc.show e, Doc.text "->" ], Doc.indent (display s) ])
        >> Doc.lines
        >> Doc.indent
