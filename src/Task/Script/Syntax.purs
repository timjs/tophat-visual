module Task.Script.Syntax
  -- # Synonyms
  ( Row_
  , Labels
  , Name
  , Label
  , showLabels
  , Message
  -- # Types
  , Type_(..)
  , ofRecord
  , ofVariant
  , ofReference
  , ofTask
  , PrimType(..)
  , BasicType(..)
  , ofType
  , ofBasic
  , isBasic
  -- # Expressions
  , Expression(..)
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
import Data.HashSet as HashSet

---- Synonyms ------------------------------------------------------------------

type Row_ a
  = HashMap Label a

showRow :: forall a. Show a => Char -> Char -> String -> Row_ a -> String
showRow beg end sep as =
  as
    |> HashMap.toArrayBy check
    |> intercalate ", "
    |> enclose beg end
  where
  check l x =
    let
      r = show x
    in
      if l == r then
        l
      else
        unwords [ l, sep, r ]

showFields :: forall a. Show a => String -> Row_ a -> String
showFields = showRow '{' '}'

showVariants :: forall a. Show a => Row_ a -> String
showVariants = showRow '[' ']' ":"

type Labels
  = HashSet Label

showLabels :: HashSet Label -> String
showLabels = HashSet.toArray >> intercalate "," >> enclose '{' '}'

type Name
  = String

type Label
  = String

type Message
  = String

---- Types ---------------------------------------------------------------------

data Type_
  = TFunction Type_ Type_
  | TName Name
  | TList Type_
  | TRecord (Row_ Type_)
  | TVariant (Row_ Type_)
  | TReference BasicType
  | TTask (Row_ Type_)
  | TPrimitive PrimType

derive instance Eq Type_

instance Show Type_ where
  show = case _ of
    TFunction t1 t2 ->
      unwords [ show t1, "->", show t2 ]
        |> enclose '(' ')'
    TName n -> n
    TList t -> unwords [ "List", show t ]
    TRecord ts -> showFields ":" ts
    TVariant ts -> showVariants ts
    TReference t -> unwords [ "Ref", show t ]
    TTask t -> unwords [ "Task", showFields ":" t ]
    TPrimitive p -> show p

ofRecord :: Type_ -> Maybe (Row_ Type_)
ofRecord = case _ of
  TRecord r -> Just r
  _ -> Nothing

ofVariant :: Type_ -> Maybe (Row_ Type_)
ofVariant = case _ of
  TVariant r -> Just r
  _ -> Nothing

ofReference :: Type_ -> Maybe BasicType
ofReference = case _ of
  TReference b -> Just b
  _ -> Nothing

ofTask :: Type_ -> Maybe (Row_ Type_)
ofTask = case _ of
  TTask r -> Just r
  _ -> Nothing

data PrimType
  = TBool
  | TInt
  | TString

derive instance Eq PrimType

instance Show PrimType where
  show = case _ of
    TBool -> "Bool"
    TInt -> "Int"
    TString -> "String"

data BasicType
  = BName Name
  | BList BasicType
  | BRecord (Row_ BasicType)
  | BVariant (Row_ BasicType)
  | BPrimitive PrimType

derive instance Eq BasicType

instance Show BasicType where
  show = case _ of
    BName n -> n
    BList t -> unwords [ "List", show t ]
    BRecord ts -> showFields ":" ts
    BVariant ts -> showVariants ts
    BPrimitive p -> show p

ofType :: Type_ -> Maybe BasicType
ofType = case _ of
  TPrimitive p -> Just <| BPrimitive p
  TName n -> Just <| BName n
  TList t
    | Just t' <- ofType t -> Just <| BList t'
    | otherwise -> Nothing
  TRecord r
    | Just ts <- traverse ofType r -> Just <| BRecord ts
    | otherwise -> Nothing
  TVariant r
    | Just ts <- traverse ofType r -> Just <| BVariant ts
    | otherwise -> Nothing
  TFunction _ _ -> Nothing
  TReference _ -> Nothing
  TTask _ -> Nothing

ofBasic :: BasicType -> Type_
ofBasic = case _ of
  BList t -> TList <| ofBasic t
  BRecord r -> TRecord <| map ofBasic r
  BVariant r -> TVariant <| map ofBasic r
  BName n -> TName n
  BPrimitive p -> TPrimitive p

isBasic :: Type_ -> Bool
isBasic t
  | Just _ <- ofType t = true
  | otherwise = false

---- Expressions ---------------------------------------------------------------

data Expression
  = Lambda Match Type_ Expression
  | Apply Expression Expression
  | Variable Name
  | IfThenElse Expression Expression Expression
  | Case Expression (Row_ (Match * Expression))
  | Record (Row_ Expression)
  | Variant Label Expression Type_
  | Nil Type_
  | Cons Expression Expression
  | Constant Constant

derive instance Eq Expression

instance Show Expression where
  show = case _ of
    Lambda m t e -> unwords [ show m, ":", show t, ".", show e ]
    Apply e1 e2 -> unwords [ show e1, show e2 ] --FIXME
    Variable n -> n
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
    Variant l e t -> unwords [ l, show e, "as", show t ]
    Nil t -> unwords [ "[]", "as", show t ]
    Cons e1 e2 -> unwords [ show e1, "::", show e2 ]
    Constant c -> show c

data Arguments
  = ARecord (Row_ Expression)

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
  | MRecord (Row_ Match)
  | MUnpack

derive instance Eq Match

instance Show Match where
  show = case _ of
    MIgnore -> "_"
    MBind x -> x
    MRecord ms -> showFields "=" ms
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

data Task t
  -- Steps
  = Step Match t t
  | Branch (Branches t)
  | Select (LabeledBranches t)
  -- Editors
  | Enter Name Message
  | Update Message Expression
  | Change Message Expression
  | View Message Expression
  | Watch Message Expression
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
    Branch bs -> Doc.lines [ Doc.text "branch", inner' bs ]
    Select bs -> Doc.lines [ Doc.text "select", inner'' bs ]
    Enter t m -> Doc.words [ Doc.text "enter", Doc.text t, Doc.quotes (Doc.text m) ]
    Update m e -> Doc.words [ Doc.text "update", Doc.quotes (Doc.text m), Doc.show e ]
    Change m e -> Doc.words [ Doc.text "change", Doc.quotes (Doc.text m), Doc.show e ]
    View m e -> Doc.words [ Doc.text "view", Doc.quotes (Doc.text m), Doc.show e ]
    Watch m e -> Doc.words [ Doc.text "watch", Doc.quotes (Doc.text m), Doc.show e ]
    Lift e -> Doc.words [ Doc.text "done", Doc.show e ]
    Pair ss -> Doc.lines [ Doc.text "pair", inner ss ]
    Choose ss -> Doc.lines [ Doc.text "choose", inner ss ]
    Execute n as -> Doc.words [ Doc.text n, Doc.show as ]
    Hole as -> Doc.words [ Doc.text "?", Doc.show as ]
    Share e -> Doc.words [ Doc.text "share", Doc.show e ]
    Assign e1 e2 -> Doc.words [ Doc.show e1, Doc.text ":=", Doc.show e2 ]
    where
    -- inner :: Array t -> Doc
    inner =
      map display
        >> Doc.lines
        >> Doc.indent

    inner' =
      map (\(e ~ s) -> Doc.lines [ Doc.words [ Doc.show e, Doc.text "|->" ], Doc.indent (display s) ])
        >> Doc.lines
        >> Doc.indent

    inner'' =
      map (\(l ~ e ~ s) -> Doc.lines [ Doc.words [ Doc.text l, Doc.text "?", Doc.show e, Doc.text "|->" ], Doc.indent (display s) ])
        >> Doc.lines
        >> Doc.indent
