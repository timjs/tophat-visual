module Task.Script.Syntax
  -- # Synonyms
  ( Row
  , Labels
  , showLabels
  , Name
  , Label
  , Message
  -- # Types
  , Type(..)
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
  , Argument(..)
  , Constant(..)
  -- # Matches
  , Match(..)
  -- # Statements
  , Statement(..)
  , Task(..)
  -- # Errors
  , Unchecked(..)
  , Checked(..)
  , Error(..)
  ) where

import Preload
import Data.HashMap as HashMap
import Data.HashSet as HashSet

---- Synonyms ------------------------------------------------------------------
type Row a
  = HashMap Label a

showRow :: forall a. Show a => Char -> Char -> String -> HashMap Label a -> String
showRow beg end sep as =
  as
    |> HashMap.toArrayBy (\l x -> unwords [ l, sep, show x ])
    |> intercalate ","
    |> inbetween beg end

showFields :: forall a. Show a => String -> HashMap Label a -> String
showFields = showRow '{' '}'

showVariants :: forall a. Show a => HashMap String a -> String
showVariants = showRow '[' ']' ":"

type Labels
  = HashSet Label

showLabels :: HashSet Label -> String
showLabels = HashSet.toArray >> intercalate "," >> inbetween '[' ']'

type Name
  = String

type Label
  = String

type Message
  = String

---- Types ---------------------------------------------------------------------
data Type
  = TFunction Type Type
  | TList Type
  | TRecord (Row Type)
  | TVariant (Row Type)
  | TReference BasicType
  | TTask (Row Type)
  | TPrimitive PrimType

derive instance eqType :: Eq Type

instance showType :: Show Type where
  show = case _ of
    TFunction t1 t2 ->
      unwords [ show t1, "->", show t2 ]
        |> inbetween '(' ')'
    TList t -> unwords [ "List", show t ]
    TRecord ts -> showFields ":" ts
    TVariant ts -> showVariants ts
    TReference t -> unwords [ "Ref", show t ]
    TTask t -> unwords [ "Task", show t ]
    TPrimitive p -> show p

ofRecord :: Type -> Maybe (Row Type)
ofRecord = case _ of
  TRecord r -> Just r
  _ -> Nothing

ofVariant :: Type -> Maybe (Row Type)
ofVariant = case _ of
  TVariant r -> Just r
  _ -> Nothing

ofReference :: Type -> Maybe BasicType
ofReference = case _ of
  TReference b -> Just b
  _ -> Nothing

ofTask :: Type -> Maybe (Row Type)
ofTask = case _ of
  TTask r -> Just r
  _ -> Nothing

data PrimType
  = TBool
  | TInt
  | TString

derive instance eqPrimType :: Eq PrimType

instance showPrimType :: Show PrimType where
  show = case _ of
    TBool -> "Bool"
    TInt -> "Int"
    TString -> "String"

data BasicType
  = BList BasicType
  | BRecord (Row BasicType)
  | BVariant (Row BasicType)
  | BPrimitive PrimType

derive instance eqBasicType :: Eq BasicType

instance showBasicType :: Show BasicType where
  show = case _ of
    BList t -> unwords [ "List", show t ]
    BRecord ts -> showFields ":" ts
    BVariant ts -> showVariants ts
    BPrimitive p -> show p

ofType :: Type -> Maybe BasicType
ofType = case _ of
  TPrimitive p -> Just <| BPrimitive p
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

ofBasic :: BasicType -> Type
ofBasic = case _ of
  BList t -> TList <| ofBasic t
  BRecord r -> TRecord <| map ofBasic r
  BVariant r -> TVariant <| map ofBasic r
  BPrimitive p -> TPrimitive p

isBasic :: Type -> Bool
isBasic t
  | Just _ <- ofType t = true
  | otherwise = false

---- Expressions ---------------------------------------------------------------
data Expression
  = Lambda Match Type Expression
  | Apply Expression Expression
  | Variable Name
  | IfThenElse Expression Expression Expression
  | Case Expression (Row (Match * Expression))
  | Record (Row Expression)
  | Variant Label Expression Type
  | Nil Type
  | Cons Expression Expression
  | Constant Constant

derive instance eqExpression :: Eq Expression

instance showExpression :: Show Expression where
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
        , unlines (HashMap.toArrayBy (\m e -> unwords [ show m, "~>", show e ] |> indent 2) ms)
        ]
    Record es -> showFields "=" es
    Variant l e t -> unwords [ l, show e, "as", show t ]
    Nil t -> unwords [ "[]", "as", show t ]
    Cons e1 e2 -> unwords [ show e1, "::", show e2 ]
    Constant c -> show c

data Argument
  = ARecord (Row Expression)

derive instance eqArgument :: Eq Argument

instance showArgument :: Show Argument where
  show (ARecord es) = showFields "=" es

data Constant
  = B Bool
  | I Int
  | S String

derive instance eqConstant :: Eq Constant

instance showConstant :: Show Constant where
  show = case _ of
    B true -> "True"
    B false -> "False"
    I i -> show i
    S s -> show s

---- Matches -------------------------------------------------------------------
data Match
  = MIgnore
  | MBind Name
  | MRecord (Row Match)
  | MUnpack

derive instance eqMatch :: Eq Match

instance showMatch :: Show Match where
  show = case _ of
    MIgnore -> "_"
    MBind x -> x
    MRecord ms -> showFields "=" ms
    MUnpack -> "{..}"

---- Statements ----------------------------------------------------------------
data Statement t
  = Step Match t (Statement t)
  | Task t

derive instance eqStatement :: Eq t => Eq (Statement t)

instance showStatement :: Show t => Show (Statement t) where
  show = case _ of
    Step m t s -> unlines [ unwords [ show m, "<-", show t ], show s ]
    Task t -> show t

data Task t
  -- Editors
  = Enter BasicType Message
  | Update Message Expression
  | Change Message Expression
  | View Message Expression
  | Watch Message Expression
  -- Basics
  | Lift Expression
  | Pair (List (Statement t))
  | Choose (List (Statement t))
  | Branch (List (Expression * (Statement t)))
  | Select (List (Label * Expression * (Statement t)))
  -- Extras
  | Execute Name Argument
  | Hole Argument
  -- Shares
  | Share Expression
  | Assign Expression Expression

derive instance eqTask :: Eq t => Eq (Task t)

instance showTask :: Show t => Show (Task t) where
  show = case _ of
    Enter t m -> unwords [ "enter", show t, quote m ]
    Update m e -> unwords [ "update", quote m, show e ]
    Change m e -> unwords [ "change", quote m, show e ]
    View m e -> unwords [ "view", quote m, show e ]
    Watch m e -> unwords [ "watch", quote m, show e ]
    Lift e -> unwords [ "done", show e ]
    Pair ss -> unwords [ "all", inner ss ]
    Choose ss -> unwords [ "any", inner ss ]
    Branch bs -> unwords [ "one", inner' bs ]
    Select bs -> unwords [ "select", inner'' bs ]
    Execute n as -> unwords [ n, show as ]
    Hole as -> unwords [ "?", show as ]
    Share e -> unwords [ "share", show e ]
    Assign e1 e2 -> unwords [ show e1, ":=", show e2 ]
    where
    inner =
      map show
        >> unlines
        >> inbetween '[' ']'

    inner' =
      map (\(e : s) -> unwords [ show e, "~>", show s ])
        >> unlines
        >> inbetween '[' ']'

    inner'' =
      map (\(l : e : s) -> unwords [ l, "?", show e, "~>", show s ])
        >> unlines
        >> inbetween '[' ']'

---- Checking ------------------------------------------------------------------
data Unchecked f
  = Unchecked (f (Unchecked f))

--NOTE: below instance is infinite, we create one for Tasks only
-- instance showUnchecked :: Show (f (Unchecked f)) => Show (Unchecked f) where
instance showUnchecked :: Show (Unchecked Task) where
  show (Unchecked x) = show x

data Checked f
  = Pass Type (f (Checked f))
  | Fail Error (f (Checked f))

instance showChecked :: Show (Checked Task) where
  show = case _ of
    Pass _ x -> unwords [ "(o)", show x ]
    Fail e x -> unwords [ "(!", show e, "!)", show x ]

data Error
  = Error

instance showError :: Show Error where
  show Error = "Error!"

test1 :: Statement (Unchecked Task)
test1 = Task <| Unchecked <| Enter (BPrimitive TString) "Hallo"

test2 :: Statement (Unchecked Task)
test2 = Task <| Unchecked <| Select <| from [ "Continue" : Constant (B true) : test1 ]
