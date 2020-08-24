module Task.Script.Syntax
  -- # Synonyms
  ( Row
  , Labels
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
  ) where

import Preload
import Data.HashMap as HashMap

---- Synonyms ------------------------------------------------------------------
type Row a
  = HashMap Label a

type Labels
  = HashSet Label

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
    TFunction t1 t2 -> unwords [ show t1, "->", show t2 ] |> inbetween '(' ')'
    TList t -> unwords [ "List", show t ]
    TRecord r -> show r
    TVariant r -> show r
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
    BRecord r -> show r
    BVariant r -> HashMap.toArrayBy (/\) r |> map (\(k /\ v) -> show k ++ ":" ++ show v) |> intercalate "," |> inbetween '<' '>'
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
  | Case Expression (Row (Match /\ Expression))
  | Record (Row Expression)
  | Variant Label Expression Type
  | Nil Type
  | Cons Expression Expression
  | Constant Constant

derive instance eqExpression :: Eq Expression

data Argument
  = ARecord (Row Expression)

derive instance eqArgument :: Eq Argument

data Constant
  = B Bool
  | I Int
  | S String

derive instance eqConstant :: Eq Constant

instance showConstant :: Show Constant where
  show = case _ of
    B b -> show b
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
    MRecord ms -> show ms
    MUnpack -> "{..}"

---- Statements ----------------------------------------------------------------
data Statement
  = Step Match Task Statement
  | Task Task

derive instance eqStatement :: Eq Statement

data Task
  -- Editors
  = Enter BasicType Message
  | Update Message Expression
  | Change Message Expression
  | View Message Expression
  | Watch Message Expression
  -- Basics
  | Lift Expression
  | Pair (List Statement)
  | Choose (List Statement)
  | Branch (List (Expression /\ Statement))
  | Select (List (Label /\ Expression /\ Statement))
  -- Extras
  | Execute Name Argument
  | Hole Argument
  -- Shares
  | Share Expression
  | Assign Expression Expression

derive instance eqTask :: Eq Task
