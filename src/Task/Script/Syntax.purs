module Task.Script.Syntax
  ( -- * Synonyms
    Row,
    Labels,
    Name,
    Label,
    Message,

    -- * Types
    Ty (..),
    ofRecord,
    ofVariant,
    ofReference,
    ofTask,
    PrimTy (..),
    BasicTy (..),
    ofType,
    ofBasic,
    isBasic,

    -- * Expressions
    Expression (..),
    Argument (..),
    Constant (..),

    -- * Matches
    Match (..),

    -- * Statements
    Statement (..),
    Task (..),
  )
where

import qualified Data.HashMap.Strict as HashMap

---- Synonyms ------------------------------------------------------------------

type Row a = HashMap Label a

type Labels = HashSet Label

type Name = Text

type Label = Text

type Message = Text

---- Types ---------------------------------------------------------------------

data Ty
  = TFunction Ty Ty
  | TList Ty
  | TRecord (Row Ty)
  | TVariant (Row Ty)
  | TReference BasicTy
  | TTask (Row Ty)
  | TPrimitive PrimTy
  deriving (Eq, Ord, Debug)

instance Display Ty where
  display = \case
    TFunction t1 t2 -> unwords [display t1, "->", display t2] |> between '(' ')'
    TList t -> unwords ["List", display t]
    TRecord r -> display r
    TVariant r -> display r
    TReference t -> unwords ["Ref", display t]
    TTask t -> unwords ["Task", display t]
    TPrimitive p -> display p

ofRecord :: Ty -> Maybe (Row Ty)
ofRecord = \case
  TRecord r -> Just r
  _ -> Nothing

ofVariant :: Ty -> Maybe (Row Ty)
ofVariant = \case
  TVariant r -> Just r
  _ -> Nothing

ofReference :: Ty -> Maybe BasicTy
ofReference = \case
  TReference b -> Just b
  _ -> Nothing

ofTask :: Ty -> Maybe (Row Ty)
ofTask = \case
  TTask r -> Just r
  _ -> Nothing

data PrimTy
  = TBool
  | TInt
  | TString
  deriving (Eq, Ord, Debug)

instance Display PrimTy where
  display = \case
    TBool -> "Bool"
    TInt -> "Int"
    TString -> "String"

data BasicTy
  = BList BasicTy
  | BRecord (Row BasicTy)
  | BVariant (Row BasicTy)
  | BPrimitive PrimTy
  deriving (Eq, Ord, Debug)

instance Display BasicTy where
  display = \case
    BList t -> unwords ["List", display t]
    BRecord r -> display r
    BVariant r -> HashMap.toList r |> map (\(k, v) -> display k ++ ":" ++ display v) |> intercalate "," |> between '<' '>'
    BPrimitive p -> display p

ofType :: Ty -> Maybe BasicTy
ofType = \case
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

ofBasic :: BasicTy -> Ty
ofBasic = \case
  BList t -> TList <| ofBasic t
  BRecord r -> TRecord <| map ofBasic r
  BVariant r -> TVariant <| map ofBasic r
  BPrimitive p -> TPrimitive p

isBasic :: Ty -> Bool
isBasic t
  | Just _ <- ofType t = True
  | otherwise = False

---- Expressions ---------------------------------------------------------------

data Expression
  = Lambda Match Ty Expression
  | Apply Expression Expression
  | Variable Name
  | IfThenElse Expression Expression Expression
  | Case Expression (Row (Match, Expression))
  | Record (Row Expression)
  | Variant Label Expression Ty
  | Nil Ty
  | Cons Expression Expression
  | Constant Constant
  deriving (Eq, Ord, Debug)

data Argument
  = ARecord (Row Expression)
  deriving (Eq, Ord, Debug)

data Constant
  = B Bool
  | I Int
  | S Text
  deriving (Eq, Ord, Debug)

instance Display Constant where
  display = \case
    B b -> display b
    I i -> display i
    S s -> display s

---- Matches -------------------------------------------------------------------

data Match
  = MIgnore
  | MBind Name
  | MRecord (Row Match)
  | MUnpack
  deriving (Eq, Ord, Debug)

instance Display Match where
  display = \case
    MIgnore -> "_"
    MBind x -> x
    MRecord ms -> display ms
    MUnpack -> "{..}"

---- Statements ----------------------------------------------------------------

data Statement
  = Step Match Task Statement
  | Task Task
  deriving (Eq, Ord, Debug)

data Task
  = -- Editors
    Enter BasicTy Message
  | Update Message Expression
  | Change Message Expression
  | View Message Expression
  | Watch Message Expression
  | -- Basics
    Done Expression
  | Pair (List Statement)
  | Choose (List Statement)
  | Branch (List (Expression, Statement))
  | Select (List (Label, Expression, Statement))
  | -- Extras
    Execute Name Argument
  | Hole Argument
  | -- Shares
    Share Expression
  | Assign Expression Expression
  deriving (Eq, Ord, Debug)
