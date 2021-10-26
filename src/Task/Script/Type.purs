module Task.Script.Type
  -- # Types
  ( FullType(..)
  , isFunction
  , isReference
  , isTask
  , ofRecord
  , ofVariant
  , ofReference
  , ofTask
  , PrimType(..)
  , BasicType(..)
  , ofType
  , ofBasic
  , isBasic
  ) where

import Preload

import Task.Script.Label (Labeled, Name, showFields, showVariants)

---- Types ---------------------------------------------------------------------

data FullType
  = TFunction FullType FullType
  | TName Name
  | TList FullType
  | TRecord (Labeled FullType)
  | TVariant (Labeled FullType)
  | TReference BasicType
  | TTask (Labeled FullType)
  | TPrimitive PrimType

derive instance Eq FullType

instance Show FullType where
  show = case _ of
    TFunction t1 t2 ->
      unlines
        [ show t1
        , indent 2 <| unwords [ "->", show t2 ]
        ]
    TName n -> n
    TList t -> unwords [ "List", show t ]
    TRecord ts -> showFields ":" ts
    TVariant ts -> showVariants ts
    TReference t -> unwords [ "Ref", show t ]
    TTask t -> unwords [ "Task", showFields ":" t ]
    TPrimitive p -> show p

isFunction :: FullType -> Bool
isFunction = case _ of
  TFunction _ _ -> true
  _ -> false

isReference :: FullType -> Bool
isReference = case _ of
  TReference _ -> true
  _ -> false

isTask :: FullType -> Bool
isTask = case _ of
  TFunction _ (TTask _) -> true
  _ -> false

ofRecord :: FullType -> Maybe (Labeled FullType)
ofRecord = case _ of
  TRecord r -> Just r
  _ -> Nothing

ofVariant :: FullType -> Maybe (Labeled FullType)
ofVariant = case _ of
  TVariant r -> Just r
  _ -> Nothing

ofReference :: FullType -> Maybe BasicType
ofReference = case _ of
  TReference b -> Just b
  _ -> Nothing

ofTask :: FullType -> Maybe (Labeled FullType)
ofTask = case _ of
  TTask r -> Just r
  _ -> Nothing

data PrimType
  = TBool
  | TInt
  | TString
  | TBuiltin Name

derive instance Eq PrimType

instance Show PrimType where
  show = case _ of
    TBool -> "Bool"
    TInt -> "Int"
    TString -> "String"
    TBuiltin n -> n

data BasicType
  = BName Name
  | BList BasicType
  | BRecord (Labeled BasicType)
  | BVariant (Labeled BasicType)
  | BPrimitive PrimType

derive instance Eq BasicType

instance Show BasicType where
  show = case _ of
    BName n -> n
    BList t -> unwords [ "List", show t ]
    BRecord ts -> showFields ":" ts
    BVariant ts -> showVariants ts
    BPrimitive p -> show p

ofType :: FullType -> Maybe BasicType
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

ofBasic :: BasicType -> FullType
ofBasic = case _ of
  BList t -> TList <| ofBasic t
  BRecord r -> TRecord <| map ofBasic r
  BVariant r -> TVariant <| map ofBasic r
  BName n -> TName n
  BPrimitive p -> TPrimitive p

isBasic :: FullType -> Bool
isBasic t
  | Just _ <- ofType t = true
  | otherwise = false
