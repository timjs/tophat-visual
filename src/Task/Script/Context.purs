module Task.Script.Context
  ( Context
  , builtins
  , Typtext
  , types
  -- # Types
  , (:->)
  , listOf
  , recordOf
  , taskOf
  ) where

import Preload
import Task.Script.Syntax (BasicType(..), Name, PrimType(..), Type(..), ofBasic)

---- Context -------------------------------------------------------------------
type Context
  = HashMap Name Type

builtins :: Context
builtins =
  from
    [ "not" ** TPrimitive TBool :-> TPrimitive TBool
    , "&&" ** TPrimitive TBool :-> TPrimitive TBool :-> TPrimitive TBool
    , "||" ** TPrimitive TBool :-> TPrimitive TBool :-> TPrimitive TBool
    , "==" ** TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TBool
    , "/=" ** TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TBool
    , "+" ** TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TInt
    , "*" ** TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TInt
    , "-" ** TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TInt
    , "/" ** TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TInt
    ]

type Typtext
  = HashMap Name BasicType

types :: Typtext
types =
  from
    [ "Bool" ** BPrimitive TBool
    , "Int" ** BPrimitive TInt
    , "Nat" ** BPrimitive TInt
    , "Date" ** BPrimitive TInt
    , "String" ** BPrimitive TString
    ]

---- Types ---------------------------------------------------------------------
infixr 3 TFunction as :->

listOf :: BasicType -> Type
listOf = BList >> ofBasic

recordOf :: Array (String ** BasicType) -> Type
recordOf = from >> map ofBasic >> TRecord

taskOf :: Array (String ** BasicType) -> Type
taskOf = from >> map ofBasic >> TTask
