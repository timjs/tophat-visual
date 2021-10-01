module Task.Script.Context
  ( Context
  , builtins
  , Typtext
  , aliases
  -- # Types
  , (:->)
  , listOf
  , recordOf'
  , recordOf
  , taskOf
  ) where

import Preload
import Task.Script.Label (Name)
import Task.Script.Type (BasicType(..),  PrimType(..), FullType(..), ofBasic)

---- Context -------------------------------------------------------------------

type Context
  = HashMap Name FullType

builtins :: Context
builtins =
  from
    [ "not" ~ TPrimitive TBool :-> TPrimitive TBool
    , "&&" ~ TPrimitive TBool :-> TPrimitive TBool :-> TPrimitive TBool
    , "||" ~ TPrimitive TBool :-> TPrimitive TBool :-> TPrimitive TBool
    , "==" ~ TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TBool
    , "/=" ~ TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TBool
    , "+" ~ TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TInt
    , "*" ~ TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TInt
    , "-" ~ TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TInt
    , "/" ~ TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TInt
    ]

type Typtext
  = HashMap Name BasicType

aliases :: Typtext
aliases =
  from
    [ "Bool" ~ BPrimitive TBool
    , "Int" ~ BPrimitive TInt
    , "Nat" ~ BPrimitive TInt
    , "Date" ~ BPrimitive TInt
    , "String" ~ BPrimitive TString
    ]

---- Types ---------------------------------------------------------------------

infixr 3 TFunction as :->

listOf :: BasicType -> FullType
listOf = BList >> ofBasic

recordOf' :: Array (String * BasicType) -> BasicType
recordOf' = from >> BRecord

recordOf :: Array (String * BasicType) -> FullType
recordOf = recordOf' >> ofBasic

taskOf :: Array (String * BasicType) -> FullType
taskOf = from >> map ofBasic >> TTask
