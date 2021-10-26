module Task.Script.Context
  ( Context
  , showContext
  , builtins
  , Typtext
  , aliases
  , isOperator
  , readable
  , unreadable
  -- # Types
  , (:->)
  , listOf
  , recordOf'
  , recordOf
  , taskOf
  ) where

import Preload

import Data.Array as Array
import Data.HashMap as HashMap
import Data.String (Pattern(..), Replacement(..))
import Data.String as String

import Task.Script.Label (Name)
import Task.Script.Type (BasicType(..), FullType(..), PrimType(..), ofBasic)

---- Context -------------------------------------------------------------------

type Context
  = HashMap Name FullType

showContext :: Context -> String
showContext =
  flip HashMap.difference operators
    >> flip HashMap.difference builtins
    >> HashMap.toArrayBy (~)
    >> Array.sortBy (compare `on` fst)
    >> map (\(l ~ t) -> l ++ " : " ++ show t)
    >> intercalate "\n"

operators :: Context
operators = from
  -- Ver
  [ "&&" ~ TPrimitive TBool :-> TPrimitive TBool :-> TPrimitive TBool
  , "||" ~ TPrimitive TBool :-> TPrimitive TBool :-> TPrimitive TBool
  -- Eq
  , "==" ~ TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TBool
  , "/=" ~ TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TBool
  -- Ord
  , "<" ~ TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TBool
  , "<=" ~ TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TBool
  , ">=" ~ TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TBool
  , ">" ~ TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TBool
  -- Calc
  , "+" ~ TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TInt
  , "*" ~ TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TInt
  , "-" ~ TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TInt
  , "/" ~ TPrimitive TInt :-> TPrimitive TInt :-> TPrimitive TInt
  ]

functions :: Context
functions = from
  [ "not" ~ TPrimitive TBool :-> TPrimitive TBool
  ]

shares :: Context
shares = from
  [ "current date" ~ TReference (BPrimitive (TBuiltin "Date"))
  , "current time" ~ TReference (BPrimitive (TBuiltin "Time"))
  ]

builtins :: Context
builtins = operators \/ functions \/ shares

isOperator :: Name -> Bool
isOperator = flip HashMap.member operators

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

readable :: Name -> Name
readable = String.replace (Pattern "_") (Replacement " ")

unreadable :: Name -> Name
unreadable = String.replace (Pattern " ") (Replacement "_")

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

---- Helpers -------------------------------------------------------------------

infixr 5 HashMap.union as \/
