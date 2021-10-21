module Task.Script.Context
  ( Context
  , builtins
  , Typtext
  , aliases
  , isOperator
  -- # Types
  , (:->)
  , listOf
  , recordOf'
  , recordOf
  , taskOf
  ) where

import Preload

import Data.HashMap as HashMap
import Data.String (Pattern(..), Replacement(..))
import Data.String as String

import Task.Script.Label (Name)
import Task.Script.Type (BasicType(..), FullType(..), PrimType(..), ofBasic)

---- Context -------------------------------------------------------------------

type Context
  = HashMap Name FullType

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
  [ "current date" ~ TReference (BPrimitive TInt)
  , "current time" ~ TReference (BPrimitive TInt)
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

readable :: forall f. Functor f => f String -> f String
readable = map (String.replace (Pattern "_") (Replacement " "))

unreadable :: forall f. Functor f => f String -> f String
unreadable = map (String.replace (Pattern " ") (Replacement "_"))

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
