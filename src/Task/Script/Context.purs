module Task.Script.Context
  ( Context
  , toplevel
  -- # Types
  , (:->)
  , b_bool
  , b_int
  , b_string
  , b_list
  , t_record
  , t_task
  ) where

import Preload
import Task.Script.Syntax (BasicType(..), Name, PrimType(..), Type(..), ofBasic)

---- Context -------------------------------------------------------------------
type Context
  = HashMap Name Type

toplevel :: Context
toplevel =
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

---- Types ---------------------------------------------------------------------
infixr 3 TFunction as :->

b_bool :: BasicType
b_bool = BPrimitive TBool

b_int :: BasicType
b_int = BPrimitive TInt

b_string :: BasicType
b_string = BPrimitive TString

b_list :: BasicType -> BasicType
b_list t = BList t

t_record :: Array (String ** BasicType) -> Type
t_record = from >> map ofBasic >> TRecord

t_task :: Array (String ** BasicType) -> Type
t_task = from >> map ofBasic >> TTask
