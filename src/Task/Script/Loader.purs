module Task.Script.Loader where

import Preload
import Prelude

import Data.HashMap as HashMap
import Task.Script.Annotation (Annotated(..), Checked, Status(..))
import Task.Script.Checker as Checker
import Task.Script.Context (Context, Typtext, aliases, builtins)
import Task.Script.Error (Error)
import Task.Script.Expander (expand)
import Task.Script.Syntax (Task, Type_)
import Task.Script.Validator as Validator

-- load :: Typtext -> Context -> Checked Task -> Error +

load :: Typtext -> Context -> Error + Typtext * Context
load s g = do
  types' <- traverse (expand types) types
  names' <- traverse (expand types') names
  done <| types' ~ names'
  where
  types = s \/ aliases
  names = g \/ builtins

check :: Typtext -> Context -> Checked Task -> Error + Type_
check s g t = do
  types' ~ names' <- load s g
  Checker.check types' names' t

validate :: Typtext -> Context -> Checked Task -> Checked Task
validate s g c@(Annotated _ t) = case load s g of
  Right (types' ~ names') -> Validator.validate types' names' c
  Left err -> Annotated (Failure g err) t

infixr 5 HashMap.union as \/
