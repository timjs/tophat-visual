module Task.Script.Builder where

import Preload

import Task.Script.Annotation (Checked, unchecked)
import Task.Script.Syntax (Arguments, Constant(..), Expression(..), Label, Match, Task(..))

view :: Expression -> Checked Task
view e = unchecked <| View e

step :: Match -> Checked Task -> Checked Task -> Checked Task
-- step m t1 t2 = unchecked <| Step m t1 t2
step m t1 t2 = unchecked <| Step m t1 (unchecked <| Branch [ Constant (B true) ~ t2 ])

cont :: Match -> Checked Task -> Checked Task -> Checked Task
cont m t1 t2 = unchecked <| Step m t1 (unchecked <| Select [ "Continue" ~ Constant (B true) ~ t2 ])

branch :: Match -> Checked Task -> Array (Expression * Checked Task) -> Checked Task
branch m t1 bs = unchecked <| Step m t1 (unchecked <| Branch bs)

select :: Match -> Checked Task -> Array (Label * Expression * Checked Task) -> Checked Task
select m t1 bs = unchecked <| Step m t1 (unchecked <| Select bs)

pair :: Array (Checked Task) -> Checked Task
pair ts = unchecked <| Pair ts

choose :: Array (Checked Task) -> Checked Task
choose ts = unchecked <| Choose ts

execute :: String -> Arguments -> Checked Task
execute n as = unchecked <| Execute n as

lift :: Expression -> Checked Task
lift e = unchecked <| Lift e