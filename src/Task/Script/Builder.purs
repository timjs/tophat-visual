module Task.Script.Builder where

import Preload

import Task.Script.Annotation (Checked, unchecked)
import Task.Script.Label (Label, Name)
import Task.Script.Syntax (Arguments(..), Constant(..), Expression(..), Match(..), Task(..))

---- Editors ----

enter :: Name -> Checked Task
enter n = unchecked <| Enter n

update :: Expression -> Checked Task
update e = unchecked <| Update e

view :: Expression -> Checked Task
view e = unchecked <| View e

watch :: Expression -> Checked Task
watch e = unchecked <| Watch e

change :: Expression -> Checked Task
change e = unchecked <| Change e

---- Steps ----

step :: Match -> Checked Task -> Checked Task -> Checked Task
step m t1 t2 = branch m t1 [ always ~ t2 ]

cont :: Match -> Checked Task -> Checked Task -> Checked Task
cont m t1 t2 = select m t1 [ "Continue" ~ always ~ t2 ]

branch :: Match -> Checked Task -> Array (Expression * Checked Task) -> Checked Task
branch m t1 bs = unchecked <| Step m t1 (unchecked <| Branch bs)

select :: Match -> Checked Task -> Array (Label * Expression * Checked Task) -> Checked Task
select m t1 bs = unchecked <| Step m t1 (unchecked <| Select bs)

always :: Expression
always = Constant (B true)

never :: Expression
never = Constant (B false)

-- NOTE:
-- Be aware of the INVARIANT: Branch and Select need to be inside a Step.
-- 1. Inserting a new tasks into a step, means we have to create a Branch.
-- 2. The new and empty branch needs to start with a Step,
--    otherwise we've a single branch.
-- 3. The old branches need to be inserted into this Step.
new :: Checked Task -> Checked Task
new t = unchecked <| Branch [ always ~ (unchecked <| Step (MRecord <| from []) hole t) ]

---- Combinators ----

pair :: Array (Checked Task) -> Checked Task
pair ts = unchecked <| Pair ts

choose :: Array (Checked Task) -> Checked Task
choose ts = unchecked <| Choose ts

item :: Checked Task
-- item = step (MRecord <| from []) hole (lift (Record <| from []))
item = step (MRecord <| from []) hole (lift Wildcard)

---- Tasks ----

execute :: String -> Arguments -> Checked Task
execute n as = unchecked <| Execute n as

hole :: Checked Task
hole = unchecked <| Hole (ARecord <| from [])

lift :: Expression -> Checked Task
lift e = unchecked <| Lift e