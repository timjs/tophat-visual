module Task.Script.Example.Singles where

import Preload

import Task.Script.Annotation (Checked)
import Task.Script.Builder (always, branch, enter, execute, hole, never, select)
import Task.Script.Context ((:->), recordOf, taskOf)
import Task.Script.Syntax (Arguments(..), Constant(..), Expression(..), Match(..), Task)
import Task.Script.Type (BasicType(..), FullType(..), PrimType(..))

internal_unguarded :: Checked Task
internal_unguarded = branch (MRecord <| from []) (enter "Bool")
  [ Constant (B true) ~ hole ]

internal_guarded :: Checked Task
internal_guarded = branch (MRecord <| from [ "x" ~ MBind "x" ]) (enter "Bool")
  [ Variable ">" `Apply` Variable "x" `Apply` Constant (I 10) ~ hole ]

external_unguarded :: Checked Task
external_unguarded = select (MRecord <| from []) (enter "Bool")
  [ "Continue" ~ Constant (B true) ~ hole ]

external_guarded :: Checked Task
external_guarded = select (MRecord <| from [ "x" ~ MBind "x" ]) (enter "Bool")
  [ "Continue" ~ Variable ">" `Apply` Variable "x" `Apply` Constant (I 10) ~ hole ]

just_execute :: Checked Task
just_execute = execute "some task" (ARecord <| from [ "a" ~ Variable "a", "b" ~ Variable "b" ])

new_task :: Checked Task
new_task = hole

-- branch (MRecord <| from []) hole
--   [ always ~ hole
--   , never ~ hole
--   ]

types = from []
context = from [ "some task" ~ recordOf [ "a" ~ BPrimitive TString, "b" ~ BPrimitive TString ] :-> taskOf [] ]
tasks = from
  [ "internal_unguarded" ~ (from [] ~ internal_unguarded)
  , "internal_guarded" ~ (from [] ~ internal_guarded)
  , "external_unguarded" ~ (from [] ~ external_unguarded)
  , "external_guarded" ~ (from [] ~ external_guarded)
  , "new task" ~ (from [] ~ new_task)
  , "just execute" ~ (from [] ~ just_execute)
  ]

world = { types, context, tasks }