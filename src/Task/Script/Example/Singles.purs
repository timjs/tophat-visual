module Task.Script.Example.Singles where

import Preload

import Task.Script.Annotation (Checked)
import Task.Script.Builder (always, branch, enter, step, match, choose, execute, hole, never, pair, end, select)
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

new_task :: Checked Task
new_task = hole

a_task :: Checked Task
a_task = execute "a task" (ARecord <| from [ "x" ~ Variable "x", "y" ~ Variable "y" ])

some_task :: Checked Task
some_task = execute "some task" (ARecord <| from [])

other_task :: Checked Task
other_task = execute "other task" (ARecord <| from [])

similar_task :: Checked Task
similar_task = execute "similar task" (ARecord <| from [])

some_pair :: Checked Task
some_pair = end (match [ "p", "q", "r" ]) <| pair [ end (match [ "p", "q" ]) some_task, end (match [ "r" ]) other_task ]

some_choose :: Checked Task
some_choose = end (match [ "p" ]) <| choose [ end (match [ "p", "q" ]) some_task, end (match [ "p", "r" ]) similar_task ]

-- branch (MRecord <| from []) hole
--   [ always ~ hole
--   , never ~ hole
--   ]

types = from []
context = from
  [ "a task" ~ recordOf [ "x" ~ BPrimitive TString, "y" ~ BPrimitive TString ] :-> taskOf []
  , "some task" ~ recordOf [] :-> taskOf [ "p" ~ BPrimitive TString, "q" ~ BPrimitive TString ]
  , "other task" ~ recordOf [] :-> taskOf [ "r" ~ BPrimitive TString ]
  , "similar task" ~ recordOf [] :-> taskOf [ "p" ~ BPrimitive TString, "r" ~ BPrimitive TString ]
  ]

tasks = from
  [ "internal_unguarded" ~ (from [] ~ internal_unguarded)
  , "internal_guarded" ~ (from [] ~ internal_guarded)
  , "external_unguarded" ~ (from [] ~ external_unguarded)
  , "external_guarded" ~ (from [] ~ external_guarded)
  , "new task" ~ (from [] ~ new_task)
  , "a task" ~ (from [ "x" ~ TPrimitive TString, "y" ~ TPrimitive TString ] ~ some_task)
  , "some task" ~ (from [] ~ some_task)
  , "other task" ~ (from [] ~ some_task)
  , "similar task" ~ (from [] ~ some_task)
  , "some pair" ~ (from [ "x" ~ TPrimitive TString, "y" ~ TPrimitive TString ] ~ some_pair)
  , "some choose" ~ (from [ "x" ~ TPrimitive TString, "y" ~ TPrimitive TString ] ~ some_choose)
  ]

world = { types, context, tasks }