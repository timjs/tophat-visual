module Task.Script.Example.Deadline where

import Preload

import Task.Script.Annotation (Checked)
import Task.Script.Builder (branch, execute, watch)
import Task.Script.Context (Context, Typtext, recordOf, taskOf, (:->))
import Task.Script.Syntax (Arguments(..), Expression(..), Match(..), Task)
import Task.Script.Type (BasicType(..), FullType(..), PrimType(..))
import Task.Script.World (Tasktext, World)

types :: Typtext
types = from []

context :: Context
context = from
  [ "send reminder"
      ~ recordOf [ "email" ~ BPrimitive TString ]
      :-> taskOf []
  ]

tasks :: Tasktext
tasks = from
  [ "remind" ~
      (from [ "deadline" ~ TPrimitive TInt, "email" ~ TPrimitive TString ] ~ remind)
  ]

world :: World
world = { types, context, tasks }

---- Tasks ---------------------------------------------------------------------

remind :: Checked Task
remind = branch (MRecord <| from [ "value" ~ MBind "now" ]) (watch (Variable "current date"))
  [ Variable ">=" `Apply` Variable "now" `Apply` Variable "deadline"
      ~ execute "send reminder" (ARecord <| from [ "email" ~ Variable "email" ])

  ]