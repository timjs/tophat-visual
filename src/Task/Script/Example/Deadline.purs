module Task.Script.Example.Deadline where

import Preload

import Task.Script.Annotation (Checked)
import Task.Script.Builder (branch, execute, watch)
import Task.Script.Context (Typtext, Context)
import Task.Script.Syntax (Arguments(..), Expression(..), Match(..), Task)
import Task.Script.Type (FullType(..), PrimType(..))
import Task.Script.World (Tasktext, World)

types :: Typtext
types = from []

context :: Context
context = from []

tasks :: Tasktext
tasks = from
  [ "remind" ~
      (from [ "deadline" ~ TPrimitive TInt, "email" ~ TPrimitive TString ] ~ remind)
  ]

world :: World
world = { types, context, tasks }

---- Tasks ---------------------------------------------------------------------

remind :: Checked Task
remind = branch (MRecord <| from [ "value" ~ MBind "now" ]) (watch (Variable "current_date"))
  [ Variable ">=" `Apply` Variable "now" `Apply` Variable "deadline"
      ~ execute "send_reminder" (ARecord <| from [ "email" ~ Variable "email" ])

  ]