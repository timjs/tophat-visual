module Task.Script.World where

import Preload

import Task.Script.Annotation (Checked)
import Task.Script.Context (Context, Typtext)
import Task.Script.Label (Labeled)
import Task.Script.Syntax (Parameters, Task)

type World =
  { types :: Typtext
  , context :: Context
  , tasks :: Labeled (Parameters * Checked Task)
  }