module Task.Script.World where

import Preload

import Task.Script.Annotation (Checked)
import Task.Script.Context (Context, Typtext)
import Task.Script.Label (Labeled)
import Task.Script.Syntax (Task)
import Task.Script.Type (FullType)

type Parameters = Labeled FullType
type Tasktext = Labeled (Parameters * Checked Task)

type World =
  { types :: Typtext
  , context :: Context
  , tasks :: Tasktext
  }