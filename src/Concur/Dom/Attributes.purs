module Concur.Dom.Attributes
  ( module Reexport
  , rx
  , ry
  , display
  , align_items
  , justify_content
  , flex_direction
  ) where

import Preload
import Concur.Core.Props as Concur
import Concur.React.Props as Reexport
import React.DOM.Props as React

rx :: forall a. String -> Reexport.ReactProps a
rx = Concur.PrimProp << React.unsafeMkProps "rx"

ry :: forall a. String -> Reexport.ReactProps a
ry = Concur.PrimProp << React.unsafeMkProps "ry"

display :: forall a. String -> Reexport.ReactProps a
display = Concur.PrimProp << React.unsafeMkProps "display"

align_items :: forall a. String -> Reexport.ReactProps a
align_items = Concur.PrimProp << React.unsafeMkProps "align_items"

justify_content :: forall a. String -> Reexport.ReactProps a
justify_content = Concur.PrimProp << React.unsafeMkProps "justify_content"

flex_direction :: forall a. String -> Reexport.ReactProps a
flex_direction = Concur.PrimProp << React.unsafeMkProps "flex_direction"
