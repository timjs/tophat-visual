module Concur.Dom.Attr
  ( module Reexport
  , Attr
  , classes
  ) where

import Preload

import Concur.React.Props as Reexport

type Attr a = Reexport.ReactProps a

classes :: forall t a. Foldable t => t String -> Attr a
classes = Reexport.className << intercalate " "