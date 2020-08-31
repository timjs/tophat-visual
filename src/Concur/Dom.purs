module Concur.Dom
  ( Dom
  , Attr
  , runWidgetInDom
  ) where

import Preload
import Concur (Widget)
import Concur.React (HTML)
import Concur.React.Props as Attr
import Concur.React.Run as Run

---- Types ---------------------------------------------------------------------
type Dom
  = HTML

type Attr a
  = Attr.ReactProps a

---- Running -------------------------------------------------------------------
runWidgetInDom :: forall a. String -> Widget Dom a -> Effect Unit
runWidgetInDom = Run.runWidgetInDom
