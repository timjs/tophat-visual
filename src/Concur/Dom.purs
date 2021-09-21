module Concur.Dom
  -- # Types
  ( Dom
  , Attr
  , Widget
  , Signal
  -- # Elements
  , block
  , blockWithData
  , inline
  , inlineWithData
  -- # Values
  , stringValue
  , intValue
  , floatValue
  -- # Run
  , runWidgetInDom
  ) where

import Preload
import Concur (Widget, Signal) as Concur
import Concur.React (HTML) as React
import Concur.React.Props (unsafeTargetValue) as React
import Concur.Dom.Node as Node
import Concur.Dom.Attr as Attr
import Concur.React.Run (runWidgetInDom) as React
import React.SyntheticEvent (SyntheticEvent_) as React
import Data.Int as Int
import Data.Number as Number

---- Types ---------------------------------------------------------------------

type Dom = React.HTML

type Attr a = Attr.Attr a

type Widget = Concur.Widget Dom

type Signal a = Concur.Signal Dom a

---- Elements ------------------------------------------------------------------

block :: forall a. Array String -> Array (Widget a) -> Widget a
block cs = Node.div (map Attr.className cs)

blockWithData :: forall a r. Array String -> Record r -> Array (Widget a) -> Widget a
blockWithData cs r = Node.div ((map Attr.className cs) ++ [ Attr._data r ])

inline :: forall a. Array String -> Array (Widget a) -> Widget a
inline cs = Node.span (map Attr.className cs)

inlineWithData :: forall a r. Array String -> Record r -> Array (Widget a) -> Widget a
inlineWithData cs r = Node.span ((map Attr.className cs) ++ [ Attr._data r ])

---- Target values -------------------------------------------------------------

stringValue :: forall a. React.SyntheticEvent_ a -> String
stringValue = React.unsafeTargetValue

floatValue :: forall a. React.SyntheticEvent_ a -> Maybe Number
floatValue = stringValue >> Number.fromString

intValue :: forall a. React.SyntheticEvent_ a -> Maybe Int
intValue e = floatValue e >-> Int.floor

---- Running -------------------------------------------------------------------

runWidgetInDom :: forall a. String -> Widget a -> Effect Unit
runWidgetInDom = React.runWidgetInDom
