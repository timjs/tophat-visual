module Concur.Dom.Layout
  ( row
  , column
  ) where

import Preload
import Concur.Dom (Attr)
import Concur.Dom.Node (El')
import Concur.Dom.Node as Node
import Concur.Dom.Attr as Attr

---- Layouting -----------------------------------------------------------------
divProps :: forall a. Array (Attr a)
divProps = [ Attr.display "flex", Attr.align_items "center", Attr.justify_content "center" ]

row :: El'
row = Node.div <| [ Attr.flex_direction "row" ] ++ divProps

column :: El'
column = Node.div <| [ Attr.flex_direction "column" ] ++ divProps
