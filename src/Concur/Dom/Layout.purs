module Concur.Dom.Layout
  ( row
  , column
  ) where

import Preload
import Concur (class MultiAlternative, class ShiftMap, Widget)
import Concur.Dom (Attr, Dom)
import Concur.Dom.Node as Node
import Concur.Dom.Attr as Attr

---- Layouting -----------------------------------------------------------------
divProps :: forall a. Array (Attr a)
divProps = [ Attr.display "flex", Attr.align_items "center", Attr.justify_content "center" ]

row :: forall m a. MultiAlternative m => ShiftMap (Widget Dom) m => Array (m a) -> m a
row = Node.div <| [ Attr.flex_direction "row" ] ++ divProps

column :: forall m a. MultiAlternative m => ShiftMap (Widget Dom) m => Array (m a) -> m a
column = Node.div <| [ Attr.flex_direction "column" ] ++ divProps
