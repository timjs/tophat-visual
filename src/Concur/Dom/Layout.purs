module Concur.Dom.Layout
  ( horizontal
  , vertical
  ) where

import Preload
import Concur (class MultiAlternative, class ShiftMap, Widget)
import Concur.Dom (Attr, Dom)
import Concur.Dom.Node as Node
import Concur.Dom.Attr as Attr

---- Layouting -----------------------------------------------------------------
divProps :: forall a. Array (Attr a)
divProps = [ Attr.display "flex", Attr.align_items "center", Attr.justify_content "center" ]

horizontal :: forall m a. MultiAlternative m => ShiftMap (Widget Dom) m => Array (m a) -> m a
horizontal = Node.div <| [ Attr.flex_direction "row" ] ++ divProps

vertical :: forall m a. MultiAlternative m => ShiftMap (Widget Dom) m => Array (m a) -> m a
vertical = Node.div <| [ Attr.flex_direction "column" ] ++ divProps
