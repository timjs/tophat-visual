module Test.Selection where

import Preload
import Concur (display, dynamic, hold)
import Concur.Dom (Signal, Widget)
import Concur.Dom.Input as Input
import Concur.Dom.Node as Node

selection :: Array String -> String -> Widget String
selection xs s = do
  Node.div'
    [ Input.selectionbox "Try this" xs
    , Node.text <| show { content: xs, selection: s }
    ]

selection_ :: String -> Array String -> Signal String
selection_ s xs = do
  x <- hold s (Input.selectionbox "Try this" xs)
  display <| Node.text <| show { content: xs, selection: x }
  done x

-- main :: forall a. Widget a
main :: Widget String
main = loop "A" <| selection [ "A", "B", "C", "D" ]

loop :: forall m a. Bind m => a -> (a -> m a) -> m a
loop x w = do
  x' <- w x
  loop x' w

main_ :: forall a. Widget a
main_ = dynamic <| selection_ "A" [ "A", "B", "C", "D" ]
