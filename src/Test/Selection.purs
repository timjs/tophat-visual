module Test.Selection where

import Preload
import Concur (display, dynamic, hold)
import Concur.Dom (Signal, Widget)
import Concur.Dom.Input as Input
import Concur.Dom.Node as Node

selection_ :: String -> Array String -> Signal String
selection_ s xs = do
  x <- hold s (Input.selectionbox "Try this" xs)
  display <| Node.text <| show { content: xs, selection: x }
  done x

main :: forall a. Widget a
main = dynamic <| selection_ "A" [ "A", "B", "C", "D" ]
