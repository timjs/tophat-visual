module Test.Selection where

import Preload
import Concur (display, repeat, dynamic, hold)
import Concur.Dom (Signal, Widget)
import Concur.Dom.Input as Input
import Concur.Dom.Node as Node

selection :: Array String -> String -> Widget String
selection xs s = do
  Node.div'
    [ Input.picker_generic xs s
    , Node.text <| show { content: xs, selection: s }
    ]

selection_ :: String -> Array String -> Signal String
selection_ s xs = do
  x <- hold s (Input.picker_generic xs s)
  display <| Node.text <| show { content: xs, selection: x }
  done x

main :: Widget String
main = repeat "B" <| selection [ "A", "B", "C", "D" ]

main_ :: forall a. Widget a
main_ = dynamic <| selection_ "A" [ "A", "B", "C", "D" ]
