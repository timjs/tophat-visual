module Test.Counter where

import Preload
import Concur (Signal, Widget, andd, display, dynamic, loop, repeat)
import Concur.Dom (Dom)
import Concur.Dom.Node as Node
import Concur.Dom.Attr as Attr
import Data.Slice (zipWith)

---- Widgets -------------------------------------------------------------------
counter :: Int -> Widget Dom Int
counter n = do
  Node.div'
    [ n + 1 -|| Node.button [ Attr.onClick ] [ Node.text "+" ]
    , Node.text <| show n
    , n - 1 -|| Node.button [ Attr.onClick ] [ Node.text "-" ]
    ]

counters :: Array Int -> Widget Dom (Array Int)
counters xs = andd (map counter xs)

main :: Widget Dom (Array Int)
main = counters [ 1, 2, 3 ]

---- Signals -------------------------------------------------------------------
counter' :: Int -> Signal Dom Int
counter' k = repeat k counter

counters' :: Array Int -> Signal Dom (Array Int)
counters' xs = do
  display <| Node.text <| show { content: xs, isSorted: isSorted xs }
  traverse counter' xs

--NOTE: This is something completely different!
counters'' :: Array Int -> Signal Dom (Array Int)
counters'' xs = do
  display <| Node.text <| show { content: xs, isSorted: isSorted xs }
  repeat xs counters

main' :: Widget Dom (Array Int)
main' = dynamic <| loop [ 1, 2, 3 ] counters'

---- Helpers -------------------------------------------------------------------
isSorted :: Array Int -> Bool
isSorted xs = case tail xs of
  Nothing -> true
  Just ys -> and <| zipWith (\x y -> x <= y) (view xs) ys
