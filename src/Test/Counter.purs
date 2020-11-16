module Test.Counter where

import Preload
import Concur (combine, display, dynamic, loop, fix)
import Concur.Dom (Widget, Signal)
import Concur.Dom.Node as Node
import Concur.Dom.Attr as Attr
import Concur.Dom.Icon as Icon
import Data.Slice (zipWith)

---- Widgets -------------------------------------------------------------------
counter :: Int -> Widget Int
counter n = do
  Node.div'
    [ Node.button [ Attr.onClick ||- n + 1 ] [ Icon.plus_square ]
    , Node.text <| show n
    , Node.button [ Attr.onClick ||- n - 1 ] [ Icon.minus_square ]
    ]

-- | Displays a counter for every widget.
-- |
-- |* Widgets end after the first event has been sent.
-- |* Results are collected in an array.
counters :: Array Int -> Widget (Array Int)
counters xs = combine (map counter xs)

main :: Widget (Array Int)
main = counters [ 1, 2, 3 ]

---- Signals -------------------------------------------------------------------
-- | Repeat the counter widget as a signal.
-- |
-- | * Signal starts with given initial value.
-- | * Widget is repeated with resulting value.
-- | * Value is kept in the signal.
counter_ :: Int -> Signal Int
-- counter_ k = loop k counter
counter_ k = loop k counter

-- | Dynamically traverse counter signal.
-- |
-- | * Because counter_ is a signal, we can get hold of its value.
counters_ :: Array Int -> Signal (Array Int)
counters_ xs = do
  display <| Node.text <| show { content: xs, isSorted: isSorted xs }
  traverse counter_ xs

-- | Statically fix the counters widget.
-- |
-- | This is something completely different!
-- | * The input value is what we get hold of
-- | * We turn the "end on click" widget we created above into a signal,
-- |   but it still ends after an event.
counters__ :: Array Int -> Signal (Array Int)
counters__ xs = do
  display <| Node.text <| show { content: xs, isSorted: isSorted xs }
  loop xs counters

-- | * `loop` has to be there to feed the start value
-- |   and to feed next results into the signal again!
main_ :: Widget (Array Int)
main_ = dynamic <| fix [ 1, 2, 3 ] counters_

---- Helpers -------------------------------------------------------------------
isSorted :: Array Int -> Bool
isSorted xs = case tail xs of
  Nothing -> true
  Just ys -> and <| zipWith (\x y -> x <= y) (view xs) ys
