module Test.Counter where

import Preload
import Concur
import Concur.Dom (Dom)
import Concur.Dom.Html as Html
import Concur.Dom.Attributes as Attr
import Data.Array (zipWith, tail)

---- Widgets -------------------------------------------------------------------
counter :: Int -> Widget Dom Int
counter n = do
  Html.div'
    [ n + 1 -|| Html.button [ Attr.onClick ] [ Html.text "+" ]
    , Html.text <| show n
    , n - 1 -|| Html.button [ Attr.onClick ] [ Html.text "-" ]
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
  display <| Html.text <| show { content: xs, sorted: sorted xs }
  traverse counter' xs

main' :: Widget Dom (Array Int)
main' = dynamic <| loop [ 1, 2, 3 ] counters'

---- Helpers -------------------------------------------------------------------
sorted :: Array Int -> Bool
sorted xs = case tail xs of
  Nothing -> true
  Just ys -> and <| zipWith (\x y -> x <= y) xs ys
