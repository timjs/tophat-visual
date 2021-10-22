module Concur.Dom.Text
  -- # Text
  ( text
  , gray
  , code
  -- # Heads
  , title
  , subtitle
  , head
  , subhead
  , subsubhead
  -- Lists
  , bullets
  , numbers
  , item
  ) where

import Preload

import Concur.Dom (Widget, block)
import Concur.Dom.Node as Node
import Concur.Dom.Attr as Attr

---- Text ----------------------------------------------------------------------
-- type TextStyle r
--   = { face :: Face
--     , size :: Int
--     , shape :: Shape
--     , weight :: Weight
--     , decoration :: Line
--     | r
--     }

text :: forall a. String -> Widget a
text = Node.text

code :: forall a. String -> String -> Widget a
code lang str = Node.pre
  [ Attr.classes [ "code" ]
  , Attr._data { lang }
  ]
  [ Node.code [] [ text str ] ]

gray :: forall a. String -> Widget a
gray s = block [ "text-gray" ] [ text s ]

-- lines :: forall a. Array String -> Widget a
-- lines xs = Layout.column <| map Node.text xs

---- Heads ----------------------------------------------------------------------
-- title = h1
-- subtitle = h2
-- heading = h3
-- subheading = h4
-- emph

title :: forall a. String -> Widget a
title s = Node.h1' [ text s ]

subtitle :: forall a. String -> Widget a
subtitle s = Node.h2' [ text s ]

head :: forall a. String -> Widget a
head s = Node.h3' [ text s ]

subhead :: forall a. String -> Widget a
subhead s = Node.h4' [ text s ]

subsubhead :: forall a. String -> Widget a
subsubhead s = Node.h5' [ text s ]

---- Lists ---------------------------------------------------------------------

bullets :: forall a. Array (Widget a) -> Widget a
bullets = Node.ul'

numbers :: forall a. Array (Widget a) -> Widget a
numbers = Node.ol'

item :: forall a. Widget a -> Widget a
item x = Node.li' [ x ]