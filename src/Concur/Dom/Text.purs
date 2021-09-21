module Concur.Dom.Text
  ( activate
  -- # Text
  , text
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
  ) where

import Preload
import Concur.Dom (Widget, Attr)
import Concur.Dom.Node as Node

activate :: forall a. Array (Attr a) -> Widget a -> Widget a
activate = Node.div_

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

code :: forall a. String -> Widget a
code s = Node.pre [] [ Node.text s ]

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
bullets xs = Node.ul' (map item xs)

numbers :: forall a. Array (Widget a) -> Widget a
numbers xs = Node.ol' (map item xs)

item :: forall a. Widget a -> Widget a
item x = Node.li' [ x ]