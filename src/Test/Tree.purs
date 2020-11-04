module Test.Tree where

import Preload
import Concur (Widget, Signal, dynamic, loop, repeat, list)
import Concur.Dom (Dom)
import Concur.Dom.Attr as Attr
import Concur.Dom.Node as Node
import Concur.Dom.Widget as Widget
import Data.Array as Array

---- Data ----------------------------------------------------------------------
data Tree a
  = Tree a (Forest a)

type Forest a
  = Array (Tree a)

empty :: Tree String
empty = Tree "New heading" []

init :: Tree String
init =
  Tree
    "Double click to edit me"
    [ Tree "Or use the 'delete' button to delete me" []
    , Tree "Or use the 'new' button to add a sub node" []
    ]

---- Widgets -------------------------------------------------------------------
{-
  Widgets:
  - Everything you lay out on a page using `Node` tags is *parallel composition* or *composition in space*.
  - Everything you do inside the monad is *stepwise composition* or *composition in time*.
-}
data Action
  = Rename String
  | Create
  | Delete
  | Modify (Forest String)

viewMaybe :: Maybe (Tree String) -> Widget Dom (Maybe (Tree String))
viewMaybe = case _ of
  Nothing -> Just empty -|| Widget.button "Create"
  Just tree -> go tree
  where
  go (Tree name children) = do
    result <-
      Node.ul'
        [ Node.li'
            [ Rename <|| titleWidget name
            , Create -|| Widget.button "Create"
            , Delete -|| Widget.button "Delete"
            , Modify <|| list go children
            ]
        ]
    done
      <| case result of
          Rename name' -> Just <| Tree name' children
          Create -> Just <| Tree name (Array.snoc children empty)
          Delete -> Nothing
          Modify children' -> Just <| Tree name children'

titleWidget :: String -> Widget Dom String
titleWidget old = do
  Node.h5 [ void Attr.onDoubleClick ] [ Node.text old ]
  new <- Node.div' [ Widget.inputbox "label" old old, Widget.button "Cancel" ||- old ]
  done
    <| if new == "" then
        old
      else
        new

-- | Main tree widget
--
-- * Draws the tree.
-- * After an event, the widget disapears.
main :: Widget Dom (Maybe (Tree String))
main = viewMaybe (Just init)

---- Signals -------------------------------------------------------------------
-- | Only render a tree
--
-- * Just do it once!
tree :: Tree String -> Widget Dom (Tree String)
tree (Tree name children) = do
  action <-
    Node.ul'
      [ Node.li'
          [ Rename <|| titleWidget name
          , Create -|| Widget.button "Create"
          , Delete -|| Widget.button "Delete"
          , Modify <|| forest children
          ]
      ]
  done
    <| case action of
        Rename name' -> Tree name' children
        Create -> Tree name (Array.snoc children empty)
        Delete -> empty
        Modify children' -> Tree name children'

tree' :: Tree String -> Signal Dom (Tree String)
tree' t = repeat t tree

forest :: Forest String -> Widget Dom (Forest String)
forest ts = dynamic <| loop ts forest'

forest' :: Forest String -> Signal Dom (Forest String)
forest' ts = traverse tree' ts

main' :: Widget Dom (Tree String)
main' = dynamic <| loop init tree'
