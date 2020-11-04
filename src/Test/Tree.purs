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

view :: Maybe (Tree String) -> Widget Dom (Maybe (Tree String))
view = case _ of
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
          Create -> Just <| Tree name (Array.cons empty children)
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
main = view (Just init)

---- Signals -------------------------------------------------------------------
treeWidget :: Tree String -> Widget Dom (Tree String)
treeWidget (Tree name children) = do
  action <-
    Node.ul'
      [ Node.li'
          [ Rename <|| titleWidget name
          , Create -|| Widget.button "Create"
          , Delete -|| Widget.button "Delete"
          -- , Modify <|| forestWidget children
          ]
      ]
  done
    <| case action of
        Rename name' -> Tree name' children
        Create -> Tree name (Array.cons empty children)
        Delete -> empty
        Modify children' -> Tree name children'

treeSignal :: Tree String -> Signal Dom (Tree String)
treeSignal t = repeat t treeWidget

forestWidget :: Forest String -> Widget Dom (Forest String)
forestWidget ts = dynamic <| forestSignal ts

forestSignal :: Forest String -> Signal Dom (Forest String)
forestSignal ts = traverse treeSignal ts

renderTree' :: Tree String -> Signal Dom (Tree String)
renderTree' t = repeat t treeWidget

renderTree :: Tree String -> Widget Dom (Tree String)
renderTree t = dynamic <| loop t renderTree'
