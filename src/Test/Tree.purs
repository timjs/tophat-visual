module Test.Tree where

import Preload
import Concur (Widget, list)
import Concur.Dom (Dom)
import Concur.Dom.Node as Node
import Concur.Dom.Attr as Attr
import Concur.Dom.Elem as Elem
import Data.Array as Array

---- Data ----------------------------------------------------------------------
data Tree a
  = Tree a (Forest a)

type Forest a
  = Array (Tree a)

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
  - Everything you lay out on a page using `Node` tags is *parallel* composition or composition *in space*.
  - Everything you do inside the monad is *stepwise* composition or composition *in time*.
-}
data Action
  = Rename String
  | Create (Tree String)
  | Delete
  | Modify (Forest String)

view :: Maybe (Tree String) -> Widget Dom (Maybe (Tree String))
view = case _ of
  Nothing -> Just <|| create
  Just tree -> view' tree
  where
  view' (Tree title children) = do
    result <-
      Node.ul'
        [ Node.li'
            [ Rename <|| rename title
            , Create <|| create
            , Delete -|| delete
            , Modify <|| list view' children
            ]
        ]
    done
      <| case result of
          Rename title' -> Just <| Tree title' children
          Create tree' -> Just <| Tree title (Array.cons tree' children)
          Delete -> Nothing
          Modify children' -> Just <| Tree title children'

rename :: String -> Widget Dom String
rename title = do
  Node.h5 [ void Attr.onDoubleClick ] [ Node.text title ]
  renamed <- Node.div' [ Elem.inputbox title, Node.button [ title -|| Attr.onClick ] [ Node.text "Cancel" ] ]
  done
    <| if renamed == "" then
        title
      else
        renamed

create :: Widget Dom (Tree String)
create = do
  Node.button [ void Attr.onClick ] [ Node.text "New" ]
  done <| Tree "New Heading" []

delete :: Widget Dom Unit
delete = do
  Node.button [ void Attr.onClick ] [ Node.text "Delete" ]
