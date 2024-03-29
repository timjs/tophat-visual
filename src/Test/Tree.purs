module Test.Tree where

import Preload

import Concur (dynamic, step, loop, fix, wither)
import Concur.Dom (Widget, Signal)
import Concur.Dom.Attr as Attr
import Concur.Dom.Input as Input
import Concur.Dom.Node as Node
import Concur.Dom.Style as Style
import Concur.Dom.Text as Text
import Data.Array as Array

---- Data ----------------------------------------------------------------------

data Tree a
  = Tree a (Forest a)

type Forest a = Array (Tree a)

newTree :: Tree String
newTree = Tree "New heading" []

initTree :: Tree String
initTree =
  Tree
    "Double click to edit me"
    [ Tree "Or use the 'delete' button to delete me" []
    , Tree "Or use the 'new' button to add a sub node" []
    ]

---- Widgets -------------------------------------------------------------------
{-
  Widgets:
  * Everything you lay out on a page using `Node` tags is *parallel composition* or *composition in space*.
  * Everything you do inside the monad is *stepwise composition* or *composition in time*.
-}

data Action
  = Rename String
  | Create
  | Delete
  | Modify (Forest String)

tree :: Tree String -> Widget (Maybe (Tree String))
tree (Tree name children) = do
  result <- Text.bullets
    [ Style.element []
        [ Rename <-< title name
        , Create <<- Input.button Style.Default Style.Normal Style.Medium "Create"
        , Delete <<- Input.button Style.Default Style.Normal Style.Medium "Delete"
        , Modify <-< wither tree children
        ]
    ]
  done <| case result of
    Rename name' -> Just <| Tree name' children
    Create -> Just <| Tree name (Array.snoc children newTree)
    Delete -> Nothing
    Modify children' -> Just <| Tree name children'

title :: String -> Widget String
title old = do
  Style.element [ void Attr.onDoubleClick ] [ Text.subhead old ]
  new <- Style.element [] [ Input.entry Style.Medium old old, Input.button Style.Default Style.Normal Style.Medium "Cancel" ->> old ]
  done <| if new == "" then old else new

render :: forall a. Tree String -> Widget a
render t = do
  mt <- tree t
  case mt of
    Nothing -> Text.subhead "Tree deleted"
    Just t' -> render t'

-- | Main tree widget
--
-- * Draws the tree.
-- * After an event, the widget disapears.
main :: Widget (Maybe (Tree String))
main = render initTree

---- Signals -------------------------------------------------------------------

tree_ :: Tree String -> Signal (Maybe (Tree String))
tree_ (Tree name children) =
  Node.li_ [] do
    name' <- loop name title
    deleting <- step false (Input.button Style.Default Style.Normal Style.Medium "Delete" ->> done true)
    if deleting then
      done Nothing
    else do
      child' <- step Nothing (Input.button Style.Default Style.Normal Style.Medium "New" ->> done (Just newTree))
      let
        children' = case child' of
          Nothing -> children
          Just new -> Array.snoc children new
      children'' <- traverse tree_ children' |> map Array.catMaybes |> Node.ul_ []
      done <| Just (Tree name' children'')

render_ :: Maybe (Tree String) -> Signal (Maybe (Tree String))
render_ = case _ of
  Nothing -> done Nothing
  Just t -> tree_ t

main_ :: Widget (Maybe (Tree String))
main_ = dynamic <| fix (Just initTree) render_

{-
tree' :: Tree String -> Widget  (Tree String)
tree' (Tree name children) = do
  action <-
    Node.ul'
      [ Node.li'
          [ Rename <-< title name
          , Create <<- Input.button "Create"
          , Delete <<- Input.button "Delete"
          , Modify <-< forest children
          ]
      ]
  done
    <| case action of
        Rename name' -> Tree name' children
        Create -> Tree name (Array.snoc children newTree)
        Delete -> newTree
        Modify children' -> Tree name children'

tree'' :: Tree String -> Signal  (Tree String)
tree'' t = loop t tree'

forest :: Forest String -> Widget  (Forest String)
forest ts = dynamic <| loop ts forest'

forest' :: Forest String -> Signal  (Forest String)
forest' ts = traverse tree'' ts

main' :: Widget  (Tree String)
main' = dynamic <| loop initTree tree''
-}
