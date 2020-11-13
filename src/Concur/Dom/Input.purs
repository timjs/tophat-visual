module Concur.Dom.Input
  -- # Inputs
  ( button
  , checkbox
  , textbox
  , inputbox
  , selectionbox
  ) where

import Preload
import Concur.Dom (Widget, intValue, stringValue)
import Concur.Dom.Attr as Attr
import Concur.Dom.Node as Node
import Data.Array as Array

---- Input ---------------------------------------------------------------------
button :: String -> Widget Unit
button label = do
  result <- Node.button [ Nothing -|| Attr.onClick, Just <|| Attr.onKeyDown ] [ Node.text label ]
  case result of
    Nothing -> done unit
    Just key ->
      if Attr.isEnterEvent key then
        done unit
      else
        button label

checkbox :: String -> Bool -> Widget Bool
checkbox label checked = do
  Node.div'
    [ Node.input [ Attr._type "checkbox", Attr.checked checked, unit -|| Attr.onInput ]
    , Node.label [] [ Node.text label ]
    ]
  done (not checked)

textbox :: String -> Widget String
textbox value = do
  Node.div [ void Attr.onClick ] [ Node.text value ]
  new <- Node.div' [ inputbox "label" value value, button "Cancel" ||- value ]
  --XXX inconsistent formatting when compared to `case-of`...
  done
    if new == "" then
      value
    else
      new

inputbox :: String -> String -> String -> Widget String
inputbox label placeholder value = do
  result <-
    Node.input
      [ Attr.autoFocus true
      , Attr._type "text"
      , Attr.label label
      , Attr.value value
      , Attr.placeholder placeholder
      , Attr.style
          { boxSizing: "border-box"
          , backgroundColor: "transparent"
          , border: "none"
          , borderBottom: "1px solid black"
          , width: "auto"
          , height: "auto"
          }
      , Left <|| Attr.onInput
      , Right <|| Attr.onKeyDown
      ]
  case result of
    Left event -> inputbox label placeholder (stringValue event)
    Right key ->
      if Attr.isEnterEvent key then
        done value
      else
        inputbox label placeholder value

selectionbox :: forall a. Show a => String -> Array a -> Widget a
selectionbox label options = do
  r <-
    Node.select
      [ Attr.label label
      , Attr.onChange
      ]
      (Array.mapWithIndex go options)
  case intValue r |= Array.index options of
    Just x -> done x
    Nothing -> selectionbox label options
  where
  -- go :: Show a => Int -> a -> Widget a
  go i x = Node.option [ Attr.value <| show i ] [ Node.text <| show x ]
