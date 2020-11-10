module Concur.Dom.Input
  -- # Inputs
  ( button
  , checkbox
  , inputbox
  ) where

import Preload
import Concur.Dom (Widget, stringValue)
import Concur.Dom.Attr as Attr
import Concur.Dom.Node as Node

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

inputbox :: String -> String -> String -> Widget String
inputbox label placeholder value = do
  result <-
    Node.input
      [ Attr.autoFocus true
      , Attr._type "text"
      , Attr.label label
      , Attr.value value
      , Attr.placeholder placeholder
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