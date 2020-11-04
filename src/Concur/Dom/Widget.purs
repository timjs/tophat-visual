module Concur.Dom.Widget
  -- # Inputs
  ( button
  , checkbox
  , inputbox
  -- # Values
  , stringValue
  , intValue
  , floatValue
  ) where

import Preload
import Concur (Widget)
import Concur.Dom (Dom)
import Concur.Dom.Attr as Attr
import Concur.Dom.Node as Node
import Data.Int as Int
import Data.Number as Number
import React.SyntheticEvent as React

---- Text ----------------------------------------------------------------------
-- title = h1
-- subtitle = h2
-- heading = h3
-- subheading = h4
-- emph
---- Input ---------------------------------------------------------------------
button :: String -> Widget Dom Unit
button label = do
  result <- Node.button [ Nothing -|| Attr.onClick, Just <|| Attr.onKeyDown ] [ Node.text label ]
  case result of
    Nothing -> done unit
    Just key ->
      if Attr.isEnterEvent key then
        done unit
      else
        button label

checkbox :: String -> Bool -> Widget Dom Bool
checkbox label checked = do
  Node.div'
    [ Node.input [ Attr._type "checkbox", Attr.checked checked, unit -|| Attr.onInput ]
    , Node.label [] [ Node.text label ]
    ]
  done (not checked)

inputbox :: String -> String -> String -> Widget Dom String
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

---- Target values -------------------------------------------------------------
stringValue :: forall a. React.SyntheticEvent_ a -> String
stringValue = Attr.unsafeTargetValue

floatValue :: forall a. React.SyntheticEvent_ a -> Maybe Number
floatValue = stringValue >> Number.fromString

intValue :: forall a. React.SyntheticEvent_ a -> Maybe Int
intValue e = floatValue e ||> Int.floor
