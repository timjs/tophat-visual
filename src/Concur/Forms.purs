module Concur.Forms
  ( Html
  , Attr
  -- # Static
  , text
  , row
  , column
  -- # Inputs
  , button
  , checkbox
  , inputbox
  -- # Values
  , stringValue
  , intValue
  , floatValue
  -- # Running
  , runWidgetInDom
  ) where

import Preload
import Concur (Widget)
import Concur.React (HTML)
import Concur.React.DOM as Html
import Concur.React.Props as Attr
import Concur.React.Run as Run
import Data.Int as Int
import Data.Number as Number
import React.SyntheticEvent as React

---- Types ---------------------------------------------------------------------
type Html
  = HTML

type Attr a
  = Attr.ReactProps a

---- Static --------------------------------------------------------------------
text :: forall a. String -> Widget Html a
text = Html.text

row :: forall a. Array (Attr a) -> Array (Widget Html a) -> Widget Html a
row = Html.div

column :: forall a. Array (Attr a) -> Array (Widget Html a) -> Widget Html a
column = Html.div

---- Inputs --------------------------------------------------------------------
button :: String -> Widget Html Unit
button label = do
  result <- Html.button [ Nothing -|| Attr.onClick, Just <|| Attr.onKeyDown ] [ Html.text label ]
  case result of
    Nothing -> done unit
    Just key ->
      if Attr.isEnterEvent key then
        done unit
      else
        button label

checkbox :: String -> Bool -> Widget Html Bool
checkbox label checked = do
  Html.div
    []
    [ Html.input [ Attr._type "checkbox", Attr.checked checked, unit -|| Attr.onInput ]
    , Html.label [] [ Html.text label ]
    ]
  checkbox label (not checked)

inputbox :: String -> Widget Html String
inputbox value = do
  result <-
    Html.input
      [ Attr.autoFocus true
      , Attr._type "text"
      , Attr.value value
      , Attr.placeholder value
      , Left <|| Attr.onInput
      , Right <|| Attr.onKeyDown
      ]
  case result of
    Left event -> inputbox (stringValue event)
    Right key ->
      if Attr.isEnterEvent key then
        done value
      else
        inputbox value

---- Target values -------------------------------------------------------------
stringValue :: forall a. React.SyntheticEvent_ a -> String
stringValue = Attr.unsafeTargetValue

floatValue :: forall a. React.SyntheticEvent_ a -> Maybe Number
floatValue = stringValue >> Number.fromString

intValue :: forall a. React.SyntheticEvent_ a -> Maybe Int
intValue e = floatValue e ||> Int.floor

---- Running -------------------------------------------------------------------
runWidgetInDom :: forall a. String -> Widget Html a -> Effect Unit
runWidgetInDom = Run.runWidgetInDom
