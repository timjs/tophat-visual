module Concur.Widgets
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
  ) where

import Preload
import Concur.Core as Core
import Concur.React as Html
import Concur.React.DOM as Html
import Concur.React.Props hiding (label) as Html
import Data.Int (floor)
import Global (readInt, readFloat)
import React.SyntheticEvent as React

---- Types ---------------------------------------------------------------------
type Widget
  = Core.Widget

type Html
  = Html.HTML

type Attr a
  = Html.ReactProps a

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
  result <- Html.button [ Nothing -|| Html.onClick, Just <|| Html.onKeyDown ] [ Html.text label ]
  case result of
    Nothing -> done unit
    Just key ->
      if Html.isEnterEvent key then
        done unit
      else
        button label

checkbox :: String -> Bool -> Widget Html Bool
checkbox label checked = do
  Html.div
    []
    [ Html.input [ Html._type "checkbox", Html.checked checked, unit -|| Html.onInput ]
    , Html.label [] [ Html.text label ]
    ]
  checkbox label (not checked)

inputbox :: String -> Widget Html String
inputbox value = do
  result <-
    Html.input
      [ Html.autoFocus true
      , Html._type "text"
      , Html.value value
      , Html.placeholder value
      , Left <|| Html.onInput
      , Right <|| Html.onKeyDown
      ]
  case result of
    Left event -> inputbox (stringValue event)
    Right key ->
      if Html.isEnterEvent key then
        done value
      else
        inputbox value

---- Target values -------------------------------------------------------------
stringValue :: forall a. React.SyntheticEvent_ a -> String
stringValue = Html.unsafeTargetValue

intValue :: forall a. React.SyntheticEvent_ a -> Int
intValue = stringValue >> readInt 10 >> floor

floatValue :: forall a. React.SyntheticEvent_ a -> Number
floatValue = stringValue >> readFloat
