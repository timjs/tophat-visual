module Concur.Forms
  -- # Static
  ( text
  , horizontal
  , vertical
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
import Concur (Widget, class ShiftMap, class MultiAlternative)
import Concur.Dom (Dom, Attr)
import Concur.Dom.Attributes as Attr
import Concur.Dom.Tags as Tag
import Data.Int as Int
import Data.Number as Number
import React.SyntheticEvent as React

---- Static --------------------------------------------------------------------
text :: forall a. String -> Widget Dom a
text = Tag.text_

divProps :: forall a. Array (Attr a)
divProps = [ Attr.display "flex", Attr.align_items "center", Attr.justify_content "center" ]

horizontal :: forall m a. MultiAlternative m => ShiftMap (Widget Dom) m => Array (m a) -> m a
horizontal = Tag.div <| [ Attr.flex_direction "row" ] ++ divProps

vertical :: forall m a. MultiAlternative m => ShiftMap (Widget Dom) m => Array (m a) -> m a
vertical = Tag.div <| [ Attr.flex_direction "column" ] ++ divProps

---- Inputs --------------------------------------------------------------------
button :: String -> Widget Dom Unit
button label = do
  result <- Tag.button [ Nothing -|| Attr.onClick, Just <|| Attr.onKeyDown ] [ Tag.text_ label ]
  case result of
    Nothing -> done unit
    Just key ->
      if Attr.isEnterEvent key then
        done unit
      else
        button label

checkbox :: String -> Bool -> Widget Dom Bool
checkbox label checked = do
  Tag.div
    []
    [ Tag.input [ Attr._type "checkbox", Attr.checked checked, unit -|| Attr.onInput ]
    , Tag.label [] [ Tag.text_ label ]
    ]
  checkbox label (not checked)

inputbox :: String -> Widget Dom String
inputbox value = do
  result <-
    Tag.input
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
