module Concur.Replica.Widgets
  ( Html,
    Attr,

    -- * Static
    text,
    row,
    column,

    -- * Inputs
    button,
    checkbox,
    inputbox,

    -- * Values
    stringValue,
    intValue,
    floatValue,
  )
where

import Concur.Core (Widget)
import qualified Concur.Replica as Html
import Concur.Replica.DOM.Events (BaseEvent)

-- Types -----------------------------------------------------------------------

type Html = Html.HTML

type Attr = Html.Props

-- Static ----------------------------------------------------------------------

text :: Text -> Widget Html void
text = Html.text

row :: List (Attr a) -> List (Widget Html a) -> Widget Html a
row = Html.div

column :: List (Attr a) -> List (Widget Html a) -> Widget Html a
column = Html.div

-- Inputs ----------------------------------------------------------------------

button :: Text -> Widget Html ()
button label = do
  result <- Html.button [const Nothing <|| Html.onClick, Just <|| Html.onKeyDown] [Html.text label]
  case result of
    Nothing -> pure ()
    Just event ->
      if Html.kbdKey event == "Enter"
        then pure ()
        else button label

checkbox :: Text -> Bool -> Widget Html Bool
checkbox label checked = do
  Html.div
    []
    [ Html.input [Html.type_ "checkbox", Html.checked checked, const () <|| Html.onInput],
      Html.label [] [Html.text label]
    ]
  checkbox label (not checked)

inputbox :: Text -> Widget Html Text
inputbox value = do
  result <-
    Html.input
      [ Html.autofocus True,
        Html.type_ "text",
        Html.value value,
        Html.placeholder value,
        Left <|| Html.onInput,
        Right <|| Html.onKeyDown
      ]
  case result of
    Left event -> inputbox (Html.targetValue <| Html.target event)
    Right e ->
      if Html.kbdKey e == "Enter"
        then pure value
        else inputbox value

-- Target values ---------------------------------------------------------------

stringValue :: BaseEvent -> Text
stringValue = Html.target >> Html.targetValue

intValue :: BaseEvent -> Maybe Int
intValue = stringValue >> read

floatValue :: BaseEvent -> Maybe Double
floatValue = stringValue >> read
