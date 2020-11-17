module Concur.Dom.Input
  -- # Inputs
  ( button
  , toggle
  , entry
  , picker
  , picker'
  ) where

import Preload
import Concur.Dom (Widget, intValue, stringValue)
import Concur.Dom.Attr as Attr
import Concur.Dom.Node as Node
import Data.Array as Array

-- [normal|radio|image]button, toggle/switch/checkbox, slider, stepper, [normal|date|time|file|color|...]picker, entry/entry
---- Controls ------------------------------------------------------------------
button :: String -> Widget Unit
button label = do
  result <-
    Node.input
      [ Attr._type "button"
      , Attr.value label
      , Attr.onClick ||- Nothing
      , Attr.onKeyDown ||> Just
      ]
  case result of
    Nothing -> done unit
    Just key ->
      if Attr.isEnterEvent key then
        done unit
      else
        button label

toggle :: Bool -> Widget Bool
toggle checked = do
  Node.input
    [ Attr._type "checkbox"
    , Attr.checked checked
    , Attr.onInput ||- unit
    ]
  done (not checked)

picker :: forall a. Show a => Eq a => Array a -> a -> Widget a
picker options default = do
  result <-
    Node.select
      [ Attr.onChange ]
      (Array.mapWithIndex go options)
  case intValue result |= Array.index options of
    Just x -> done x
    Nothing -> picker options default
  where
  -- go :: Show a => Int -> a -> Widget a
  go i x =
    Node.option
      [ Attr.selected (x == default)
      , Attr.value <| show i
      ]
      [ Node.text <| show x ]

-- | Preconditions:
-- | * all elements are unique
picker' :: Array (String ** Array String) -> String -> Widget String
picker' groups default = do
  result <-
    Node.select
      [ Attr.onChange ]
      (map go groups)
  done <| stringValue result
  where
  go (label ** options) =
    Node.optgroup [ Attr.label label ]
      (map go' options)

  go' option =
    Node.option
      [ Attr.selected (option == default)
      , Attr.value option
      ]
      [ Node.text option ]

-- | Text entry showing `value` and `placeholder` when `value` is empty.
-- |
-- | Commits when hitting the enter-key.
entry :: String -> String -> Widget String
entry placeholder value = do
  result <-
    Node.input
      [ Attr._type "text"
      -- , Attr.autoFocus true
      -- , Attr.label label
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
      , Attr.onInput ||> Left
      , Attr.onKeyDown ||> Right
      ]
  case result of
    Left event -> entry placeholder (stringValue event)
    Right key ->
      if Attr.isEnterEvent key then
        done value
      else
        entry placeholder value

---- Derived -------------------------------------------------------------------
textbox :: String -> Widget String
textbox value = do
  Node.div [ void Attr.onClick ] [ Node.text value ]
  new <- Node.div' [ entry value value, button "Cancel" ||- value ]
  --XXX inconsistent formatting when compared to `case-of`...
  done
    if new == "" then
      value
    else
      new
