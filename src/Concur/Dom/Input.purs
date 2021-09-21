module Concur.Dom.Input
  -- # Basic
  ( button
  , switch
  , checkbox
  , entry
  , picker
  -- # Extr
  , addon
  , tooltip
  , popover
  ) where

import Preload
import Concur.Dom (Widget, block, inline, intValue, stringValue)
import Concur.Dom.Attr as Attr
import Concur.Dom.Node as Node
import Concur.Dom.Style (Kind, Size, Position)
import Data.Array as Array

---- Buttons -------------------------------------------------------------------
-- * [normal|radio|image]button

button :: Kind -> Size -> String -> Widget Unit
button kind size label = do
  result <-
    Node.input
      [ Attr._type "button"
      , Attr.className "btn"
      , Attr.className ("btn-" ++ show kind)
      , Attr.className ("btn-" ++ show size)
      , Attr.value label
      , Attr.onClick ->> Nothing
      , Attr.onKeyDown >-> Just
      ]
  case result of
    Nothing -> done unit
    Just key ->
      if Attr.isEnterEvent key then
        done unit
      else
        button kind size label

-- group :: forall a. Array (Widget a) -> Widget a
-- group = block [ "btn-group", "btn-group-block" ]

---- Toggles -------------------------------------------------------------------
-- * toggle/switch/checkbox

toggle :: Toggle -> String -> Bool -> Widget Bool
toggle typ label checked = do
  block [ "form-group" ]
    [ Node.label [ Attr.className ("form-" ++ show typ) ]
        [ Node.input
            [ Attr._type "checkbox"
            , Attr.checked checked
            , Attr.onInput ->> unit
            ]
        , Node.i [ Attr.className "form-icon" ] [ Node.text label ]
        ]
    ]
  done (not checked)

switch :: String -> Bool -> Widget Bool
switch = toggle Switch

checkbox :: String -> Boolean -> Widget Boolean
checkbox = toggle Checkbox

data Toggle = Switch | Checkbox

instance Show Toggle where
  show = case _ of
    Switch -> "switch"
    Checkbox -> "checkbox"

---- Pickers -------------------------------------------------------------------
-- * [normal|date|time|file|color|...]picker

picker_generic :: forall a. Show a => Eq a => Array a -> a -> Widget a
picker_generic options default = do
  result <-
    Node.select
      [ Attr.onChange
      , Attr.defaultValue (show default)
      ]
      (Array.mapWithIndex go options)
  case intValue result >>= Array.index options of
    Just x -> done x
    Nothing -> picker_generic options default
  where
  go i x =
    Node.option [ Attr.value <| show i ]
      [ Node.text <| show x ]

-- | Preconditions:
-- | * all elements are unique
picker :: Assoc String (Array String) -> String -> Widget String
picker groups default = do
  result <-
    Node.select
      [ Attr.className "form-select"
      , Attr.onChange
      , Attr.defaultValue default
      ]
      (map go groups)
  done <| stringValue result
  where
  go (label ~> options) =
    Node.optgroup [ Attr.label label ]
      (map go' options)
  go' option =
    Node.option [ Attr.value option ]
      [ Node.text option ]

-- multipicker :: Array (String * Bool) -> Widget (Array (String * Bool))
-- multipicker items = do
--   result <-
--     Node.select
--       [Attr.onChange]
--       (map go items)
--   done <| stringValue result

---- Entries -------------------------------------------------------------------

-- | Text entry showing `value` and `placeholder` when `value` is empty.
-- |
-- | Commits when hitting the enter-key.
entry :: Size -> String -> String -> Widget String
entry size placeholder value = do
  result <-
    Node.input
      [ Attr._type "text"
      , Attr.className "form-input"
      , Attr.className ("input-" ++ show size)
      -- , Attr.autoFocus true
      -- , Attr.label label
      , Attr.defaultValue value
      , Attr.placeholder placeholder
      , Attr.onInput >-> Left
      , Attr.onKeyDown >-> Right
      ]
  case result of
    Left event -> entry size placeholder (stringValue event)
    Right key ->
      if Attr.isEnterEvent key then
        done value
      else
        entry size placeholder value

---- Extras --------------------------------------------------------------------

addon :: forall a. Size -> Widget a -> Widget a -> Widget a
addon size contents widget =
  block [ "input-group" ]
    [ inline [ "input-group-addon", "addon-" ++ show size ]
        [ contents ]
    , widget
    ]

tooltip :: forall a. Position -> String -> Widget a -> Widget a
tooltip pos text widget = Node.span
  [ Attr.className "tooltip"
  , Attr.className ("tooltip-" ++ show pos)
  , Attr._data { dataTooltip: text }
  ]
  [ widget ]

popover :: forall a. Position -> Widget a -> Widget a -> Widget a
popover pos contents widget =
  block [ "poppover", "popover-" ++ show pos ]
    [ widget
    , block [ "popover-container" ]
        [ contents ]
    ]

---- Derived -------------------------------------------------------------------

-- textbox :: String -> Widget String
-- textbox value = do
--   Node.div [ void Attr.onClick ] [ Node.text value ]
--   new <- Node.div' [ entry value value, button "Cancel" ->> value ]
--   --XXX inconsistent formatting when compared to `case-of`...
--   done
--     if new == "" then
--       value
--     else
--       new

---- Missing -------------------------------------------------------------------
-- * slider, stepper
