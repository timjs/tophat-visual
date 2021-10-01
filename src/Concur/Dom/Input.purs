module Concur.Dom.Input
  -- # Basic
  ( button
  , group
  , switch
  , checkbox
  , entry
  , area
  , picker
  , picker_generic
  -- # Extra
  , addon
  , tooltip
  , popover
  , card
  , Action(..)
  , chip
  ) where

import Preload

import Concur.Dom (Widget, block, blockWithData, intValue, stringValue)
import Concur.Dom.Attr as Attr
import Concur.Dom.Node as Node
import Concur.Dom.Style (Kind, Button, Size, Position)
import Concur.Dom.Text as Text
import Data.Array as Array

---- Buttons -------------------------------------------------------------------
-- * [normal|radio|image]button

button :: Button -> Kind -> Size -> String -> Widget Unit
button but kind size label = do
  result <-
    Node.input
      [ Attr._type "button"
      , Attr.classes [ "btn", "btn-" ++ show but, "btn-" ++ show kind, "btn-" ++ show size ]
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
        button but kind size label

group :: forall a. Array (Widget a) -> Widget a
group = block [ "btn-group", "btn-group-block" ]

---- Toggles -------------------------------------------------------------------
-- * toggle/switch/checkbox

toggle :: Toggle -> String -> Bool -> Widget Bool
toggle typ label checked = do
  block [ "form-group" ]
    [ Node.label [ Attr.classes [ "form-" ++ show typ ] ]
        [ Node.input
            [ Attr._type "checkbox"
            , Attr.checked checked
            , Attr.onInput ->> unit
            ]
        , Node.i [ Attr.classes [ "form-icon" ] ] [ Node.text label ]
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
      [ Attr.classes [ "form-select" ]
      , Attr.onChange
      , Attr.defaultValue default
      ]
      (map go groups)
  done <| stringValue result
  where
  go (label ~ options) =
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

-- | Text entry showing `placeholder` when `value` is empty.
-- |
-- | Commits when hitting the enter-key.
entry :: Size -> String -> String -> Widget String
entry size placeholder value = do
  result <-
    Node.input
      [ Attr._type "text"
      , Attr.classes [ "form-input", "input-" ++ show size ]
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

area :: Int -> String -> String -> Widget String
area lines placeholder value = do
  result <-
    Node.textarea
      [ Attr.classes [ "form-input" ]
      , Attr.rows lines
      , Attr.placeholder placeholder
      , Attr.onInput >-> Left
      , Attr.onKeyDown >-> Right
      ]
      [ Node.text value ]
  case result of
    Left event -> area lines placeholder (stringValue event)
    Right key ->
      if Attr.isEnterEvent key then
        done value
      else
        area lines placeholder value

---- Extras --------------------------------------------------------------------

addon :: forall a. Size -> Widget a -> Widget a -> Widget a
addon size contents widget =
  block [ "input-group" ]
    [ block [ "input-group-addon", "addon-" ++ show size ]
        [ contents ]
    , widget
    ]

tooltip :: forall a. Position -> String -> Widget a -> Widget a
tooltip pos text widget = blockWithData [ "tooltip", "tooltip-" ++ show pos ]
  { tooltip: text }
  [ widget ]

popover :: forall a. Position -> Widget a -> Widget a -> Widget a
popover pos contents widget =
  block [ "popover", "popover-" ++ show pos ]
    [ widget
    , block [ "popover-container" ]
        [ contents ]
    ]

card :: forall a. Array (Widget a) -> Array (Widget a) -> Array (Widget a) -> Widget a
card header body footer =
  block [ "card" ]
    [ if Array.null header then empty else block [ "card-header" ] header
    -- [ Text.subsubhead title
    -- , Text.text subtitle
    -- ]
    , block [ "card-body" ] body
    , if Array.null footer then empty else block [ "card-footer" ] footer
    ]

data Action = Add | Remove | None

instance Show Action where
  show = case _ of
    Add -> "add"
    Remove -> "clear"
    None -> "none"

chip :: Kind -> Action -> String -> Widget Unit
-- chip text = block [ "label", "label-sm", "label-rounded", "label-primary" ] [ Text.text text ]
chip kind action text = block [ "chip", "chip-" ++ show kind ]
  [ Text.text text
  , case action of
      None -> empty
      _ -> Node.a
        [ Attr.classes [ "btn", "btn-" ++ show action ]
        , Attr.role "button"
        , Attr.onClick ->> unit
        ]
        []
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
