module Concur.Dom.Style
  -- # Types
  ( Kind(..)
  , Size(..)
  , Orientation(..)
  , Position(..)
  , Style(..)
  , Stroke(..)
  -- # Layout
  , column
  , row
  , branch
  , group
  , place
  , divider
  -- # Shapes
  , line
  , triangle
  , chip
  ) where

import Preload

import Concur.Dom (Widget, block, blockWithData)
import Concur.Dom.Text as Text

---- Types ---------------------------------------------------------------------

data Kind = Default | Primary | Link | Success | Warning | Error

data Size = Large | Medium | Small

-- data Anchor = North | East | South | West

data Orientation = Horizontal | Vertical

data Position = Above | Below | Before | After

data Style = Outlined | Filled

data Stroke = Dotted | Dashed | Solid | Double

instance Show Kind where
  show = case _ of
    Default -> "default" -- NOTE: not named in Spectre
    Primary -> "primary"
    Link -> "link"
    Success -> "success"
    Warning -> "warning"
    Error -> "error"

instance Show Size where
  show = case _ of
    Large -> "lg"
    Medium -> "md" -- NOTE: not named in Spectre
    Small -> "sm"

instance Show Orientation where
  show = case _ of
    Horizontal -> "hori" -- NOTE: not named in Spectre
    Vertical -> "vert"

instance Show Position where
  show = case _ of
    Above -> "top"
    Below -> "bottom"
    Before -> "left"
    After -> "right"

instance Show Style where
  show = case _ of
    Outlined -> "outlined" --TODO: was open
    Filled -> "filled" --TODO: was closed

instance Show Stroke where
  show = case _ of
    Dotted -> "dotted"
    Dashed -> "dashed"
    Solid -> "solid"
    Double -> "double"

---- Layout --------------------------------------------------------------------

column :: forall a. Array (Widget a) -> Widget a
column = block [ "layout-column" ]

row :: forall a. Array (Widget a) -> Widget a
row = block [ "layout-row" ]

branch :: forall a. Array (Widget a) -> Widget a
branch = block [ "layout-row", "layout-branch" ]

group :: forall a. Stroke -> Array (Widget a) -> Widget a
group stroke = block [ "layout-row", "layout-group", "stroke-" ++ show stroke ]

place :: forall a. Position -> Widget a -> Widget a
place pos widget = block [ "layout-side", "side-" ++ show pos ] [ widget ]

divider :: forall a. Orientation -> Maybe String -> Widget a
divider orient text = blockWithData
  [ "divider-" ++ show orient, if isJust text then "text-center" else "" ]
  { dataContent: text ?? "" }
  []

---- Shapes --------------------------------------------------------------------

line :: forall a. Stroke -> Widget a -> Widget a
line stroke contents = block [ "shape-line", "stroke-" ++ show stroke ] [ contents ]

triangle :: forall a. Style -> Widget a -> Widget a
triangle style _contents = block [ "shape-triangle", "style-" ++ show style ] []

chip :: forall a. Kind -> String -> Widget a
-- chip text = block [ "label", "label-sm", "label-rounded", "label-primary" ] [ Text.text text ]
chip kind text = block [ "chip", "chip-" ++ show kind ] [ Text.text text ]
