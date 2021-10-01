module Concur.Dom.Style
  -- # Types
  ( Kind(..)
  , Button(..)
  , Size(..)
  , Orientation(..)
  , Position(..)
  , Style(..)
  , Stroke(..)
  -- # Layout
  , element
  , has
  , column
  , row
  , branch
  , group
  , place
  , divider
  -- # Shapes
  , line
  , triangle
  ) where

import Preload

import Concur.Dom (Widget, Attr, block, blockWithData, blockWithAttr)

---- Types ---------------------------------------------------------------------

data Kind = Normal | Primary | Secondary | Link | Success | Warning | Error

data Button = Default | Action

data Size = Large | Medium | Small

-- data Anchor = North | East | South | West

data Orientation = Horizontal | Vertical

data Position = Above | Below | Before | After

data Style = Outlined | Filled

data Stroke = Dotted | Dashed | Solid | Double

instance Show Kind where
  show = case _ of
    Normal -> "normal" -- NOTE: not named in Spectre
    Primary -> "primary"
    Secondary -> "secondary"
    Link -> "link"
    Success -> "success"
    Warning -> "warning"
    Error -> "error"

instance Show Button where
  show = case _ of
    Default -> "default" -- NOTE: not named in Spectre
    Action -> "action"

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

element :: forall a. Array (Attr a) -> Array (Widget a) -> Widget a
element = blockWithAttr

has :: forall a. Kind -> Array (Widget a) -> Widget a
has k = block [ "has-" ++ show k ]

column :: forall a. Array (Widget a) -> Widget a
column = block [ "layout-column" ]

row :: forall a. Array (Widget a) -> Widget a
row = block [ "layout-row" ]

branch :: forall a. Array (Widget a) -> Widget a
branch = block [ "layout-row", "layout-branch" ]

group :: forall a. Stroke -> Array (Widget a) -> Widget a
group stroke = block [ "layout-row", "layout-group", "stroke-" ++ show stroke ]

place :: forall a. Position -> Array (Widget a) -> Widget a
place pos = block [ "layout-side", "side-" ++ show pos ]

divider :: forall a. Orientation -> Maybe String -> Widget a
divider orient text = blockWithData
  [ "divider-" ++ show orient, if isJust text then "text-center" else "" ]
  { content: text ?? "" }
  []

---- Shapes --------------------------------------------------------------------

line :: forall a. Stroke -> Array (Widget a) -> Widget a
line stroke = block [ "shape-line", "stroke-" ++ show stroke ]

triangle :: forall a. Style -> Array (Widget a) -> Widget a
triangle style _contents = block [ "shape-triangle", "style-" ++ show style ] []
