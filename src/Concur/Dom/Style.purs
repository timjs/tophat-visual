module Concur.Dom.Style
  -- # Types
  ( Kind(..)
  , Size(..)
  , Orientation(..)
  , Position(..)
  -- # Layout
  , vertical
  , horizontal
  , place
  , divider
  -- # Shapes
  , line
  ) where

import Preload

import Concur.Dom (Widget, block, blockWithData)

---- Types ---------------------------------------------------------------------

data Kind = Default | Primary | Link | Success | Error

data Size = Large | Normal | Small

-- data Anchor = North | East | South | West

data Orientation = Horizontal | Vertical

data Position = Above | Below | Before | After

instance Show Kind where
  show = case _ of
    Default -> ""
    Primary -> "primary"
    Link -> "link"
    Success -> "success"
    Error -> "error"

instance Show Size where
  show = case _ of
    Large -> "lg"
    Normal -> ""
    Small -> "sm"

instance Show Orientation where
  show = case _ of
    Horizontal -> ""
    Vertical -> "-vert"

instance Show Position where
  show = case _ of
    Above -> "top"
    Below -> "bottom"
    Before -> "left"
    After -> "right"

---- Layout --------------------------------------------------------------------

vertical :: forall a. Array (Widget a) -> Widget a
vertical = block [ "layout-vertical" ]

horizontal :: forall a. Array (Widget a) -> Widget a
horizontal = block [ "layout-horizontal" ]

place :: forall a. Position -> Widget a -> Widget a
place pos widget = block [ "layout-side", show pos ] [ widget ]

divider :: forall a. Orientation -> Maybe String -> Widget a -> Widget a
divider orient text widget = blockWithData
  [ "divider" ++ show orient, if isJust text then "text-center" else "" ]
  { dataContent: text ?? "" }
  [ widget ]

---- Shapes --------------------------------------------------------------------

line :: forall a. Array (Widget a) -> Widget a
line = block [ "shape-line" ]
