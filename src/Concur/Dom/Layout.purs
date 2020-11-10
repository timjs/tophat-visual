module Concur.Dom.Layout
  -- # Boxes
  ( box
  , empty
  , row
  , column
  -- # Text
  , text
  -- # Shapes
  , Orientation(..)
  , Direction(..)
  , line
  , head
  , square
  , circle
  , diamond
  ) where

import Preload hiding (empty)
import Concur.Core.LiftWidget (class LiftWidget)
import Concur.Dom (Dom, Widget)
import Concur.Dom.Attr as Attr
import Concur.Dom.Node as Node
import Control.ShiftMap (class ShiftMap)
import Record as Record

---- Boxes ---------------------------------------------------------------------
box :: forall m a s. ShiftMap Widget m => Record s -> m a -> m a
box s = Node.div_ [ Attr.style s ]

empty :: forall m a s. LiftWidget Dom m => ShiftMap Widget m => Record s -> m a
empty s = box s (text "")

-- row :: forall a. Array (Widget a) -> Widget a
-- row :: forall m a. MultiAlternative m => ShiftMap Widget m => Array (m a) -> m a
-- row = merge >> box ({ flexDirection: "row" } /\ style_flexbox)
row :: forall m a. ShiftMap Widget m => m a -> m a
row = box ({ flexDirection: "row" } /\ style_flexbox)

-- column :: forall a. Array (Widget a) -> Widget a
-- column :: forall m a. MultiAlternative m => ShiftMap Widget m => Array (m a) -> m a
-- column = merge >> box ({ flexDirection: "column" } /\ style_flexbox)
column :: forall m a. ShiftMap Widget m => m a -> m a
column = box ({ flexDirection: "column" } /\ style_flexbox)

style_flexbox :: { alignItems :: String, display :: String, justifyContent :: String }
style_flexbox = { display: "flex", alignItems: "center", justifyContent: "center" }

---- Text ----------------------------------------------------------------------
-- type TextStyle r
--   = { face :: Face
--     , size :: Int
--     , shape :: Shape
--     , weight :: Weight
--     , decoration :: Line
--     | r
--     }
text :: forall m a. LiftWidget Dom m => String -> m a
text = Node.text

---- Shapes --------------------------------------------------------------------
data Orientation
  = Horizontal
  | Vertical

data Direction
  = Up
  | Down
  | Left
  | Right

type Color
  = String

type LineStyle r
  = { draw :: Color
    , stroke :: String
    -- , phase :: Phase
    , thickness :: Number -- in pt
    -- , cap :: Cap
    -- , join :: Join
    | r
    }

type ShapeStyle r
  = LineStyle ( fill :: Color | r )

line :: forall m a r. LiftWidget Dom m => ShiftMap Widget m => LineStyle r -> Orientation -> Number -> m a
line { draw, stroke, thickness } orientation length =
  empty
    <| { position: "relative"
      , borderColor: draw
      , borderStyle: stroke
      , borderWidth: thickness |> pt
      }
    /\ style_orientation
  where
  style_orientation = case orientation of
    Horizontal ->
      { width: length |> pc
      , height: 0.0 |> pc
      }
    Vertical ->
      { width: 0.0 |> pc
      , height: length |> pc
      }

head :: forall m a r. LiftWidget Dom m => ShiftMap Widget m => LineStyle r -> Direction -> m a
head { draw, stroke, thickness } direction =
  empty
    <| { width: 0.0 |> pc
      , height: 0.0 |> pc
      , borderStyle: "solid"
      , borderColor: "transparent"
      }
    /\ style_direction
  where
  style_direction = case direction of
    _ ->
      -- top, right, bottom, and left in that order (clockwise)
      { borderWidth: unwords [ thickness |> pt, thickness / 2.0 |> pt, 0.0 |> pt, thickness / 2.0 |> pt ]
      , borderColor: unwords [ draw, "transparent", "transparent", "transparent" ]
      }

square :: forall m a r. LiftWidget Dom m => ShiftMap Widget m => ShapeStyle r -> Number -> Number -> m a
square { fill, draw, stroke, thickness } width height =
  empty
    { width: width |> pc
    , height: height |> pc
    , backgroundColor: fill
    , borderColor: draw
    , borderStyle: stroke
    , borderWidth: thickness |> pt
    }

circle :: forall m a r. LiftWidget Dom m => ShiftMap Widget m => ShapeStyle r -> Number -> m a
circle { fill, draw, stroke, thickness } radius =
  empty
    { width: radius * 2.0 |> pc
    , height: radius * 2.0 |> pc
    , backgroundColor: fill
    , borderRadius: "50%"
    , borderColor: draw
    , borderStyle: stroke
    , borderWidth: thickness |> pt
    }

diamond :: forall m a r. LiftWidget Dom m => ShiftMap Widget m => ShapeStyle r -> Number -> Number -> m a
diamond { fill, draw, stroke, thickness } width height =
  empty
    { width: width |> pc
    , height: height |> pc
    , backgroundColor: fill
    , transform: "rotate(45deg)"
    -- margin: -0.75pc,
    }

---- Units ---------------------------------------------------------------------
pt :: Number -> String
pt x = show x ++ "pt"

pc :: Number -> String
pc x = show x ++ "pc"

---- Helpers -------------------------------------------------------------------
infixr 5 Record.union as /\
