module Concur.Dom.Layout
  -- # Boxes
  ( element
  , row
  , column
  , group
  -- # Text
  , text
  -- # Shapes
  , Orientation(..)
  , Direction(..)
  , line
  , head
  , rectangle
  , square
  , circle
  , diamond
  ) where

import Preload
import Concur (class Lift, class Shift)
import Concur.Dom (Dom, Widget)
import Concur.Dom.Attr as Attr
import Concur.Dom.Node as Node
import Record as Record

---- Boxes ---------------------------------------------------------------------
element :: forall m a s. Shift Widget m => Record s -> m a -> m a
element s = Node.div_ [ Attr.style s ]

-- row :: forall a. Array (Widget a) -> Widget a
-- row :: forall m a. MultiAlternative m => Lift Dom m => Array (m a) -> m a
-- row = merge >> element ({ flexDirection: "row" } /\ style_flexelement)
row :: forall m a. Shift Widget m => m a -> m a
row = element ({ flexDirection: "row" } /\ style_flexbox)

-- column :: forall a. Array (Widget a) -> Widget a
-- column :: forall m a. MultiAlternative m => Lift Dom m => Array (m a) -> m a
-- column = merge >> element ({ flexDirection: "column" } /\ style_flexbox)
column :: forall m a. Shift Widget m => m a -> m a
column = element ({ flexDirection: "column" } /\ style_flexbox)

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
text :: forall m a. Lift Dom m => String -> m a
text = Node.text

---- Lines ---------------------------------------------------------------------
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

line :: forall m a r. Lift Dom m => LineStyle r -> Orientation -> Number -> m a
line { draw, stroke, thickness } orientation length =
  element
    ( { position: "relative"
      , borderColor: draw
      , borderStyle: stroke
      , borderWidth: thickness |> pt
      }
        /\ style_orientation
    )
    empty
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

head :: forall m a r. Lift Dom m => LineStyle r -> Direction -> m a
head { draw, stroke, thickness } direction =
  element
    ( { width: 0.0 |> pc
      , height: 0.0 |> pc
      , borderStyle: "solid"
      , borderColor: "transparent"
      }
        /\ style_direction
    )
    empty
  where
  style_direction = case direction of
    _ ->
      -- top, right, bottom, left in that order (clockwise)
      { borderWidth: unwords [ thickness |> pt, thickness / 2.0 |> pt, 0.0 |> pt, thickness / 2.0 |> pt ]
      , borderColor: unwords [ draw, "transparent", "transparent", "transparent" ]
      }

---- Shapes --------------------------------------------------------------------
type ShapeStyle r
  = LineStyle ( fill :: Color | r )

group :: forall m a r. Shift Widget m => LineStyle r -> Orientation -> m a -> m a
group { draw, stroke, thickness } orientation =
  element
    { borderColor: draw
    , borderStyle: stroke
    , borderWidth: style_orientation
    -- , margin: "-2pt"
    }
  where
  style_orientation = case orientation of
    Horizontal -> unwords [ thickness |> pt, "0" ]
    Vertical -> unwords [ "0", thickness |> pt ]

box :: forall m a r. Shift Widget m => ShapeStyle r -> Number -> Number -> Number -> m a -> m a
box { fill, draw, stroke, thickness } round width height =
  element
    { width: width |> pc
    , height: height |> pc
    , backgroundColor: fill
    , borderRadius: round |> ct
    , borderColor: draw
    , borderStyle: stroke
    , borderWidth: thickness |> pt
    }

rectangle :: forall m a r. Shift Widget m => ShapeStyle r -> Number -> Number -> m a -> m a
rectangle style = box style 0.0

square :: forall m a r. Shift Widget m => ShapeStyle r -> Number -> m a -> m a
square style size = rectangle style size size

circle :: forall m a r. Shift Widget m => ShapeStyle r -> Number -> m a -> m a
circle style radius = box style 50.0 diameter diameter
  where
  diameter = radius * 2.0

diamond :: forall m a r. Shift Widget m => ShapeStyle r -> Number -> Number -> m a -> m a
diamond style width height inner = rotate 45.0 (rectangle style width height (rotate (-45.0) inner))

---- Transformations -----------------------------------------------------------
rotate :: forall m a. Shift Widget m => Number -> m a -> m a
rotate degrees =
  element
    { transform: "rotate(" ++ show degrees ++ "deg)" }

---- Units ---------------------------------------------------------------------
pt :: Number -> String
pt x = show x ++ "pt"

pc :: Number -> String
pc x = show x ++ "pc"

ct :: Number -> String
ct x = show x ++ "%"

---- Helpers -------------------------------------------------------------------
infixr 5 Record.union as /\
