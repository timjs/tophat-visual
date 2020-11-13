module Concur.Dom.Layout
  -- # Actions
  ( area
  -- # Boxes
  , row
  , column
  -- # Text
  , text
  -- # Lines
  , Orientation(..)
  , Direction(..)
  , Sided(..)
  , Sized(..)
  , line
  , head
  -- # Shapes
  , group
  , box
  , rectangle
  , square
  , circle
  , diamond
  ) where

import Preload
import Concur (class Lift, class Shift)
import Concur.Dom (Dom, Attr, Widget)
import Concur.Dom.Attr as Attr
import Concur.Dom.Node as Node
import Record as Record

---- Actions -------------------------------------------------------------------
area :: forall m a. Shift Widget m => Array (Attr a) -> m a -> m a
area = Node.div_

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

line :: forall m a r. Lift Dom m => Orientation -> Number -> LineStyle r -> m a
line orientation length { draw, stroke, thickness } =
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

head :: forall m a r. Lift Dom m => Direction -> LineStyle r -> m a
head direction { draw, stroke, thickness } =
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
data Sided a
  = All a
  | Some { top :: a, right :: a, bottom :: a, left :: a }

derive instance functorSided :: Functor Sided

convert :: Sided String -> String
convert = case _ of
  All a -> a
  Some { top, right, bottom, left } -> unwords [ top, right, bottom, left ]

data Sized a
  = Exactly a
  | Maximally a
  | Minimally a
  | Between a a

type ShapeStyle r
  = LineStyle ( fill :: Color, margin :: Sided Number, padding :: Sided Number | r )

group :: forall m a r. Shift Widget m => Orientation -> LineStyle r -> m a -> m a
group orientation { draw, stroke, thickness } =
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

box :: forall m a r. Shift Widget m => Number -> Number -> Number -> ShapeStyle r -> m a -> m a
box round width height { fill, draw, stroke, thickness, margin, padding } =
  element
    { width: width |> pc
    , height: height |> pc
    , backgroundColor: fill
    , borderRadius: round |> ct
    , borderColor: draw
    , borderStyle: stroke
    , borderWidth: thickness |> pt
    , margin: margin |> map pc |> convert
    , padding: padding |> map pc |> convert
    }

rectangle :: forall m a r. Shift Widget m => Number -> Number -> ShapeStyle r -> m a -> m a
rectangle = box 0.0

square :: forall m a r. Shift Widget m => Number -> ShapeStyle r -> m a -> m a
square size = rectangle size size

circle :: forall m a r. Shift Widget m => Number -> ShapeStyle r -> m a -> m a
circle radius = box 50.0 diameter diameter
  where
  diameter = radius * 2.0

diamond :: forall m a r. Shift Widget m => Number -> Number -> ShapeStyle r -> m a -> m a
diamond width height style inner = rotate 45.0 (rectangle width height style (rotate (-45.0) inner))

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
