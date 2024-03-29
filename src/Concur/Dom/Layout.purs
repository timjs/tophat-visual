module Concur.Dom.Layout
  -- # Boxes
  ( element
  , row
  , column
  -- # Lines
  , LineStyle
  , Orientation(..)
  , Direction(..)
  , Stroke(..)
  , Sided(..)
  , Sized(..)
  , Color
  , line
  -- # Shapes
  , ShapeStyle
  , group
  , box
  , rectangle
  , square
  , circle
  , diamond
  , head
  , dot
  ) where

import Preload
import Concur.Dom (Widget)
import Concur.Dom.Attr as Attr
import Concur.Dom.Node as Node
import Record as Record

---- Boxes ---------------------------------------------------------------------

element :: forall a s. Record s -> Array (Widget a) -> Widget a
element s = Node.div [ Attr.style s ]

row :: forall a. Array (Widget a) -> Widget a
row = element ({ flexDirection: "row", alignItems: "stretch", justifyItems: "stretch" } \/ style_flexbox)

column :: forall a. Array (Widget a) -> Widget a
column = element ({ flexDirection: "column", alignItems: "center", justifyItems: "stretch" } \/ style_flexbox)

style_flexbox :: { display :: String }
style_flexbox = { display: "flex" }

---- Types ---------------------------------------------------------------------

data Orientation
  = Horizontal
  | Vertical

data Direction
  = Upward
  | Downward
  | Forward
  | Backward

type Color = String
type Length = Number

data Stroke
  = Dotted
  | Dashed
  | Solid
  | Double

instance Show Stroke where
  show = case _ of
    Dotted -> "dotted"
    Dashed -> "dashed"
    Solid -> "solid"
    Double -> "double"

type Round = Number

type Width = Number

type Height = Number

type Radius = Number

data Sided a
  = All a
  | Some { top :: a, right :: a, bottom :: a, left :: a }

derive instance Functor Sided

convert :: Sided String -> String
convert = case _ of
  All a -> a
  Some { top, right, bottom, left } -> unwords [ top, right, bottom, left ]

data Sized a
  = Exactly a
  | Maximally a
  | Minimally a
  | Between a a

---- Lines ---------------------------------------------------------------------

type LineStyle r =
  { draw :: Color
  , stroke :: String
  -- , phase :: Phase
  , thickness :: Number -- in pt
  -- , cap :: Cap
  -- , join :: Join
  | r
  }

line :: forall a r. Orientation -> Length -> LineStyle r -> Widget a
line orientation length { draw, stroke, thickness } =
  element
    ( { position: "relative"
      , borderColor: draw
      , borderStyle: stroke
      , borderWidth: thickness |> pt
      } \/ style_orientation
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

head :: forall a r. Direction -> LineStyle r -> Widget a
head direction { draw, stroke, thickness } =
  element
    ( { width: 0.0 |> pc
      , height: 0.0 |> pc
      , borderStyle: stroke
      } \/ style_direction
    )
    empty
  where
  style_direction = case direction of
    Downward ->
      -- top, right, bottom, left in that order (clockwise)
      { borderWidth: unwords [ thickness |> pc, thickness / 2.0 |> pc, 0.0 |> pc, thickness / 2.0 |> pc ]
      , borderColor: unwords [ draw, "transparent", "transparent", "transparent" ]
      }
    _ -> todo "add other directions"

-- border-left: 0.5pc solid transparent;
-- border-right: 0.5pc solid transparent;
-- border-top: 1pc solid lightgray;

---- Shapes --------------------------------------------------------------------

type ShapeStyle r = LineStyle
  ( fill :: Color
  , margin :: Sided Number
  , padding :: Sided Number
  | r
  )

group :: forall a r. Orientation -> ShapeStyle r -> Array (Widget a) -> Widget a
group orientation { draw, stroke, thickness, margin, padding } =
  element
    { backgroundColor: "transparent"
    , borderColor: draw
    , borderStyle: stroke
    , borderWidth: style_orientation
    , margin: margin |> map pc |> convert
    , padding: padding |> map pc |> convert
    }
  where
  style_orientation = case orientation of
    Horizontal -> unwords [ thickness |> pt, "0" ]
    Vertical -> unwords [ "0", thickness |> pt ]

box :: forall a r. Round -> Width -> Height -> ShapeStyle r -> Array (Widget a) -> Widget a
box round width height { fill, draw, stroke, thickness, margin, padding } =
  element
    { width: width |> pc
    , height: height |> pc
    , backgroundColor: fill
    , borderRadius: round |> pc
    , borderColor: draw
    , borderStyle: stroke
    , borderWidth: thickness |> pt
    , margin: margin |> map pc |> convert
    , padding: padding |> map pc |> convert
    }

rectangle :: forall a r. Width -> Height -> ShapeStyle r -> Array (Widget a) -> Widget a
rectangle = box 0.0

square :: forall a r. Width -> ShapeStyle r -> Array (Widget a) -> Widget a
square size = rectangle size size

circle :: forall a r. Radius -> ShapeStyle r -> Array (Widget a) -> Widget a
circle radius = box 50.0 diameter diameter
  where
  diameter = radius * 2.0

diamond :: forall a r. Width -> Height -> ShapeStyle r -> Array (Widget a) -> Widget a
diamond width height style inner = rotate 45.0 [ rectangle width height style [ rotate (negate 45.0) inner ] ]

dot :: forall a. Radius -> Color -> Widget a
dot radius color = circle radius
  { fill: color
  , draw: color
  , stroke: "solid"
  , thickness: 0.0
  , margin: All 0.0
  , padding: All 0.0
  }
  empty

---- Transformations -----------------------------------------------------------

type Degrees = Number

rotate :: forall a. Degrees -> Array (Widget a) -> Widget a
rotate degrees =
  element
    { transform: "rotate(" ++ show degrees ++ "deg)" }

---- Units ---------------------------------------------------------------------

pt :: Number -> String
pt x = show x ++ "pt"

pc :: Number -> String
pc x = show x ++ "pc"

-- ct :: Number -> String
-- ct x = show x ++ "%"

---- Helpers -------------------------------------------------------------------

infixr 5 Record.union as \/
