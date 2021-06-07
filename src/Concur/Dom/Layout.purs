module Concur.Dom.Layout
  -- # Boxes
  ( row
  , column
  -- # Text
  , text
  , code
  , lines
  -- # Lines
  , LineStyle
  , Orientation(..)
  , Direction(..)
  , Sided(..)
  , Sized(..)
  , Color
  , line
  , head
  -- # Shapes
  , ShapeStyle
  , group
  , box
  , rectangle
  , square
  , circle
  , diamond
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
row = element ({ flexDirection: "row" } /\ style_flexbox)

column :: forall a. Array (Widget a) -> Widget a
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
text :: forall a. String -> Widget a
text = Node.text

code :: forall a. String -> Widget a
code s = Node.pre [] [ Node.text s ]

lines :: forall a. Array String -> Widget a
lines xs = column <| map Node.text xs

---- Lines ---------------------------------------------------------------------
data Orientation
  = Horizontal
  | Vertical

data Direction
  = Upward
  | Downward
  | Forward
  | Backward

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

line :: forall a r. Orientation -> Number -> LineStyle r -> Widget a
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

head :: forall a r. Direction -> LineStyle r -> Widget a
head direction { draw, stroke, thickness } =
  element
    ( { width: 0.0 |> pc
      , height: 0.0 |> pc
      , borderStyle: "solid"
      }
        /\ style_direction
    )
    empty
  where
  style_direction = case direction of
    _ ->  --TODO add other directions
      -- top, right, bottom, left in that order (clockwise)
      { borderWidth: unwords [ thickness |> pc, thickness / 2.0 |> pc, 0.0 |> pc, thickness / 2.0 |> pc ]
      , borderColor: unwords [ draw, "transparent", "transparent", "transparent" ]
      }

-- border-left: 0.5pc solid transparent;
-- border-right: 0.5pc solid transparent;
-- border-top: 1pc solid lightgray;
---- Shapes --------------------------------------------------------------------
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

type ShapeStyle r
  = LineStyle
      ( fill :: Color
      , margin :: Sided Number
      , padding :: Sided Number
      | r
      )

group :: forall a r. Orientation -> LineStyle r -> Array (Widget a) -> Widget a
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

box :: forall a r. Number -> Number -> Number -> ShapeStyle r -> Array (Widget a) -> Widget a
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

rectangle :: forall a r. Number -> Number -> ShapeStyle r -> Array (Widget a) -> Widget a
rectangle = box 0.0

square :: forall a r. Number -> ShapeStyle r -> Array (Widget a) -> Widget a
square size = rectangle size size

circle :: forall a r. Number -> ShapeStyle r -> Array (Widget a) -> Widget a
circle radius = box 50.0 diameter diameter
  where
  diameter = radius * 2.0

diamond :: forall a r. Number -> Number -> ShapeStyle r -> Array (Widget a) -> Widget a
diamond width height style inner = rotate 45.0 [ rectangle width height style [ rotate (-45.0) inner ] ]

---- Transformations -----------------------------------------------------------
rotate :: forall a. Number -> Array (Widget a) -> Widget a
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
