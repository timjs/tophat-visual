module Task.Script.Renderer where

import Preload hiding (group)
import Concur (display, loop, merge, dynamic)
import Concur.Dom (Signal, Widget)
import Concur.Dom.Icon (Icon)
import Concur.Dom.Icon as Icon
import Concur.Dom.Input as Input
import Concur.Dom.Layout as Layout
import Data.Array as Array
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Task.Script.Context (basics)
import Task.Script.Error (Unchecked(..))
import Task.Script.Syntax (BasicType, Message, Task(..))

---- Rendering -----------------------------------------------------------------
main :: forall a. Unchecked Task -> Widget a
main u =
  dynamic
    <| Layout.column do
        u' <- task u
        display <| Layout.text (show u')

task :: Unchecked Task -> Signal (Unchecked Task)
task u@(Unchecked t) =
  Layout.column do
    -- display <| Layout.text (show u)
    case t of
      ---- Editors
      Enter b m ->
        loop b (selection Icon.pen)
          ||> \b' -> Unchecked (Enter b' m)
      Update m e ->
        loop m (box Icon.edit)
          ||> \m' -> Unchecked (Update m' e)
      Change m e ->
        loop (m ** "") (couple Mutating (box Icon.edit) (box Icon.database))
          ||> \(m' ** _) -> Unchecked (Change m' e)
      View m e ->
        loop m (box Icon.eye)
          ||> \m' -> Unchecked (View m' e)
      Watch m e ->
        loop (m ** "") (couple Reading (box Icon.eye) (box Icon.database))
          ||> \(m' ** _) -> Unchecked (Watch m' e)
      ---- Combinators
      Lift e ->
        loop "" (box Icon.check_square)
          ||> \m' -> Unchecked (Lift e)
      Pair ts ->
        group ts
          ||> \ts' -> Unchecked (Pair ts')
      Choose ts ->
        group ts
          ||> \ts' -> Unchecked (Choose ts')
      Branch bs ->
        group (map snd bs) ||> Array.zip (map fst bs)
          ||> \bs' -> Unchecked (Branch bs')
      Select bs ->
        group (map trd bs) ||> Array.zip (map fst2 bs) ||> map swap
          ||> \bs' -> Unchecked (Select bs')
        where
        fst2 (x ** y ** z) = x ** y

        swap ((x ** y) ** z) = x ** (y ** z)
      Step m t1 t2 ->
        connect (t1 ** t2)
          ||> \(t1' ** t2') -> Unchecked (Step m t1' t2')
      ---- Extras
      Execute n a ->
        loop n (box Icon.none)
          ||> \n' -> Unchecked (Execute n' a)
      Hole a ->
        loop "" (box Icon.question)
          ||> \m' -> Unchecked (Hole a)
      ---- Shares
      Share e ->
        loop "" (box Icon.retweet)
          ||> \m' -> Unchecked (Share e)
      Assign e1 e2 ->
        loop ("" ** "") (couple Writing (box Icon.retweet) (box Icon.database))
          ||> \(_ ** _) -> Unchecked (Assign e1 e2)

group :: Array (Unchecked Task) -> Signal (Array (Unchecked Task))
group ts =
  -- Layout.column
  --   [ bar
  --   , bar
  --   ]
  -- where
  -- bar = Layout.line { draw: "lightgray", stroke: "solid", thickness: 4.0 } Layout.Horizontal 60.0
  Layout.group Layout.Horizontal
    { draw: "lightgray"
    , stroke: "solid"
    , thickness: 4.0
    -- , margin: "-2pt"
    } do
    Layout.row do
      traverse task ts

connect :: Unchecked Task ** Unchecked Task -> Signal (Unchecked Task ** Unchecked Task)
connect (t1 ** t2) =
  Layout.column do
    t1' <- task t1
    display <| Layout.line Layout.Vertical 2.0 { draw: "lightgray", stroke: "solid", thickness: 1.0 }
    display <| Layout.head Layout.Down { draw: "lightgray", stroke: "solid", thickness: 1.0 }
    t2' <- task t2
    done (t1' ** t2')

---- Widgets -------------------------------------------------------------------
-- | [i a]^m
box :: Icon -> Message -> Widget Message
box i m =
  Layout.box 0.5 10.0 1.0
    { fill: "lightgray"
    , draw: "none"
    , stroke: "none"
    , thickness: 0.0
    , padding: Layout.All 0.5
    , margin: Layout.Some { top: 0.0, bottom: 0.0, left: 1.0, right: 1.0 }
    } do
    Layout.row do
      merge [ i, Input.entry m m ]

selection :: Icon -> BasicType -> Widget BasicType
selection i x =
  Layout.box 0.5 10.0 1.0
    { fill: "lightgray"
    , draw: "none"
    , stroke: "none"
    , thickness: 0.0
    , padding: Layout.All 0.5
    , margin: Layout.Some { top: 0.0, bottom: 0.0, left: 1.0, right: 1.0 }
    } do
    Layout.row do
      merge [ i, Input.picker basics ]

-- | a *--* b
couple :: forall a b. Mode -> (a -> Widget a) -> (b -> Widget b) -> (a ** b) -> Widget (a ** b)
couple m f g (a ** b) =
  Layout.row do
    r <- merge [ f a ||> Left, line, g b ||> Right ]
    done case r of
      Left a' -> a' ** b
      Right b' -> a ** b'
  where
  line = case m of
    Reading -> Layout.row <| merge [ dot, connection ]
    Writing -> Layout.row <| merge [ connection, dot ]
    Mutating -> Layout.row <| merge [ dot, connection, dot ]

  dot = Layout.circle 0.33 { fill: "black", draw: "black", stroke: "solid", thickness: 0.0, margin: Layout.All 0.0, padding: Layout.All 0.0 } empty

  connection = Layout.line Layout.Horizontal 4.0 { draw: "black", stroke: "solid", thickness: 2.0 }

data Mode
  = Reading
  | Writing
  | Mutating
