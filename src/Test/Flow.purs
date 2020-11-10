module Test.Flow where

import Preload hiding (group)
import Concur (display, loop, merge)
import Concur.Dom (Signal, Widget)
import Concur.Dom.Icon (Icon)
import Concur.Dom.Icon as Icon
import Concur.Dom.Layout (Orientation(..))
import Concur.Dom.Layout as Layout
import Data.Array as Array
import Task.Script.Error (Unchecked(..))
import Task.Script.Syntax (Message, Task(..))

---- Rendering -----------------------------------------------------------------
task :: Unchecked Task -> Signal (Unchecked Task)
task u@(Unchecked t) = case t of
  ---- Editors
  Enter b m ->
    loop b (box Icon.pen m)
      ||> \b' -> Unchecked (Enter b' m)
  Update m e ->
    loop e (box Icon.edit m)
      ||> \e' -> Unchecked (Update m e')
  Change m e ->
    loop ({} ** e) (couple Mutating (box Icon.edit m) (box Icon.database ""))
      ||> \({} ** e') -> Unchecked (Change m e')
  View m e ->
    loop e (box Icon.eye m)
      ||> \e' -> Unchecked (View m e')
  Watch m e ->
    loop ({} ** e) (couple Reading (box Icon.eye m) (box Icon.database ""))
      ||> \({} ** e') -> Unchecked (Watch m e')
  ---- Combinators
  Lift e ->
    loop e (box Icon.check_square "")
      ||> \e' -> Unchecked (Lift e')
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
    loop n (box Icon.none "")
      ||> \n' -> Unchecked (Execute n' a)
  Hole a ->
    loop {} (box Icon.question "")
      ||> \_ -> Unchecked (Hole a)
  ---- Shares
  Share e ->
    loop e (box Icon.retweet "")
      ||> \e' -> Unchecked (Share e')
  Assign e1 e2 ->
    loop (e2 ** e1) (couple Writing (box Icon.retweet "") (box Icon.database ""))
      ||> \(e2' ** e1') -> Unchecked (Assign e1' e2')

group :: Array (Unchecked Task) -> Signal (Array (Unchecked Task))
group ts =
  -- Layout.column
  --   [ bar
  --   , bar
  --   ]
  -- where
  -- bar = Layout.line { draw: "lightgray", stroke: "solid", thickness: 4.0 } Horizontal 60.0
  Layout.box
    { borderStyle: "solid"
    , borderColor: "lightgray"
    , borderWidth: "4pt 0"
    , margin: "-2pt"
    } do
    traverse task ts

connect :: Unchecked Task ** Unchecked Task -> Signal (Unchecked Task ** Unchecked Task)
connect (t1 ** t2) =
  Layout.column do
    t1' <- task t1
    display <| Layout.line { draw: "lightgray", stroke: "solid", thickness: 1.0 } Vertical 2.0
    t2' <- task t2
    done (t1' ** t2')

---- Widgets -------------------------------------------------------------------
-- | [i a]^m
box :: forall a. Icon -> Message -> a -> Widget a
box i m a =
  Layout.box
    { backgroundColor: "lightgray"
    , borderRadius: "0.5pc"
    , minWidth: "10pc"
    , minHeight: "1pc"
    , padding: "6pt"
    , margin: "0 1pc"
    }
    (merge [ i, Layout.text m ])

-- | a *--* b
couple :: forall a b. Mode -> (a -> Widget a) -> (b -> Widget b) -> (a ** b) -> Widget (a ** b)
couple m f g (a ** b) =
  Layout.row (merge [ f a ||> Left, line, g b ||> Right ])
    ||> case _ of
        Left a' -> a' ** b
        Right b' -> a ** b'
  where
  line = case m of
    Reading -> Layout.row <| merge [ dot, connection ]
    Writing -> Layout.row <| merge [ connection, dot ]
    Mutating -> Layout.row <| merge [ dot, connection, dot ]

  dot = Layout.circle { fill: "black", draw: "black", stroke: "solid", thickness: 0.0 } 0.5

  connection = Layout.line { draw: "black", stroke: "solid", thickness: 2.0 } Horizontal 4.0

data Mode
  = Reading
  | Writing
  | Mutating
