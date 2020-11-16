module Task.Script.Renderer where

import Preload hiding (group)
import Concur (list', repeat)
import Concur.Dom (Widget)
import Concur.Dom.Icon (Icon)
import Concur.Dom.Icon as Icon
import Concur.Dom.Input as Input
import Concur.Dom.Layout (Sided, Direction(..), Orientation(..))
import Concur.Dom.Layout as Layout
import Data.Array as Array
import Task.Script.Context (basics)
import Task.Script.Error (Unchecked(..))
import Task.Script.Syntax (BasicType, Expression, Message, Name, Task(..))

---- Rendering -----------------------------------------------------------------
-- main :: forall a. Unchecked Task -> Widget (Unchecked Task)
main :: Unchecked Task -> Widget (Unchecked Task)
main u =
  repeat u \u' ->
    Layout.column
      [ Layout.text <| show u'
      , renderTask u'
      ]

renderTask :: Unchecked Task -> Widget (Unchecked Task)
renderTask u@(Unchecked t) =
  Layout.column
    [ case t of
        ---- Editors
        Enter b m -> do
          b' <- selectType Icon.pen b
          done <| Unchecked (Enter b' m)
        Update m e -> do
          m' <- editMessage Icon.edit m
          done <| Unchecked (Update m' e)
        Change m e -> do
          r <- renderShare Mutating (editMessage Icon.edit m) (editExpression Icon.database e)
          let
            m' ** e' = consolidate m e r
          done <| Unchecked (Change m' e')
        View m e -> do
          m' <- editMessage Icon.eye m
          done <| Unchecked (View m' e)
        Watch m e -> do
          r <- renderShare Reading (editMessage Icon.eye m) (editExpression Icon.database e)
          let
            m' ** e' = consolidate m e r
          done <| Unchecked (Watch m' e')
        ---- Combinators
        Lift e -> do
          e' <- editExpression Icon.check_square e
          done <| Unchecked (Lift e)
        Pair ts -> do
          ts' <- renderGroup ts
          done <| Unchecked (Pair ts')
        Choose ts -> do
          ts' <- renderGroup ts
          done <| Unchecked (Choose ts')
        Branch bs -> do
          ts' <- renderGroup (map snd bs)
          let
            bs' = Array.zip (map fst bs) ts'
          done <| Unchecked (Branch bs')
        Select bs -> do
          ts' <- renderGroup (map trd bs)
          let
            bs' = Array.zip (map fst2 bs) ts' |> map swap
          done <| Unchecked (Select bs')
          where
          fst2 (x ** y ** z) = x ** y

          swap ((x ** y) ** z) = x ** (y ** z)
        Step m t1 t2 -> do
          r <- renderStep t1 t2
          let
            t1' ** t2' = consolidate t1 t2 r
          done <| Unchecked (Step m t1' t2')
        ---- Extras
        Execute n a -> do
          n' <- selectTask n
          done <| Unchecked (Execute n' a)
        Hole a -> do
          _ <- editMessage Icon.question ""
          done <| Unchecked (Hole a)
        ---- Shares
        Share e -> do
          e' <- editExpression Icon.retweet e
          done <| Unchecked (Share e')
        Assign e1 e2 -> do
          r <- renderShare Writing (editExpression Icon.retweet e1) (editExpression Icon.database e2)
          let
            e1' ** e2' = consolidate e1 e2 r
          done <| Unchecked (Assign e1' e2')
    ]

---- Styles --------------------------------------------------------------------
style_box ::
  { draw :: String
  , fill :: String
  , margin :: Sided Number
  , padding :: Sided Number
  , stroke :: String
  , thickness :: Number
  }
style_box =
  { fill: "lightgray"
  , draw: "lightgray"
  , stroke: "solid"
  , thickness: 4.0
  , padding: Layout.All 0.5
  , margin: Layout.Some { top: 0.0, bottom: 0.0, left: 1.0, right: 1.0 }
  }

---- Widgets -------------------------------------------------------------------
type Both a
  = Either a a

consolidate :: forall a b. a -> b -> Either a b -> a ** b
consolidate x y = case _ of
  Left x' -> x' ** y
  Right y' -> x ** y'

-- | [[ t ]]
renderBox :: forall a. Array (Widget a) -> Widget a
renderBox inner = Layout.box 0.5 10.0 1.0 style_box [ Layout.row inner ]

-- | ============
-- |  t1 ... tn
-- | ===========
renderGroup :: Array (Unchecked Task) -> Widget (Array (Unchecked Task))
renderGroup ts =
  Layout.group Horizontal style_box
    [ Layout.row [ list' renderTask ts ] ]

-- |    t1
-- |    |
-- | ..t2..
renderStep :: Unchecked Task -> Unchecked Task -> Widget (Both (Unchecked Task))
renderStep t1 t2 = do
  Layout.column
    [ renderTask t1 ||> Left
    , Layout.line Vertical 2.0 { draw: "lightgray", stroke: "solid", thickness: 1.0 }
    , Layout.head Downward { draw: "lightgray", stroke: "solid", thickness: 1.0 }
    , renderTask t2 ||> Right
    ]

-- | [[ i m ]]
editMessage :: Icon -> Message -> Widget Message
editMessage i m =
  renderBox
    [ i, Input.entry m m ]

-- | [[ i b ]]
selectTask :: Name -> Widget Name
selectTask n =
  --FIXME how to select default value?
  renderBox
    [ Input.picker [] n ]

-- | [[ i b ]]
selectType :: Icon -> BasicType -> Widget BasicType
selectType i b =
  --FIXME how to select default value?
  renderBox
    [ i, Input.picker basics b ]

-- | [[ i e ]]
editExpression :: Icon -> Expression -> Widget Expression
editExpression i e =
  renderBox
    [ i, Layout.text <| show e ]

-- | x *--* y
renderShare :: forall a b. Mode -> Widget a -> Widget b -> Widget (Either a b)
renderShare m a b = do
  Layout.row [ a ||> Left, line, b ||> Right ]
  where
  line = case m of
    Reading -> Layout.row [ dot, connection ]
    Writing -> Layout.row [ connection, dot ]
    Mutating -> Layout.row [ dot, connection, dot ]

  dot = Layout.circle 0.33 { fill: "black", draw: "black", stroke: "solid", thickness: 0.0, margin: Layout.All 0.0, padding: Layout.All 0.0 } empty

  connection = Layout.line Layout.Horizontal 4.0 { draw: "black", stroke: "solid", thickness: 2.0 }

data Mode
  = Reading
  | Writing
  | Mutating
