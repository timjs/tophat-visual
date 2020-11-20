module Task.Script.Renderer where

import Preload hiding (group)
import Concur (list', repeat)
import Concur.Dom (Widget)
import Concur.Dom.Icon (Icon)
import Concur.Dom.Icon as Icon
import Concur.Dom.Input as Input
import Concur.Dom.Layout (Direction(..), Orientation(..), ShapeStyle, Sided(..), LineStyle)
import Concur.Dom.Layout as Layout
import Data.Array as Array
import Data.HashMap as HashMap
import Task.Script.Context (Typtext, Context, types)
import Task.Script.Error (Unchecked(..))
import Task.Script.Syntax (Row, Arguments, Expression, Message, Name, Task(..))

---- Rendering -----------------------------------------------------------------
main :: Context -> Typtext -> Unchecked Task -> Widget (Unchecked Task)
main g s u =
  repeat u \u' ->
    Layout.column
      [ renderTask g s u'
      , Layout.code <| show u'
      ]

renderTask :: Context -> Typtext -> Unchecked Task -> Widget (Unchecked Task)
renderTask g s u_ = Layout.column [ renderTask' u_ ]
  where
  renderTask' :: Unchecked Task -> Widget (Unchecked Task)
  renderTask' u@(Unchecked t) = case t of
    ---- Editors
    Enter n m -> do
      n' <- selectType s Icon.pen n
      done <| Unchecked (Enter n' m)
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
      _ <- selectValues g Icon.check_square HashMap.empty
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
        bs' = Array.zip (map fst2 bs) ts' |> map assoc
      done <| Unchecked (Select bs')
      where
      fst2 (x ** y ** z) = x ** y

      assoc ((x ** y) ** z) = x ** (y ** z)
    Step m t1 t2 -> do
      r <- renderStep t1 t2
      let
        t1' ** t2' = consolidate t1 t2 r
      done <| Unchecked (Step m t1' t2')
    ---- Extras
    Execute n a -> do
      n' <- selectTask g n
      done <| Unchecked (Execute n' a)
    Hole a -> do
      _ <- editHole g Icon.question a
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

  -- | ============
  -- |  t1 ... tn
  -- | ===========
  renderGroup :: Array (Unchecked Task) -> Widget (Array (Unchecked Task))
  renderGroup ts =
    Layout.group Horizontal style_box
      [ Layout.row [ list' renderTask' ts ] ]

  -- |    t1
  -- |    |
  -- | ..t2..
  renderStep :: Unchecked Task -> Unchecked Task -> Widget (Both (Unchecked Task))
  renderStep t1 t2 = do
    Layout.column
      [ renderTask' t1 ||> Left
      , Layout.line Vertical 2.0 style_line
      , Layout.head Downward style_line
      , renderTask' t2 ||> Right
      ]

-- | [[ t ]]
showBox :: forall a r. ShapeStyle r -> Array (Widget a) -> Widget a
showBox style inner =
  Layout.column
    [ Layout.line Vertical 2.0 style_line
    , Layout.box 0.5 10.0 1.0 style [ Layout.row inner ]
    ]

-- | [[ i m ]]
editMessage :: Icon -> Message -> Widget Message
editMessage i m =
  showBox style_box
    [ i, Input.entry m m ]

editHole :: Context -> Icon -> Arguments -> Widget Name
editHole g i as =
  showBox style_hole
    [ Input.picker'
        [ "Builtin" ** []
        , "Project" ** Array.sort (HashMap.keys g)
        ]
        "??"
    ]

-- | [[ i b ]]
selectTask :: Context -> Name -> Widget Name
selectTask g n =
  showBox style_box
    [ Input.picker'
        [ "Builtin" ** []
        , "Project" ** Array.sort (HashMap.keys g)
        ]
        n
    ]

-- | [[ i b ]]
selectType :: Typtext -> Icon -> Name -> Widget Name
selectType s i n =
  showBox style_box
    [ i
    , Input.picker'
        [ "Builtin" ** Array.sort (HashMap.keys types)
        , "Project" ** Array.sort (HashMap.keys s)
        ]
        n
    ]

selectValues :: Context -> Icon -> Row Name -> Widget (Row Name)
selectValues g i ns = do
  _ <- showBox style_box []
  done ns

-- | [[ i e ]]
editExpression :: Icon -> Expression -> Widget Expression
editExpression i e =
  showBox style_box
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

  dot = Layout.circle 0.33 { fill: "black", draw: "black", stroke: "solid", thickness: 0.0, margin: All 0.0, padding: All 0.0 } empty

  connection = Layout.line Layout.Horizontal 4.0 style_line

data Mode
  = Reading
  | Writing
  | Mutating

---- Styles --------------------------------------------------------------------
style_box :: ShapeStyle ()
style_box =
  { draw: "lightgray"
  , stroke: "solid"
  , thickness: 4.0
  , fill: "lightgray"
  , padding: All 0.5
  , margin: Some { top: 0.0, bottom: 0.0, left: 1.0, right: 1.0 }
  }

style_hole :: ShapeStyle ()
style_hole = style_box { stroke = "dash", fill = "white" }

style_line :: ShapeStyle ()
style_line = style_box { thickness = 1.0 }

---- Widgets -------------------------------------------------------------------
type Both a
  = Either a a

consolidate :: forall a b. a -> b -> Either a b -> a ** b
consolidate x y = case _ of
  Left x' -> x' ** y
  Right y' -> x ** y'
