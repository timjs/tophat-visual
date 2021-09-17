module Task.Script.Renderer where

import Preload

import Concur (list', repeat)
import Concur.Dom (Widget)
import Concur.Dom.Icon (Icon)
import Concur.Dom.Icon as Icon
import Concur.Dom.Input as Input
import Concur.Dom.Layout (Direction(..), Orientation(..), ShapeStyle, Sided(..), Stroke(..), LineStyle)
import Concur.Dom.Layout as Layout
import Concur.Dom.Text (text)
import Concur.Dom.Text as Text
import Data.Array as Array
import Data.HashMap as HashMap
import Task.Script.Context (Context, Typtext, aliases)
import Task.Script.Knot (Unchecked(..))
import Task.Script.Syntax (Row_, Arguments, Expression, Message, Name, Task(..))

---- Rendering -----------------------------------------------------------------

main :: Context -> Typtext -> Unchecked Task -> Widget (Unchecked Task)
main g s u =
  repeat u \u' ->
    Layout.column
      [ renderTask g s u'
      , Text.code <| show u'
      ]

renderTask :: Context -> Typtext -> Unchecked Task -> Widget (Unchecked Task)
renderTask g s u = renderTask' u
  where
  renderTask' :: Unchecked Task -> Widget (Unchecked Task)
  renderTask' (Unchecked t) = case t of
    ---- Steps
    Step m t1 t2 -> do
      r <- renderStep t1 t2
      let t1' ~> t2' = consolidate t1 t2 r
      done <| Unchecked (Step m t1' t2')
    Branch bs -> do
      ts' <- showContinuation Closed style_branch renderTask' (map snd bs)
      let bs' = Array.zip (map fst bs) ts'
      done <| Unchecked (Branch bs')
    Select bs -> do
      ts' <- showContinuation Open style_branch renderTask' (map trd bs)
      let bs' = Array.zip (map fst2 bs) ts' |> map assoc
      done <| Unchecked (Select bs')
      where
      fst2 (x ~> y ~> _) = x ~> y
      assoc ((x ~> y) ~> z) = x ~> (y ~> z)

    ---- Editors
    Enter n m -> do --OK
      n' <- selectType s Icon.pen n
      done <| Unchecked (Enter n' m)
    Update m e -> do --OK
      --TODO: edit message
      -- m' <- editMessage Icon.edit m
      e' <- editExpression Icon.edit e
      done <| Unchecked (Update m e')
    Change m e -> do --OK
      r <- showConnect style_line Both (editMessage Icon.edit m) (editExpression Icon.database e)
      let m' ~> e' = consolidate m e r
      done <| Unchecked (Change m' e')
    View m e -> do --OK
      --TODO: edit message
      -- m' <- editMessage Icon.eye m
      e' <- editExpression Icon.eye e
      done <| Unchecked (View m e')
    Watch m e -> do --OK
      r <- showConnect style_line Pull (editMessage Icon.eye m) (editExpression Icon.database e)
      let m' ~> e' = consolidate m e r
      done <| Unchecked (Watch m' e')

    ---- Combinators
    Lift e -> do --OK
      _ <- editExpression Icon.check_square e
      done <| Unchecked (Lift e)
    Pair ts -> do --OK
      ts' <- showGroup style_group renderTask' ts
      done <| Unchecked (Pair ts')
    Choose ts -> do --OK
      ts' <- showGroup style_branch renderTask' ts
      done <| Unchecked (Choose ts')

    ---- Extras
    Execute n a -> do
      n' <- selectTask g n
      done <| Unchecked (Execute n' a)
    Hole a -> do
      _ <- editHole g Icon.question a
      done <| Unchecked (Hole a)

    ---- Shares
    Share e -> do --OK
      e' <- editExpression Icon.retweet e
      done <| Unchecked (Share e')
    Assign e1 e2 -> do --OK
      r <- showConnect style_line Push (editExpression Icon.retweet e1) (editExpression Icon.database e2)
      let e1' ~> e2' = consolidate e1 e2 r
      done <| Unchecked (Assign e1' e2')

  -- |  r
  -- |  | m
  -- |  f
  renderStep :: Unchecked Task -> Unchecked Task -> Widget (Both (Unchecked Task))
  renderStep t1 t2 = do
    Layout.column
      [ renderTask' t1 >-> Left
      , Layout.line Vertical 2.0 style_line
      , renderTask' t2 >-> Right
      ]

data Kind = Open | Closed

---- Parts ---------------------------------------------------------------------

-- |   || w
showLine :: forall a r. LineStyle r -> Widget a -> Widget a
showLine s w =
  Layout.row [ Layout.line Vertical 2.0 s, w ]

-- | [[ w_1 .. w_n ]]
showBox :: forall a r. ShapeStyle r -> Array (Widget a) -> Widget a
showBox s w =
  Layout.box 0.5 10.0 1.0 s [ Layout.row w ]

-- |       V
-- | ==============
-- |  w_1 ... w_n
-- | =============
showContinuation :: forall a. Kind -> ShapeStyle () -> (a -> Widget a) -> Array a -> Widget (Array a)
showContinuation k s f ws =
  Layout.column
    [ Layout.head Downward style_line
    -- TODO either group or single, depending on count
    , showGroup s f ws
    ]

-- | ==============
-- |  w_1 ... w_n
-- | =============
showGroup :: forall a. ShapeStyle () -> (a -> Widget a) -> Array a -> Widget (Array a)
showGroup s f ws =
  Layout.group Horizontal s
    [ Layout.row [ list' f ws ] ]

-- | w_1 *--* w_2
showConnect :: forall a b r. LineStyle r -> Connect -> Widget a -> Widget b -> Widget (Either a b)
showConnect s m a b = do
  Layout.row [ a >-> Left, line, b >-> Right ]
  where
  line = case m of
    Pull -> Layout.row [ dot, connection ]
    Push -> Layout.row [ connection, dot ]
    Both -> Layout.row [ dot, connection, dot ]
  --TODO: factor out style
  dot = Layout.dot 0.33 "black"
  connection = Layout.line Layout.Horizontal 4.0 s

data Connect
  = Pull
  | Push
  | Both

---- Inputs --------------------------------------------------------------------

-- | [[ i m ]]
editMessage :: Icon -> Message -> Widget Message
editMessage i m =
  showBox style_box
    [ i, Input.entry m m ]

-- | [[ ?? ]]
editHole :: Context -> Icon -> Arguments -> Widget Name
editHole g i as =
  showBox style_hole
    [ Input.picker'
        [ "Builtin" ~> []
        , "Project" ~> Array.sort (HashMap.keys g)
        ]
        "??"
    ]

-- | [[ n ]]
selectTask :: Context -> Name -> Widget Name
selectTask g n =
  showBox style_box
    [ Input.picker'
        [ "Builtin" ~> []
        , "Project" ~> Array.sort (HashMap.keys g)
        ]
        n
    ]

-- | [[ i b ]]
selectType :: Typtext -> Icon -> Name -> Widget Name
selectType s i n =
  showBox style_box
    [ i
    , Input.picker'
        [ "Builtin" ~> Array.sort (HashMap.keys aliases)
        , "Project" ~> Array.sort (HashMap.keys s)
        ]
        n
    ]

-- -- | [[ i n ]]
-- selectValues :: Context -> Icon -> Row_ Name -> Widget (Row_ Name)
-- selectValues g i ns = do
--   _ <- showBox style_box []
--   done ns

-- | [[ i e ]]
editExpression :: Icon -> Expression -> Widget Expression
editExpression i e =
  showBox style_box
    --TODO make editable
    [ i, text <| show e ]

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

style_group :: ShapeStyle ()
style_group = style_box
  { thickness = 6.0
  , padding = All 2.0
  }

style_branch :: ShapeStyle ()
style_branch = style_group { stroke = "dashed" }

style_hole :: ShapeStyle ()
style_hole = style_box { stroke = "dashed", fill = "white" }

style_line :: ShapeStyle ()
style_line = style_box { thickness = 1.0 }

---- Widgets -------------------------------------------------------------------
type Both a
  = Either a a

consolidate :: forall a b. a -> b -> Either a b -> a * b
consolidate x y = case _ of
  Left x' -> x' ~> y
  Right y' -> x ~> y'
