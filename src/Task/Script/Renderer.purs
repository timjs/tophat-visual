module Task.Script.Renderer where

import Preload

import Concur as Concur
import Concur.Dom (Widget)
import Concur.Dom.Icon as Icon
import Concur.Dom.Input as Input
import Concur.Dom.Style (Kind(..), Position(..), Size(..), Stroke(..), Style(..))
import Concur.Dom.Style as Layout
import Concur.Dom.Text as Text
import Data.Array as Array
import Data.Either.Nested as Either
import Data.HashMap as HashMap
import Task.Script.Annotation (Annotated(..), Checked, Status(..))
import Task.Script.Context (Context, Typtext, aliases)
import Task.Script.Syntax (Arguments(..), BasicType, Branches, Expression(..), Label, LabeledBranches, Match(..), Name, Row_, Task(..))
import Task.Script.Loader (validate)

---- Rendering -----------------------------------------------------------------

type Renderer = Checked Task -> Widget (Checked Task)

main :: Context -> Typtext -> Checked Task -> Widget (Checked Task)
main g s t =
  Concur.repeat t \t' ->
    Layout.column
      [ Layout.row
          [ renderTask g s t'
          , Input.button Primary Medium "Check" ->> validate s g t'
          ]
      , Text.code "TopHat" (show t')
      -- , Input.editor 20 "code" (show t') ->> t'
      ]

renderTask :: Context -> Typtext -> Checked Task -> Widget (Checked Task)
renderTask g s = go
  where
  go :: Checked Task -> Widget (Checked Task)
  go (Annotated a_t t) = case t of
    ---- Steps
    --INVARIANT third arg of `Step` is always Branch or Select
    Step m t (Annotated a_bs (Branch bs)) -> do
      m' ~ t' ~ bs' <- renderBranches go m t bs
      done <| Annotated a_t (Step m' t' (Annotated a_bs (Branch bs')))
    Step m t (Annotated a_bs (Select bs)) -> do
      m' ~ t' ~ bs' <- renderSelects go m t bs
      done <| Annotated a_t (Step m' t' (Annotated a_bs (Select bs')))
    Step m t (Annotated a_l (Lift e)) -> do
      m' ~ t' <- renderEnd go m t
      done <| Annotated a_t (Step m' t' (Annotated a_l (Lift e)))
    Step m t1 t2 -> do
      m' ~ t1' ~ t2' <- renderSingle go m t1 t2
      done <| Annotated a_t (Step m' t1' t2')
    Branch _ -> todo "invalid single branch"
    Select _ -> todo "invalid single select"
    -- Branch bs -> do
    --   ts' <- renderContinuation Closed style_branch go (map snd bs)
    --   let bs' = Array.zip (map fst bs) ts'
    --   done <| Annotated ?h (Branch bs')
    -- Select bs -> do
    --   ts' <- renderContinuation Open style_branch go (map trd bs)
    --   let bs' = Array.zip (map fst2 bs) ts' |> map assoc
    --   done <| Annotated ?h (Select bs')
    --   where
    --   fst2 (x ~ y ~ _) = x ~ y
    --   assoc ((x ~ y) ~ z) = x ~ (y ~ z)

    ---- Editors
    Enter n -> do
      n' <- renderEnter s n
      done <| Annotated a_t (Enter n')
    Update e -> do
      e' <- renderUpdate e
      done <| Annotated a_t (Update e')
    Change e -> todo "change"
    -- Change  e -> do
    --   r <- renderConnect style_line Both (editMessage Icon.edit m) (editExpression Icon.database e)
    --   let e' = consolidate m e r
    --   done <| Annotated a_t (Change m' e')
    View e -> do
      e' <- renderView e
      done <| Annotated a_t (View e')
    Watch e -> todo "watch"
    -- Watch  e -> do
    --   r <- renderConnect style_line Pull (editMessage Icon.eye m) (editExpression Icon.database e)
    --   let e' = consolidate m e r
    --   done <| Annotated a_t (Watch m' e')

    ---- Combinators
    Lift e -> do
      e' <- renderLift e
      done <| Annotated a_t (Lift e')
    Pair ts -> do
      ts' <- renderGroup Solid go ts
      done <| Annotated a_t (Pair ts')
    Choose ts -> do
      ts' <- renderGroup Double go ts
      done <| Annotated a_t (Choose ts')

    ---- Extras
    Execute n as -> do
      n' ~ as' <- renderExecute g n as
      done <| Annotated a_t (Execute n' as')
    Hole as -> do
      n' ~ as' <- renderExecute g "??" as
      if n' == "??" then
        done <| Annotated a_t (Hole as')
      else
        done <| Annotated a_t (Execute n' as')

    ---- Shares
    Assign e1 e2 -> todo "assign"
    -- Assign e1 e2 -> do
    --   r <- renderConnect style_line Push (editExpression Icon.retweet e1) (editExpression Icon.database e2)
    --   let e1' ~ e2' = consolidate e1 e2 r
    --   done <| Annotated ?h (Assign e1' e2')

    Share e -> do
      e' <- renderShare e
      done <| Annotated a_t (Share e')

---- Parts ---------------------------------------------------------------------

---- General ----

-- | [[ * |   n   ]]
-- |     ||
renderStart :: Name -> Widget Name
renderStart name = Layout.column
  [ Input.addon Medium Icon.clipboard (Input.entry Medium "name of flow" name)
  , Layout.line Solid empty
  ]

-- |      || as
-- |  [[  n  ?]]
renderExecute :: Context -> Name -> Arguments -> Widget (Name * Arguments)
renderExecute context name args@(ARecord row) =
  Layout.column
    [ renderLine row >-> (ARecord >> Either.in2)
    , Input.picker
        [ "Builtin" ~ []
        , "Project" ~ Array.sort (HashMap.keys context)
        ]
        name >-> Either.in1
    ]
    >-> fix2 name args

renderLine :: forall a. Row_ a -> Widget (Row_ a)
renderLine row =
  Layout.line Solid (Layout.place After (renderLabels row))

-- | || (( a_1 .. a_n ))
renderLabels :: forall a. Row_ a -> Widget (Row_ a)
renderLabels =
  HashMap.keys >> map (Layout.chip Default) >> Layout.row

---- Steps ----

-- |   || as
-- |   V
renderStep :: Style -> Match -> Widget Match
renderStep style (MRecord row) =
  Layout.column
    [ renderLine row >-> MRecord
    , Layout.triangle style empty
    ]
renderStep _ _ = todo "other matches in step rendering"

renderOption :: Status -> Expression -> Widget Expression
renderOption status guard =
  Layout.line Dashed (Layout.place After (renderGuard status guard))

renderSingle :: forall a. (a -> Widget a) -> Match -> a -> a -> Widget (Match * a * a)
renderSingle render match sub1 sub2 =
  Layout.column
    [ render sub1 >-> Either.in2
    , renderStep Filled match >-> Either.in1
    , render sub2 >-> Either.in3
    ]
    >-> fix3 match sub1 sub2

renderEnd :: forall a. (a -> Widget a) -> Match -> a -> Widget (Match * a)
renderEnd render args@(MRecord row) subtask =
  Layout.column
    [ render subtask >-> Either.in2
    , renderLine row >-> (MRecord >> Either.in1)
    ]
    >-> fix2 args subtask
renderEnd _ _ _ = todo "other matches in end rendering"

---- Branches ----

renderBranches :: Renderer -> Match -> Checked Task -> Branches (Checked Task) -> Widget (Match * Checked Task * Branches (Checked Task))
renderBranches render match subtask branches =
  Layout.column
    [ render subtask >-> Either.in2
    , renderStep Filled match >-> Either.in1
    , Layout.branch [ Concur.traverse (renderBranch render) branches >-> Either.in3 ]
    -- , Layout.branch (Concur.traverse ?renderBranch branches) >-> Either.in3
    ]
    >-> fix3 match subtask branches

renderBranch :: Renderer -> Expression * Checked Task -> Widget (Expression * Checked Task)
renderBranch render (guard ~ subtask@(Annotated status _)) =
  Layout.column
    [ renderOption status guard >-> Either.in1
    , render subtask >-> Either.in2
    , Layout.line Solid empty
    ]
    >-> fix2 guard subtask

--   Layout.column
--     [ Layout.line Dashed [ Layout.place After (Input.addon Icon.question (Input.entry Small ?holder ?value)) ]
--     , renderTask task
--     ]

-- renderSelects :: forall a b z. (a -> Widget a) -> Match -> a -> Array b -> Widget (Match + a + Array b + z)
renderSelects :: Renderer -> Match -> Checked Task -> LabeledBranches (Checked Task) -> Widget (Match * Checked Task * LabeledBranches (Checked Task))
renderSelects render match subtask branches =
  Layout.column
    [ render subtask >-> Either.in2
    , renderStep Outlined match >-> Either.in1
    , Layout.branch [ Concur.traverse (renderSelect render) branches ] >-> Either.in3
    ]
    >-> fix3 match subtask branches

renderSelect :: Renderer -> Label * Expression * Checked Task -> Widget (Label * Expression * Checked Task)
renderSelect render (label ~ guard ~ subtask@(Annotated status _)) =
  Layout.column
    [ renderOption status guard >-> Either.in2
    , renderLabel label >-> Either.in1
    , render subtask >-> Either.in3
    ]
    >-> fix3 label guard subtask

---- Combinators ----

-- | ==============
-- |  t_1 ... t_n
-- | =============
renderGroup :: forall a. Stroke -> (a -> Widget a) -> Array a -> Widget (Array a)
renderGroup stroke trans widgets =
  Layout.group stroke
    [ Concur.traverse trans widgets ]

---- Editors ----

-- | [[ i |   w   ]]
renderEditor :: forall a. Widget a -> Widget a -> Widget a
renderEditor =
  Input.addon Medium

renderEnter :: Row_ BasicType -> Name -> Widget Name
renderEnter types name =
  renderEditor Icon.pen (selectType types name)

renderUpdate :: Expression -> Widget Expression
renderUpdate expr =
  renderEditor Icon.edit (editExpression expr)

renderView :: Expression -> Widget Expression
renderView expr =
  renderEditor Icon.eye (editExpression expr)

renderLift :: Expression -> Widget Expression
renderLift expr =
  renderEditor Icon.check_double (editExpression expr)

renderShare :: Expression -> Widget Expression
renderShare expr =
  renderEditor Icon.retweet (editExpression expr)

renderGuard :: Status -> Expression -> Widget Expression
renderGuard status expr =
  renderError status
    (Input.addon Small Icon.question (editGuard expr))

renderLabel :: Label -> Widget Label
renderLabel = editLabel

---- Helpers -------------------------------------------------------------------

renderError :: forall a. Status -> Widget a -> Widget a
renderError (Failure _ err) =
  Layout.element Error << Input.tooltip Before (show err)
renderError _ =
  Layout.element Default

---- Entries -------------------------------------------------------------------

-- | [[  n  ?]]
selectType :: Typtext -> Name -> Widget Name
selectType types name =
  Input.picker
    [ "Builtin" ~ Array.sort (HashMap.keys aliases)
    , "Project" ~ Array.sort (HashMap.keys types)
    ]
    name

-- | [[  e  ]]
editExpression :: Expression -> Widget Expression
editExpression expr =
  Input.entry Medium "enter an expression..." (show expr) ->> Variable "x"

editGuard :: Expression -> Widget Expression
editGuard expr =
  Input.entry Small "enter an expression..." (show expr) ->> Variable "x"

editLabel :: Label -> Widget Label
editLabel lbl =
  Input.entry Small "enter a label..." lbl

{-
-- |       V
-- | ==============
-- |  w_1 ... w_n
-- | =============
renderContinuation :: forall a. Kind -> ShapeStyle () -> (a -> Widget a) -> Array a -> Widget (Array a)
renderContinuation k s f ws =
  Layout.column
    [ Layout.head Downward style_line
    -- TODO either group or single, depending on count
    , renderGroup s f ws
    ]

-- |  r
-- |  | m
-- |  f
renderStep :: Checked Task -> Checked Task -> Widget (Both (Checked Task))
renderStep t1 t2 = do
  Layout.column
    [ go t1 >-> Left
    , Layout.line
    , go t2 >-> Right
    ]

-- -- | w_1 *--* w_2
-- renderConnect :: forall a b r. LineStyle r -> Connect -> Widget a -> Widget b -> Widget (Either a b)
-- renderConnect s m a b = do
--   Layout.row [ a >-> Left, line, b >-> Right ]
--   where
--   line = case m of
--     Pull -> Layout.row [ dot, connection ]
--     Push -> Layout.row [ connection, dot ]
--     Both -> Layout.row [ dot, connection, dot ]
--   --TODO: factor out style
--   dot = Layout.dot 0.33 "black"
--   connection = Layout.line Layout.Horizontal 4.0 s

-- data Connect
--   = Pull
--   | Push
--   | Both

---- Inputs --------------------------------------------------------------------

-- | [[ i m ]]
editMessage :: Icon ->  Widget Message
editMessage i m =
  renderBox style_box
    [ i, Input.entry m m ]

editLabels :: Context -> Arguments -> Widget Arguments
editLabels g (ARecord as) =
  --TODO show labels and expressions
  Layout.column (map (show >> text) (HashMap.keys as))

-- -- | [[ i n ]]
-- selectValues :: Context -> Icon -> Row_ Name -> Widget (Row_ Name)
-- selectValues g i ns = do
--   _ <- renderBox style_box []
--   done ns

-}

---- Helpers -------------------------------------------------------------------

type Both a
  = Either a a

fix2 :: forall a b. a -> b -> a + b + Void -> a * b
fix2 x y = case _ of
  Left x' -> x' ~ y
  Right (Left y') -> x ~ y'
  Right (Right contra) -> absurd contra

fix3 :: forall a b c. a -> b -> c -> a + b + c + Void -> a * b * c
fix3 x y z = case _ of
  Left x' -> x' ~ y ~ z
  Right (Left y') -> x ~ y' ~ z
  Right (Right (Left z')) -> x ~ y ~ z'
  Right (Right (Right contra)) -> absurd contra
