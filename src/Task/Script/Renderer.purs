module Task.Script.Renderer where

import Preload

import Concur as Concur
import Concur.Dom (Widget)
import Concur.Dom.Attr as Attr
import Concur.Dom.Icon as Icon
import Concur.Dom.Input as Input
import Concur.Dom.Style (Button(..), Kind(..), Position(..), Size(..), Stroke(..), Style(..))
import Concur.Dom.Style as Layout
import Concur.Dom.Text as Text
import Data.Array as Array
import Data.Either.Nested as Either
import Data.HashMap as HashMap
import Task.Script.Annotation (Annotated(..), Checked, Status(..))
import Task.Script.Context (Context, Typtext, aliases)
import Task.Script.Loader (validate)
import Task.Script.Syntax (Arguments(..), BasicType, Branches, Expression(..), Constant(..), Label, LabeledBranches, Match(..), Name, Row_, Task(..))

---- Rendering -----------------------------------------------------------------

type Renderer = Checked Task -> Widget (Checked Task)

main :: Context -> Typtext -> Checked Task -> Widget (Checked Task)
main g s t =
  Concur.repeat t \t' ->
    let
      t'' = validate s g t'
    in
      Layout.column
        [ renderTask g s t''
        , Text.code "TopHat" (show t'')
        ]

renderTask :: Context -> Typtext -> Checked Task -> Widget (Checked Task)
renderTask g s = go
  where
  go :: Checked Task -> Widget (Checked Task)
  go (Annotated a_t t) = case t of
    ---- Steps
    --INVARIANT third arg of `Step` is always Branch or Select

    Step m t1 (Annotated a_b (Branch [ Constant (B true) ~ Annotated a_l (Lift e) ])) -> do
      m' ~ t1' <- renderEnd go m t1
      done <| Annotated a_t (Step m' t1' (Annotated a_b (Branch [ Constant (B true) ~ Annotated a_l (Lift e) ])))

    Step m t1 (Annotated a_b (Branch [ Constant (B true) ~ t2 ])) -> do
      c' ~ m' ~ t1' ~ t2' <- renderSingle go Hurry m t1 t2
      done <| Annotated a_t <| Step m' t1' <| Annotated a_b case c' of
        Hurry -> Branch [ Constant (B true) ~ t2' ]
        Delay -> Select [ "Continue" ~ Constant (B true) ~ t2' ]
    Step m t1 (Annotated a_b (Branch bs)) -> do
      c' ~ m' ~ t1' ~ bs' <- renderBranches go m t1 bs
      done <| Annotated a_t <| Step m' t1' <| Annotated a_b <| case c' of
        Hurry -> Branch bs'
        Delay -> Select (addLabels bs')

    Step m t1 (Annotated a_b (Select [ "Continue" ~ Constant (B true) ~ t2 ])) -> do
      c' ~ m' ~ t1' ~ t2' <- renderSingle go Delay m t1 t2
      done <| Annotated a_t <| Step m' t1' <| Annotated a_b <| case c' of
        Hurry -> Branch [ Constant (B true) ~ t2' ]
        Delay -> Select [ "Continue" ~ Constant (B true) ~ t2' ]
    Step m t1 (Annotated a_b (Select bs)) -> do
      c' ~ m' ~ t1' ~ bs' <- renderSelects go m t1 bs
      done <| Annotated a_t <| Step m' t1' <| Annotated a_b <| case c' of
        Hurry -> Branch (removeLabels bs')
        Delay -> Select bs'

    Step _ _ _ -> panic "invalid single step"
    -- m' ~ t1' ~ t2' <- renderSingle Hurry go m t1 t2
    -- done <| Annotated a_t (Step m' t1' t2')
    Branch _ -> panic "invalid single branch"
    Select _ -> panic "invalid single select"
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
      t' <- renderGroup And go ts
      done <| Annotated a_t t'
    Choose ts -> do
      t' <- renderGroup Or go ts
      done <| Annotated a_t t'

    ---- Extras
    Execute n as -> do
      n' ~ as' <- renderExecute g a_t n as
      done <| Annotated a_t (Execute n' as')
    Hole as -> do
      n' ~ as' <- renderExecute g a_t "??" as
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
renderExecute :: Context -> Status -> Name -> Arguments -> Widget (Name * Arguments)
renderExecute context status name args@(ARecord row) =
  Layout.column
    [ renderLine row >-> (ARecord >> Either.in2)
    , renderError status
        ( Input.picker
            [ "Builtin" ~ []
            , "Project" ~ Array.sort (HashMap.keys context)
            ]
            name
        ) >-> Either.in1
    ]
    >-> fix2 name args

renderLine :: forall a. Row_ a -> Widget (Row_ a)
renderLine row =
  Layout.line Solid [ Layout.place After [ renderLabels row ] ]

-- | || (( a_1 .. a_n ))
renderLabels :: forall a. Row_ a -> Widget (Row_ a)
renderLabels =
  HashMap.keys >> map (Layout.chip Normal) >> Layout.row

---- Steps ----

-- |   || as
-- |   V
renderStep :: Cont -> Match -> Widget (Cont * Match)
renderStep cont match@(MRecord row) =
  Layout.column
    [ renderLine row >-> (MRecord >> Either.in2)
    , Layout.element [ void Attr.onDoubleClick ->> (switch cont |> Either.in1) ] [ Layout.triangle (style cont) empty ]
    ]
    >-> fix2 cont match
renderStep _ _ = todo "other matches in step rendering"

renderOption :: Status -> Expression -> Widget Expression
renderOption status guard =
  Layout.line Dashed
    [ Layout.place After [ renderGuard status guard ] ]

renderOptionWithLabel :: Status -> Label -> Expression -> Widget (Label * Expression)
renderOptionWithLabel status label guard =
  Layout.line Dashed
    [ Layout.place After [ renderLabel label >-> Either.in1, renderGuard status guard >-> Either.in2 ]
    -- , Layout.place Before [  ] >-> Either.in1
    ]
    >-> fix2 label guard

renderSingle :: forall a. (a -> Widget a) -> Cont -> Match -> a -> a -> Widget (Cont * Match * a * a)
renderSingle render cont match sub1 sub2 =
  Layout.column
    [ render sub1 >-> Either.in1
    , renderStep cont match >-> Either.in3
    , render sub2 >-> Either.in2
    ]
    >-> fix3 sub1 sub2 (cont ~ match)
    >-> reorder4

renderEnd :: forall a. (a -> Widget a) -> Match -> a -> Widget (Match * a)
renderEnd render args@(MRecord row) subtask =
  Layout.column
    [ render subtask >-> Either.in2
    , renderLine row >-> (MRecord >> Either.in1)
    ]
    >-> fix2 args subtask
renderEnd _ _ _ = todo "other matches in end rendering"

---- Branches ----

renderBranches :: Renderer -> Match -> Checked Task -> Branches (Checked Task) -> Widget (Cont * Match * Checked Task * Branches (Checked Task))
renderBranches render match subtask branches =
  Layout.column
    [ render subtask >-> Either.in1
    , renderStep Hurry match >-> Either.in3
    , Layout.branch [ Concur.traverse (renderBranch render) branches >-> Either.in2 ]
    ]
    >-> fix3 subtask branches (Hurry ~ match)
    >-> reorder4

-- renderSingleBranch :: Renderer -> Match -> Checked Task -> Expression * Checked Task -> Widget (Match * Checked Task * Checked Task)
-- renderSingleBranch render match sub1 (guard ~ sub2) =
--   Layout.column
--     [ render sub1 >-> Either.in2
--     , renderStep Hurry match >-> Either.in1
--     , render sub2
--     ]
--     >-> fix3 match sub1 sub2

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

renderSelects :: Renderer -> Match -> Checked Task -> LabeledBranches (Checked Task) -> Widget (Cont * Match * Checked Task * LabeledBranches (Checked Task))
renderSelects render match subtask branches =
  Layout.column
    [ render subtask >-> Either.in1
    , renderStep Delay match >-> Either.in3
    , Layout.branch [ Concur.traverse (renderSelect render) branches ] >-> Either.in2
    ]
    >-> fix3 subtask branches (Delay ~ match)
    >-> reorder4

renderSelect :: Renderer -> Label * Expression * Checked Task -> Widget (Label * Expression * Checked Task)
renderSelect render (label ~ guard ~ subtask@(Annotated status _)) =
  Layout.column
    [ renderOptionWithLabel status label guard >-> Either.in2
    -- , Layout.line Solid empty
    , render subtask >-> Either.in1
    , Layout.line Solid empty
    ]
    >-> fix2 subtask (label ~ guard)
    >-> reorder3

---- Combinators ----

-- | ==============
-- |  t_1 ... t_n
-- | =============
-- renderGroup :: forall a. Stroke -> (a -> Widget a) -> Array a -> Widget (Array a)
renderGroup :: Par -> (Checked Task -> Widget (Checked Task)) -> Array (Checked Task) -> Widget (Task (Checked Task))
renderGroup par trans tasks =
  Layout.element [ void Attr.onDoubleClick ->> other par tasks ]
    [ Layout.group (stroke par)
        [ Concur.traverse trans tasks >-> this par
        --TODO: add branch
        , Input.button Action Secondary Small "+" ->> this par tasks
        ]
    ]

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
renderError (Failure _ err) w =
  Layout.has Error [ Input.tooltip Before (show err) w ]
renderError _ w =
  Layout.has Normal [ w ]

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
fix2 _1 _2 = case _ of
  Left _1' -> _1' ~ _2
  Right (Left _2') -> _1 ~ _2'
  Right (Right contra) -> absurd contra

fix3 :: forall a b c. a -> b -> c -> a + b + c + Void -> a * b * c
fix3 _1 _2 z = case _ of
  Left _1' -> _1' ~ _2 ~ z
  Right (Left _2') -> _1 ~ _2' ~ z
  Right (Right (Left _3')) -> _1 ~ _2 ~ _3'
  Right (Right (Right contra)) -> absurd contra

fix4 :: forall a b c d. a -> b -> c -> d -> a + b + c + d + Void -> a * b * c * d
fix4 _1 _2 _3 _4 = case _ of
  Left _1' -> _1' ~ _2 ~ _3 ~ _4
  Right (Left _2') -> _1 ~ _2' ~ _3 ~ _4
  Right (Right (Left _3')) -> _1 ~ _2 ~ _3' ~ _4
  Right (Right (Right (Left _4'))) -> _1 ~ _2 ~ _3 ~ _4'
  Right (Right (Right (Right contra))) -> absurd contra

reorder3 (a ~ b ~ c) = b ~ c ~ a
reorder4 (a ~ b ~ c ~ d) = (c ~ d ~ a ~ b)

data Par = And | Or

this :: forall a. Par -> Array a -> Task a
this And = Pair
this Or = Choose

other :: forall a. Par -> Array a -> Task a
other And = Choose
other Or = Pair

stroke :: Par -> Stroke
stroke And = Solid
stroke Or = Double

data Cont = Hurry | Delay

style :: Cont -> Style
style Hurry = Filled
style Delay = Outlined

class Switch a where
  switch :: a -> a

instance Switch Par where
  switch And = Or
  switch Or = And

instance Switch Cont where
  switch Hurry = Delay
  switch Delay = Hurry

addLabels = map ("" ~ _)
removeLabels = map snd