module Task.Script.Knot
  -- # Checked or Unchecked
  ( Unchecked(..)
  , Checked(..)
  , lift
  , sink
  , pass
  , fail
  , bury
  , extract
  , withTypeOf
  -- , annotate
  ) where

import Preload

import Data.Doc (class Display, display)
import Data.Doc as Doc
import Task.Script.Syntax (Task, Type_)
import Task.Script.Context (Context)
import Task.Script.Error (Error(..))

---- Checked or Unchecked ------------------------------------------------------
data Unchecked f
  = Unchecked (f (Unchecked f))

--NOTE: below instance is infinite, we create one for Tasks only
-- instance Show (f (Unchecked f)) => Show (Unchecked f) where
instance Display (Unchecked Task) where
  display (Unchecked x) = display x

instance Show (Unchecked Task) where
  show = display >> Doc.render

data Checked f
  = Fail Context Error (f (Unchecked f))
  | Bury (f (Checked f))
  | Pass Context Type_ (f (Checked f))

instance Display (Checked Task) where
  display = case _ of
    Fail _ e u -> Doc.words [ Doc.text "(!", display u, Doc.text ":", Doc.show e, Doc.text "!)" ]
    Bury c -> Doc.words [ Doc.text "(?", display c, Doc.text "?)" ]
    Pass _ a c -> Doc.words [ Doc.text "(", display c, Doc.text ":", Doc.show a, Doc.text ")" ]

instance Show (Checked Task) where
  show = display >> Doc.render

lift :: forall f. Context -> f (Unchecked f) -> f (Checked f) -> Error + Type_ -> Checked f
lift g u c = case _ of
  Left e -> Fail g e u
  Right a -> Pass g a c

sink :: forall f. Context -> f (Checked f) -> Error + Type_ -> Checked f
sink g c = case _ of
  Left _ -> Bury c
  Right a -> Pass g a c

fail :: forall f. Context -> f (Unchecked f) -> Error -> Checked f
fail g u e = Fail g e u

pass :: forall f. Context -> f (Checked f) -> Type_ -> Checked f
pass g c a = Pass g a c

bury :: forall f. f (Checked f) -> Checked f
bury = Bury

extract :: forall f. Checked f -> Error + Type_
extract = case _ of
  Pass _ t _ -> Right t
  _ -> Left UndeterminedType

withTypeOf :: forall f. Checked f -> f (Checked f) -> (Type_ -> Checked f) -> Checked f
withTypeOf c b f = case c of
  Pass _ t _ -> f t
  _ -> Bury b

-- annotate :: Unchecked Task -> (Task (Unchecked Task) -> Error + (Task (Checked Task) ~> Type_)) -> Checked Task
-- annotate (Unchecked u) f = case f u of
--   Left x -> Fail x u
--   Right (c ~> t) -> Pass t c
-- annotate :: Unchecked Task -> (Task (Unchecked Task) -> Error + (Task (Checked Task) ~> Type_)) -> Checked Task
-- annotate (Unchecked u) f = case f u of
--   Left x -> Fail x u
--   Right (c ~> t) -> Pass t c
-- replace :: Error -> Checked Task -> Checked Task
-- replace x = case _ of
--   Fail _ c -> Fail x c
--   Pass _ c -> Pass x c