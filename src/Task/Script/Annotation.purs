module Task.Script.Annotation where

import Preload

import Data.Doc (class Display, display)
import Data.Doc as Doc
import Data.HashMap as HashMap
import Task.Script.Context (Context)
import Task.Script.Error (Error(..))
import Task.Script.Syntax (Task, Type_)

---- Annotations ---------------------------------------------------------------

data Annotated a f =
  Annotated a (f (Annotated a f))

---- Checked -------------------------------------------------------------------

type Checked = Annotated Status

data Status
  = Success Context Type_
  | Failure Context Error
  | Unknown

instance Display (Checked Task) where
  display (Annotated s t) = case s of
    Success _ a -> Doc.words [ Doc.text "(", display t, Doc.text ":", Doc.show a, Doc.text ")" ]
    Failure _ e -> Doc.words [ Doc.text "(!", display t, Doc.text ":", Doc.show e, Doc.text "!)" ]
    Unknown -> Doc.words [ Doc.text "(?", display t, Doc.text "?)" ]

instance Show (Checked Task) where
  show = display >> Doc.render

---- Helpers -------------------------------------------------------------------

unchecked :: forall f. f (Checked f) -> Checked f
unchecked = Annotated Unknown

lift :: forall f. Context -> f (Checked f) -> f (Checked f) -> Error + Type_ -> Checked f
lift g u c = case _ of
  Left e -> Annotated (Failure g e) u
  Right a -> Annotated (Success g a) c

sink :: forall f. Context -> f (Checked f) -> Error + Type_ -> Checked f
sink g c = case _ of
  Left _ -> Annotated Unknown c
  Right a -> Annotated (Success g a) c

fail :: forall f. Context -> f (Checked f) -> Error -> Checked f
fail g u e = Annotated (Failure g e) u

pass :: forall f. Context -> f (Checked f) -> Type_ -> Checked f
pass g c a = Annotated (Success g a) c

bury :: forall f. f (Checked f) -> Checked f
bury = Annotated Unknown

extractType :: forall f. Checked f -> Error + Type_
extractType = case _ of
  Annotated (Success _ t) _ -> Right t
  _ -> Left UndeterminedType

withTypeOf :: forall f. Checked f -> f (Checked f) -> (Type_ -> Checked f) -> Checked f
withTypeOf c b f = case c of
  Annotated (Success _ t) _ -> f t
  _ -> Annotated Unknown b

extractError :: Status -> Maybe String
extractError (Failure _ e) = Just <| show e
extractError _ = Nothing

extractContext :: Status -> Context
extractContext (Failure g _) = g
extractContext (Success g _) = g
extractContext (Unknown) = HashMap.empty