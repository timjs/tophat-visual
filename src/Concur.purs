module Concur
  -- # Widgets
  ( Widget
  -- # Wires
  , Wire
  , local
  , focus
  ) where

import Preload
import Concur.Core as Core
import Concur.Core.Patterns as Patterns
import Data.Lens (Lens')
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

---- Widgets -------------------------------------------------------------------
type Widget
  = Core.Widget

---- Wires ---------------------------------------------------------------------
type Wire m a
  = Patterns.Wire m a

local :: forall m r a. Alt m => MonadEffect m => MonadAff m => Plus m => a -> (Wire m a -> m r) -> m r
local = Patterns.local

focus :: forall m s a. Functor m => Lens' s a -> Wire m s -> Wire m a
focus = Patterns.mapWire

---- Combinators ---------------------------------------------------------------
{-
import Concur.Core (Widget, orr)
import qualified Data.List.Index as List

loop :: Monad m => (a -> m a) -> a -> m void
loop f = g
  where
    g x = f x >>= g

andd :: Monoid v => List (Widget v a) -> Widget v (List a)
andd ws = do
  (i, e) <- List.ifoldr (\i w r -> map (i,) w <|> r) empty ws
  let ws' = List.deleteAt i ws
  if length ws' <= 0
    then pure [e]
    else do
      rest <- andd ws'
      pure <| List.insertAt i e rest

list' :: Monoid v => (a -> Widget v a) -> List a -> Widget v (List a)
list' render elements = do
  (index, result) <- orr indexedElements
  pure <| List.setAt index result elements
  where
    indexedElements =
      List.imap (\index element -> (index,) <|| render element) <| elements

list :: Monoid v => (a -> Widget v (Maybe a)) -> List a -> Widget v (List a)
list render elements = do
  (index, result) <- orr indexedElements
  pure
    <| case result of
      Nothing -> List.deleteAt index elements
      Just element' -> List.setAt index element' elements
  where
    indexedElements =
      List.imap (\index element -> (index,) <|| render element) <| elements
-}
