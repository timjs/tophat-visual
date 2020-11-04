module Concur
  ( module Reexport
  -- # Widgets
  , combine
  -- , choose
  -- # Signals
  , repeat
  , loop
  , dynamic
  -- # Wires
  , Wire
  , local
  , focus
  -- # Combinators
  , list
  ) where

import Preload
import Control.Cofree (Cofree)
import Concur.Core.Types as Internal
import Concur.Core.FRP (Signal, display, step, always, update, poll, hold, foldp) as Reexport
import Concur.Core.FRP as Internal
import Concur.Core.Patterns as Internal
import Concur.Core.Types (Widget) as Reexport
import Control.MultiAlternative (class MultiAlternative) as Reexport
import Control.MultiAlternative as Internal
import Control.ShiftMap (class ShiftMap) as Reexport
import Data.Array as Array
import Data.Lens (Lens')
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

---- Widgets -------------------------------------------------------------------
-- | Combine multiple widgets in parallel until all finish, and collect their outputs.
combine :: forall v a. Monoid v => Array (Reexport.Widget v a) -> Reexport.Widget v (Array a)
combine = Internal.andd

choose :: forall a m. Reexport.MultiAlternative m => Array (m a) -> m a
choose = Internal.orr

---- Signals -------------------------------------------------------------------
-- | Turn a (closed) signal into a widget.
dynamic :: forall m a b. Monad m => Cofree m a -> m b
dynamic = Internal.dyn

-- | Repeatedly invoke a widget function for values to create a signal, looping in the previous value.
repeat :: forall m a. Monad m => a -> (a -> m a) -> Cofree m a
repeat = Internal.loopW

-- | Loop a signal so that the return value is passed to the beginning again.
loop :: forall m a. Monad m => a -> (a -> Cofree m a) -> Cofree m a
loop = Internal.loopS

---- Wires ---------------------------------------------------------------------
type Wire m a
  = Internal.Wire m a

local :: forall m r a. Alt m => MonadEffect m => MonadAff m => Plus m => a -> (Wire m a -> m r) -> m r
local = Internal.local

focus :: forall m s a. Functor m => Lens' s a -> Wire m s -> Wire m a
focus = Internal.mapWire

---- Combinators ---------------------------------------------------------------
list :: forall v a. Monoid v => (a -> Reexport.Widget v (Maybe a)) -> Array a -> Reexport.Widget v (Array a)
list render elements = do
  (index ** result) <- Internal.orr indexedElements
  done
    <| case result of
        Nothing -> Array.deleteAt index elements ?? elements
        Just element' -> Array.updateAt index element' elements ?? elements
  where
  indexedElements = Array.mapWithIndex (\index element -> (index ** _) <|| render element) <| elements

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

-}
