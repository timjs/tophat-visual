module Concur
  -- # Wires
  ( Wire
  , local
  , focus
  ) where

import Preload
import Concur.Core.Patterns as Concur
import Data.Lens (Lens')
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

type Wire m a
  = Concur.Wire m a

local :: forall m r a. Alt m => MonadEffect m => MonadAff m => Plus m => a -> (Wire m a -> m r) -> m r
local = Concur.local

focus :: forall m s a. Functor m => Lens' s a -> Wire m s -> Wire m a
focus = Concur.mapWire
