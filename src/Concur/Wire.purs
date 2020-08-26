module Concur.Wire
  ( Wire,
    value,
    send,
    with,
    focus,
  )
where

import Concur.Core (Widget, liftSafeBlockingIO, liftUnsafeBlockingIO)
import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TChan (newTChan, readTChan, writeTChan)
import Lens.Simple (Lens')
import qualified Lens.Simple as Lens

data Wire a
  = forall s. Wire s (TChan s) (Lens' s a)

-- | Get the current value on the wire
value :: Wire a -> a
value (Wire store _ lens) = Lens.view lens store

-- | Send a value through the wire
send :: Wire a -> a -> Widget v Unit
send (Wire store chan lens) x =
  liftUnsafeBlockingIO <| atomically <| writeTChan chan <| Lens.set lens x store

-- | Setup a local environment
with :: Monoid v => a -> (Wire a -> Widget v r) -> Widget v r
with x use = do
  chan <- liftUnsafeBlockingIO <| atomically newTChan
  go (Wire x chan _identity)
  where
    go wire@(Wire _ chan lens) = do
      r <- Left <|| use wire <|> Right <|| receive
      either pure (\x' -> go <| Wire x' chan lens) r
      where
        receive = liftSafeBlockingIO <| atomically <| readTChan chan

-- | Focus on some part of data on the wire
focus :: Lens' s a -> Wire s -> Wire a
focus lens' (Wire store chan lens) = Wire store chan (lens << lens')

_identity :: Lens' a a
_identity = Lens.iso identity identity
