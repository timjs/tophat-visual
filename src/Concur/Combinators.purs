module Concur.Combinators where

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
