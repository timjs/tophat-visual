module Task.Script.Label
  -- # Labels
  ( Labeled
  , Labels
  , Name
  , Label
  , showLabels
  , showFields
  , showVariants
  , Message
  ) where

import Preload

import Data.HashMap as HashMap
import Data.HashSet as HashSet

---- Labels --------------------------------------------------------------------

type Labeled a
  = HashMap Label a

showRow :: forall a. Show a => Char -> Char -> String -> Labeled a -> String
showRow beg end sep as =
  as
    |> HashMap.toArrayBy check
    |> intercalate ", "
    |> enclose beg end
  where
  check l x =
    let
      r = show x
    in
      if l == r then
        l
      else
        unwords [ l, sep, r ]

showFields :: forall a. Show a => String -> Labeled a -> String
showFields = showRow '{' '}'

showVariants :: forall a. Show a => Labeled a -> String
showVariants = showRow '[' ']' ":"

type Labels
  = HashSet Label

showLabels :: HashSet Label -> String
showLabels = HashSet.toArray >> intercalate "," >> enclose '{' '}'

type Name
  = String

type Label
  = String

type Message
  = String
