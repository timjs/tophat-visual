module Data.Doc
  ( module Reexport
  , Lines(..)
  , class Display
  , display
  , show
  , space
  , line
  , comma
  , columns
  , lines
  , words
  , indentWith
  , indent
  , enclose
  , parens
  , brackets
  , braces
  , quotes
  , ticks
  , backticks
  , list
  , tuple
  , record
  ) where

import Preload hiding (show, enclose, empty)
import Preload as Preload
import Text.Pretty (Doc, Columns(..), text, empty, atop)
import Text.Pretty (Doc, Columns(..), text, render) as Reexport

---- Texts ---------------------------------------------------------------------
class Display a where
  display :: a -> Doc

show :: forall a. Show a => a -> Doc
show = Preload.show .> text

---- Characters ----------------------------------------------------------------
space :: Columns
space = wrap <| empty 1 0

line :: Doc
line = empty 0 1

comma :: Columns
comma = wrap <| text ","

---- Combinators ---------------------------------------------------------------
---- Concating ----
-- hcat <=> columns
-- hsep <=> words
-- vcat <=> lines
columns :: forall f. Foldable f => f Doc -> Doc
columns = ala Columns foldMap

lines :: forall f. Foldable f => f Doc -> Doc
lines = ala Lines foldMap

words :: forall f. Traversable f => f Doc -> Doc
words = using Columns (intercalate space)

---- Indenting ----
indentWith :: Int -> Doc -> Doc
indentWith n d = columns [ empty n 0, d ]

indent :: Doc -> Doc
indent = indentWith 2

---- Enclosing ----
enclose :: Doc -> Doc -> Doc -> Doc
enclose l r d = columns [ l, d, r ]

parens :: Doc -> Doc
parens = enclose (text "(") (text ")")

brackets :: Doc -> Doc
brackets = enclose (text "[") (text "]")

braces :: Doc -> Doc
braces = enclose (text "{") (text "}")

quotes :: Doc -> Doc
quotes = enclose (text "\"") (text "\"")

ticks :: Doc -> Doc
ticks = enclose (text "'") (text "'")

backticks :: Doc -> Doc
backticks = enclose (text "`") (text "`")

tuple :: forall f. Traversable f => f Doc -> Doc
tuple = parens <. using Columns (intercalate comma)

list :: forall f. Traversable f => f Doc -> Doc
list = brackets <. using Columns (intercalate comma)

record :: forall f. Traversable f => Doc -> f (Doc ** Doc) -> Doc
record sep = braces <. using Columns (intercalate comma) <. map (\(k ** v) -> columns [ k, sep, v ])

---- Wrappers ------------------------------------------------------------------
-- | A wrapper for `Doc` with a `Monoid` instance which stacks documents as lines.
newtype Lines
  = Lines Doc

derive instance newtypeLines :: Newtype Lines _

instance semigroupLines :: Semigroup Lines where
  append (Lines d1) (Lines d2) = Lines (d1 `atop` d2)

instance monoidLines :: Monoid Lines where
  mempty = Lines (empty 0 0)
