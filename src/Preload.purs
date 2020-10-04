module Preload
  ( module Reexport
  -- Booleans
  , Bool
  , (&&&)
  , composeAnd
  , (|||)
  , composeOr
  -- Tuples
  , (**)
  , type (**)
  -- Eithers
  , type (++)
  , throw
  -- Maybes
  , (??)
  , withDefault
  -- Strings
  , inbetween
  , quote
  , indent
  -- Arrays
  , Slice
  , slice
  , uncons
  , unsnoc
  , tail
  , init
  , take
  , takeEnd
  , takeWhile
  , drop
  , dropEnd
  , dropWhile
  , span
  , group
  , groupBy
  , class Initialise
  , from
  -- Functions
  , (<<)
  , (>>)
  , (|>)
  , (<|)
  , undefined
  -- Enums
  , (..)
  -- Naturals
  , Nat
  , nat
  , unnat
  -- Hashables
  , (=<)
  , (/<)
  , notMember
  -- Semigroups, Monoids, Groups, Modules, Torsors
  , (++)
  , neutral
  , class Group
  , invert
  , subtract
  , (~~)
  , class Module
  , scale
  , class Torsor
  , diff
  , adjust
  -- Foldables, Traversables, Unfoldables
  , foldlInfix
  , (/:)
  , foldrInfix
  , (:\)
  , foldr1
  , gather
  , words
  , lines
  , unwords
  , unlines
  -- Functors, Applicatives, Alternatives, Monads
  , (<||)
  , (||>)
  , (-||)
  , (||-)
  , done
  , (-<)
  , (-|)
  , (|-)
  , skip
  , pair
  , (<>)
  , (|=)
  , (=|)
  -- Newtypes
  , using
  ) where

import Control.Applicative (pure)
import Control.Apply (applyFirst, applySecond) as Reexport
import Control.Bind (bind, bindFlipped, discard) as Reexport
import Control.Plus (class Plus, empty) as Reexport
import Control.Alt (class Alt, (<|>)) as Reexport
import Control.Semigroupoid (composeFlipped)
import Data.Array as Array
import Data.Array.NonEmpty (fromNonEmpty, toUnfoldable)
import Data.ArrayView as Slice
import Data.ArrayView.Internal (fromNonEmptyArray)
import Data.ArrayView.Internal as Slice
import Data.Bifoldable as Reexport
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap) as Reexport
import Data.Either hiding (Either) as Reexport
import Data.Either (Either)
import Data.Enum (class Enum) as Reexport
import Data.Enum (enumFromTo)
import Data.Foldable (foldM)
import Data.Foldable hiding (foldM) as Reexport
import Data.FoldableWithIndex hiding (foldlDefault, foldrDefault, foldMapDefault) as Reexport
import Data.Function (apply, applyFlipped)
import Data.Functor (mapFlipped, voidLeft, voidRight) as Reexport
import Data.HashMap (HashMap) as Reexport
import Data.HashMap as HashMap
import Data.HashSet (HashSet) as Reexport
import Data.HashSet as HashSet
import Data.Hashable (class Hashable) as Reexport
import Data.List (List) as Reexport
import Data.List as List
import Data.Maybe hiding (fromMaybe) as Reexport
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, wrap, unwrap, ala, over, over2, under, under2) as Reexport
import Data.NonEmpty (NonEmpty(..)) as Reexport
import Data.String.CodeUnits as String
import Data.String.Common as String
import Data.String.Pattern as String
import Data.Traversable as Reexport
import Data.Tuple (curry, fst, lookup, snd, swap, uncurry, Tuple(..)) as Reexport
-- import Data.Tuple.Nested ((/\), type (/\)) as Reexport
import Data.Unfoldable as Reexport
import Effect (Effect) as Reexport
import Prelude hiding (mempty, pure, (<<<), (>>>), (<>), ($), (#), (<$>), (<#>), (<@>), (<$), ($>), (<*>), (<*), (*>)) as Reexport
import Prim.TypeError (class Warn, Text)
import Unsafe.Coerce (unsafeCoerce)

---- Booleans ------------------------------------------------------------------
type Bool
  = Boolean

infixr 3 composeAnd as &&&

infixr 2 composeOr as |||

composeAnd :: forall a. (a -> Bool) -> (a -> Bool) -> a -> Bool
composeAnd f g x = f x Reexport.&& g x

composeOr :: forall a. (a -> Bool) -> (a -> Bool) -> a -> Bool
composeOr f g x = f x Reexport.|| g x

---- Tuples --------------------------------------------------------------------
infixr 2 Reexport.Tuple as **

infixr 2 type Reexport.Tuple as **

---- Eithers -------------------------------------------------------------------
infixr 1 type Reexport.Either as ++

throw :: forall x a. x -> Reexport.Either x a
throw = Reexport.Left

---- Maybes --------------------------------------------------------------------
infix 2 withDefault as ??

withDefault :: forall a. Reexport.Maybe a -> a -> a
withDefault = Reexport.flip fromMaybe

---- Strings -------------------------------------------------------------------
inbetween :: Char -> Char -> String -> String
inbetween a b s = String.singleton a ++ s ++ String.singleton b

quote :: String -> String
quote = inbetween '"' '"'

indent :: Int -> String -> String
indent n s = Reexport.fold (Reexport.replicate n "  " :: Array String) ++ s

---- Arrays --------------------------------------------------------------------
type Slice
  = Slice.ArrayView

-- *O(1)*
slice :: forall a. Int -> Int -> Array a -> Slice a
slice n m = Slice.fromArray >> Slice.slice n m

-- *O(1)*
uncons :: forall a. Array a -> Reexport.Maybe { head :: a, tail :: Slice a }
uncons = Slice.fromArray >> Slice.uncons

-- *O(1)*
unsnoc :: forall a. Array a -> Reexport.Maybe { init :: Slice a, last :: a }
unsnoc = Slice.fromArray >> Slice.unsnoc

-- *O(1)*
tail :: forall a. Array a -> Reexport.Maybe (Slice a)
tail = Slice.fromArray >> Slice.tail

-- *O(1)*
init :: forall a. Array a -> Reexport.Maybe (Slice a)
init = Slice.fromArray >> Slice.init

-- *O(1)*
take :: forall a. Int -> Array a -> Slice a
take n = Slice.fromArray >> Slice.take n

-- *O(1)*
takeEnd :: forall a. Int -> Array a -> Slice a
takeEnd n = Slice.fromArray >> Slice.takeEnd n

-- *O(m)*
takeWhile :: forall a. (a -> Boolean) -> Array a -> Slice a
takeWhile f = Slice.fromArray >> Slice.takeWhile f

-- *O(1)*
drop :: forall a. Int -> Array a -> Slice a
drop n = Slice.fromArray >> Slice.drop n

-- *O(1)*
dropEnd :: forall a. Int -> Array a -> Slice a
dropEnd n = Slice.fromArray >> Slice.dropEnd n

-- *O(m)*
dropWhile :: forall a. (a -> Boolean) -> Array a -> Slice a
dropWhile f = Slice.fromArray >> Slice.dropWhile f

-- *O(m)*
span :: forall a. (a -> Boolean) -> Array a -> { init :: Slice a, rest :: Slice a }
span f = Slice.fromArray >> Slice.span f

group :: forall a. Reexport.Eq a => Array a -> Array (Reexport.NonEmpty Slice a)
group = Slice.fromArray >> Slice.group >> Reexport.map fromNonEmptyArrayView >> Slice.toArray

groupBy :: forall a. (a -> a -> Boolean) -> Array a -> Array (Reexport.NonEmpty Slice a)
groupBy f = Slice.fromArray >> Slice.groupBy f >> Reexport.map fromNonEmptyArrayView >> Slice.toArray

-- INTERNAL USE ONLY --
fromNonEmptyArrayView :: forall a. Slice.NonEmptyArrayView a -> Reexport.NonEmpty Slice a
fromNonEmptyArrayView (Slice.NonEmptyArrayView xs) = xs

class Initialise c i | c -> i where
  -- type Item c = i
  from :: Array i -> c

instance initialiseList :: Initialise (Reexport.List a) a where
  from = List.fromFoldable

instance initialiseHashSet :: Reexport.Hashable a => Initialise (Reexport.HashSet a) a where
  from = HashSet.fromArray

instance initialiseHashMap :: Reexport.Hashable k => Initialise (Reexport.HashMap k v) (k ** v) where
  from = HashMap.fromArray

---- Functions -----------------------------------------------------------------
infixr 9 Reexport.compose as <<

infixr 9 composeFlipped as >>

infixr 0 apply as <|

infixl 1 applyFlipped as |>

undefined :: forall a. Warn (Text "Undefined function in code") => a
undefined = unsafeCoerce Reexport.unit

---- Enums ---------------------------------------------------------------------
infix 8 enumFromTo as ..

---- Natural numbers -----------------------------------------------------------
newtype Nat
  = Nat Int

nat :: Int -> Nat
nat i
  | i Reexport.>= 0 = Nat i
  | Reexport.otherwise = Nat 0

unnat :: Nat -> Int
unnat (Nat i) = i

instance showNat :: Reexport.Show Nat where
  show (Nat i) = "+" ++ Reexport.show i

instance eqNat :: Reexport.Eq Nat where
  eq (Nat i) (Nat j) = i Reexport.== j

instance ordNat :: Reexport.Ord Nat where
  compare (Nat i) (Nat j) = Reexport.compare i j

---- Hashables -----------------------------------------------------------------
infix 4 HashSet.member as =<

infix 4 notMember as /<

notMember :: forall a. Reexport.Hashable a => a -> Reexport.HashSet a -> Bool
notMember x = Reexport.not << HashSet.member x

---- Semigroups, Monoids, Groups, Modules, Torsors -----------------------------
infixr 5 Reexport.append as ++

infixr 5 subtract as ~~

neutral :: forall m. Reexport.Monoid m => m
neutral = mempty

class
  (Reexport.Monoid a) <= Group a where
  invert :: a -> a
  subtract :: a -> a -> a

invertDefault :: forall a. Group a => a -> a
invertDefault x = neutral ~~ x

subtractDefault :: forall a. Group a => a -> a -> a
subtractDefault x y = x ++ invert y

class
  (Group a, Reexport.Semiring s) <= Module a s | a -> s where
  -- type Factor a = s
  scale :: s -> a -> a

class
  (Group d) <= Torsor a d | a -> d where
  -- type Difference a = d
  diff :: a -> a -> d
  adjust :: d -> a -> a

---- Foldables, Traversables ---------------------------------------------------
infix 4 foldlInfix as /:

infix 4 foldrInfix as :\

foldlInfix :: forall f a b. Reexport.Foldable f => b -> f a -> (b -> a -> b) -> b
foldlInfix x xs f = Reexport.foldl f x xs

foldrInfix :: forall f a b. Reexport.Foldable f => f a -> b -> (a -> b -> b) -> b
foldrInfix xs x f = Reexport.foldr f x xs

foldr1 :: forall f a. Reexport.Foldable f => (a -> a -> a) -> f a -> Reexport.Maybe a
foldr1 f xs = Reexport.foldr mf Reexport.Nothing xs
  where
  mf x m =
    Reexport.Just
      <| case m of
          Reexport.Nothing -> x
          Reexport.Just y -> f x y

gather :: forall f m a b. Reexport.Foldable f => Reexport.Monad m => (a -> b -> m a) -> a -> f b -> m a
gather = foldM

words :: forall f. Reexport.Unfoldable f => String -> f String
words = String.split (String.Pattern " ") >> Array.toUnfoldable

lines :: forall f. Reexport.Unfoldable f => String -> f String
lines = String.split (String.Pattern "\n") >> Array.toUnfoldable

unwords :: forall f. Reexport.Foldable f => f String -> String
unwords = Reexport.intercalate " "

unlines :: forall f. Reexport.Foldable f => f String -> String
unlines = Reexport.intercalate "\n"

---- Functors, Applicatives, Applicatives, Monads ------------------------------
infixl 4 Reexport.map as <||

infixl 1 Reexport.mapFlipped as ||>

infixl 4 Reexport.voidRight as -||

infixl 4 Reexport.voidLeft as ||-

infixl 4 Reexport.apply as -<

infixl 4 Reexport.applyFirst as -|

infixl 4 Reexport.applySecond as |-

infixl 5 pair as <>

infixl 1 Reexport.bind as |=

infixr 1 Reexport.bindFlipped as =|

done :: forall f a. Reexport.Applicative f => a -> f a
done = pure

pair :: forall f a b. Reexport.Applicative f => f a -> f b -> f (a ** b)
pair x y = done (**) -< x -< y

skip :: forall f. Reexport.Applicative f => f Reexport.Unit
skip = done Reexport.unit

---- Newtypes ------------------------------------------------------------------
using :: forall f s b a t. Reexport.Newtype t a => Reexport.Newtype s b => Reexport.Functor f => (a -> t) -> (f s -> t) -> f b -> a
using _ f = Reexport.unwrap << f << Reexport.map Reexport.wrap
