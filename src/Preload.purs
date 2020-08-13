module Preload
  ( module Reexport
  -- Booleans
  , Bool
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
  -- HashSets
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
  -- Foldables, Traversables
  , foldlInfix
  , (/:)
  , foldrInfix
  , (:\)
  , gather
  -- Functors, Applicatives, Alternatives, Monads
  , (<||)
  , (||>)
  , (-||)
  , (||-)
  , done
  , (-<)
  , (-|)
  , (|-)
  , none
  , pair
  , (<>)
  , (|=)
  , (=|)
  -- Newtypes
  , using
  ) where

---- Reexports
import Prelude hiding (mempty, pure, (<<<), (>>>), (<>), ($), (#), (<$>), (<#>), (<@>), (<$), ($>), (<*>), (<*), (*>)) as Reexport
import Control.Apply (applyFirst, applySecond) as Reexport
import Control.Bind (bind, bindFlipped, discard) as Reexport
import Data.Bifoldable as Reexport
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap) as Reexport
import Data.Either hiding (Either) as Reexport
import Data.Either.Nested (type (\/)) as Reexport
import Data.Enum (class Enum) as Reexport
import Data.Functor (mapFlipped, voidLeft, voidRight) as Reexport
import Data.Foldable hiding (foldM) as Reexport
import Data.FoldableWithIndex hiding (foldlDefault, foldrDefault, foldMapDefault) as Reexport
import Data.HashMap (HashMap) as Reexport
import Data.HashSet (HashSet) as Reexport
import Data.List (List(..)) as Reexport
import Data.Maybe as Reexport
import Data.Newtype (class Newtype, wrap, unwrap, ala, over, over2, under, under2) as Reexport
import Data.Traversable as Reexport
import Data.Tuple (curry, fst, lookup, snd, swap, uncurry) as Reexport
import Data.Tuple.Nested ((/\), type (/\)) as Reexport
-- import Data.Generic.Rep (class Generic) as Reexport
-- import Data.Generic.Rep.Show (genericShow) as Reexport
---- Rest
import Control.Semigroupoid (composeFlipped)
import Control.Applicative (pure)
import Data.Enum (enumFromTo)
import Data.Monoid (mempty)
import Data.Foldable (foldM)
import Data.Function (apply, applyFlipped)
import Data.Hashable (class Hashable)
import Data.HashSet (member) as HashSet
import Unsafe.Coerce (unsafeCoerce)
import Prim.TypeError (class Warn, Text)

---- Booleans ------------------------------------------------------------------
type Bool
  = Boolean

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

---- HashSets ------------------------------------------------------------------
infix 4 HashSet.member as =<

infix 4 notMember as /<

notMember :: forall a. Hashable a => a -> Reexport.HashSet a -> Bool
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

gather :: forall f m a b. Reexport.Foldable f => Reexport.Monad m => (a -> b -> m a) -> a -> f b -> m a
gather = foldM

---- Functors, Applicatives, Applicatives, Monads ------------------------------
infixl 4 Reexport.map as <||

infixl 1 Reexport.mapFlipped as ||>

infixl 4 Reexport.voidLeft as -||

infixl 4 Reexport.voidRight as ||-

infixl 4 Reexport.apply as -<

infixl 4 Reexport.applyFirst as -|

infixl 4 Reexport.applySecond as |-

infixl 5 pair as <>

infixl 1 Reexport.bind as |=

infixr 1 Reexport.bindFlipped as =|

done :: forall f a. Reexport.Applicative f => a -> f a
done = pure

pair :: forall f a b. Reexport.Applicative f => f a -> f b -> f (a Reexport./\ b)
pair x y = done Reexport.(/\) -< x -< y

none :: forall f. Reexport.Applicative f => f Reexport.Unit
none = done Reexport.unit

---- Newtypes ------------------------------------------------------------------
using :: forall f s b a t. Reexport.Newtype t a => Reexport.Newtype s b => Reexport.Functor f => (a -> t) -> (f s -> t) -> f b -> a
using _ f = Reexport.unwrap << f << Reexport.map Reexport.wrap
