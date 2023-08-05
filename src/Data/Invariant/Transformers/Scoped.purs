module Data.Invariant.Transformers.Scoped
  ( Scope
  , Scoped(..)
  , invConstructor
  , invField
  , invField'
  )
  where

import Prelude

import Debug (spy)
import Data.Array (cons, uncons)
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Invariant (class Cartesian, class CoCartesian, class Filtered, invfright, invmap)
import Data.Invariant.Optics (invLens, invPrism)
import Data.Invariant.Transformers.Tunneling (Tunneling)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Zero (class Zero)
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy)

-- Scoped: carrying additional info about the scope of a change
-- `Scope` is a Monoid
--   => `Tuple Scope` is a Functor, Apply, CoApply
--     => `Scoped i` preserves Invariant, Cartesian, CoCartesian, Plusoid and Plus instances of of `i`.
--   => `Tuple Scope` is CoApplicative and Applicative so
--     => `Scoped i` is InvTransformer.
type Scoped i a = Tunneling (Tuple Scope) i a

data Scope = Scope (Array Hop) | AnyScope -- this is actually a lattice (bottom, singleton vales, top), TODO: find already existing data type for it

derive instance Eq Scope

type Hop = String

instance Show Scope where
  show AnyScope = "*"
  show (Scope hops) = "'" <> intercalate "/" hops <> "'"

-- finds least common scope
instance Semigroup Scope where
  append AnyScope s = s
  append s AnyScope = s
  append s1 s2 = s1 -- TODO this is hack!

instance Monoid Scope where
  mempty = AnyScope

instance Zero Scope where
  zero = Scope []

scopedOut :: forall i a. Filtered i => Hop -> Scoped i a -> Scoped i a
scopedOut hop = wrap <<< invmap (\e -> let (Tuple subScope a) = either identity identity e in Tuple (spy "zoomed out" (zoomOut (spy "to zoom out" subScope))) a) (\(Tuple scope a) -> maybe (Left (Tuple scope a)) (\subScope -> Right (Tuple subScope a)) (spy "zoomed in" (zoomIn (spy "to zoom in" scope)))) <<< invfright <<< unwrap
  where
  zoomOut :: Scope -> Scope
  zoomOut AnyScope = Scope [hop]
  zoomOut (Scope hops) = Scope (hop `cons` hops)  -- zooming out zero is not zero
  zoomIn :: Scope -> Maybe Scope
  zoomIn AnyScope = Just AnyScope
  zoomIn s@(Scope hops) = case uncons hops of
    Nothing -> Just s -- zooming in zero is zero
    Just { head, tail }
      | head == hop -> Just $ Scope tail
      | otherwise -> Nothing -- cannot zoom in

-- then we can come up with an optic:

invField :: forall i a s. Filtered i => Cartesian i => String -> (s -> a -> s) -> (s -> a) -> Scoped i a -> Scoped i s
invField fieldName setter getter =  invLens getter setter >>> scopedOut fieldName

invConstructor :: forall i a s. Filtered i => CoCartesian i => String -> (a -> s) -> (s -> Maybe a) -> Scoped i a -> Scoped i s
invConstructor constructorName construct deconstruct = invPrism construct (\s -> maybe (Right s) Left (deconstruct s)) >>> scopedOut constructorName

--

invField'
  :: forall i l r1 r a
  . Filtered i
  => Cartesian i
  => IsSymbol l
  => Row.Cons l a r r1
  => Proxy l
  -> Scoped i a -> Scoped i (Record r1)
invField' l = invField (reflectSymbol l) (flip (set l)) (get l)
