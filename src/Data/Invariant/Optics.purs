-- Polymorphic invariant transformers - invariant optics
--
-- This module included functions that turn basic encoding of adapters, lenses, prisms, affineTraversals etc. into invariant encoding
-- i.e. polymorphic invariant transformers.
module Data.Invariant.Optics
  ( invAdapter
  , invAffineTraversal
  , invConst
  , invGrate
  , invLens
  , invPrism
  , invProjection
  , invZero
  )
  where

import Prelude hiding (zero)

import Data.Either (Either, either)
import Data.Invariant (class Cartesian, class Closed, class CoCartesian, class Invariant, closed, invfirst, invleft, invmap, invright, invsecond)
import Data.Plus (class Plus, pzero)
import Data.Tuple (Tuple(..))

invAdapter :: forall f a b. Invariant f => (a -> b) -> (b -> a) -> f a -> f b
invAdapter = invmap

invLens :: forall i a s. Invariant i => Cartesian i => (s -> a) -> (s -> a -> s) -> i a -> i s
invLens get set ia = invmap (\(Tuple a s) -> set s a) (\s -> Tuple (get s) s) (invfirst ia)

invPrism :: forall i a s. Invariant i => CoCartesian i => (a -> s) -> (s -> Either a s) -> i a -> i s
invPrism review preview ia = invmap (\aors -> either review identity aors) preview (invleft ia)

invProjection :: forall i a s . Invariant i => Cartesian i => (s -> a) -> i a -> i s
invProjection f = invLens f (\s _ -> s)

invAffineTraversal
  :: forall s a i
   . Invariant i
  => CoCartesian i
  => Cartesian i
  => (s -> a -> s)
  -> (s -> Either s a)
  -> i a -> i s
invAffineTraversal set pre = invAffineTraversal' (\s -> Tuple (set s) (pre s))

invAffineTraversal'
  :: forall s a i
   . Invariant i
  => Cartesian i
  => CoCartesian i
  => (s -> Tuple (a -> s) (Either s a))
  -> i a -> i s
invAffineTraversal' to pab =
  invmap (\(Tuple b f) -> either identity b f) to (invsecond (invright pab))

invZero :: forall i a s . Invariant i => Plus i => i a -> i s
invZero = const pzero

invGrate :: forall i a s. Invariant i => Closed i => (((s -> a) -> a) -> s) -> i a -> i s
invGrate f pab = invmap f (#) (closed pab)
-- example:
-- data Pair a = Pair a a
-- fstOfPair (Pair a _) = a
-- sndOfPair (Pair _ a) = a
-- f :: forall a. ((Pair a -> a) -> a) -> Pair a
-- f foo = Pair (foo fstOfPair) (foo sndOfPair)
-- pair :: forall i a. Invariant i => Closed i => i a -> i (Pair a)
-- pair = invGrate f

invConst :: forall i a s. Invariant i => Cartesian i => a -> i a -> i s
invConst a = invLens (const a) (\s _ -> s)
