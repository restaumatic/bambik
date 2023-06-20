-- Polymorphic invariant transformers - invariant optics 
module Data.Invariant.Optics
  ( invAffineTraversal
  , invAffineTraversal'
  , invLens
  , invPrism
  , projection
  , replace
  , zeroed
  )
  where

import Prelude hiding (zero)

import Data.Either (Either, either)
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, invfirst, invleft, invmap, invright, invsecond)
import Data.Plus (class Plus, zero)
import Data.Tuple (Tuple(..))



invLens :: forall i a s. Invariant i => Cartesian i => (s -> a) -> (s -> a -> s) -> i a -> i s
invLens get set ia = invmap (\(Tuple a s) -> set s a) (\s -> Tuple (get s) s) (invfirst ia)

invPrism :: forall i a s. Invariant i => CoCartesian i => (a -> s) -> (s -> Either a s) -> i a -> i s
invPrism review preview ia = invmap (\aors -> either review identity aors) preview (invleft ia)

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

projection :: forall i a s . Invariant i => Cartesian i => (s -> a) -> i a -> i s
projection f = invLens f (\s _ -> s)

zeroed :: forall i a s . Invariant i => Plus i => i a -> i s
zeroed = const zero

-- TODO: these are not a strict optic
replace :: forall i a . Invariant i => i a -> i a -> i a
replace = const
