-- Polymorphic invariant transformers - invariant optics 
module Data.Invariant.Optics
  ( constructorInvPrism
  , invAdapter
  , invAffineTraversal
  , invAffineTraversal'
  , invLens
  , invPrism
  , projection
  , propertyInvLens
  , propertyInvLens'
  , replace
  , zeroed
  )
  where

import Prelude hiding (zero)

import Data.Either (Either(..), either)
import Data.Function (on)
import Data.Invariant (class CartesianInvariant, class CoCartesianInvariant, class Invariant, invfirst, invleft, invmap, invright, invsecond)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Plus (class Plus, plus, zero)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy)

invAdapter :: forall i a s . Invariant i => (a -> s) -> (s -> a) -> i a -> i s
invAdapter f g = invmap f g

invLens :: forall i a s. CartesianInvariant i => (s -> a) -> (s -> a -> s) -> i a -> i s
invLens get set ia = invmap (\(Tuple a s) -> set s a) (\s -> Tuple (get s) s) (invfirst ia)

propertyInvLens
  :: forall i l r1 r a
   . CartesianInvariant i
  => IsSymbol l
  => Row.Cons l a r r1
  => Proxy l
  -> i a -> i (Record r1)
propertyInvLens l = invLens (\s -> get l s) (\s a -> (set l) a s)

propertyInvLens'
  :: forall i l r1 r a s
   . CartesianInvariant i
  => IsSymbol l
  => Row.Cons l a r r1
  => Newtype s (Record r1)
  => Proxy s
  -> Proxy l
  -> i a -> i s
propertyInvLens' _ l = invLens (\s -> get l (unwrap s)) (\s a -> wrap $ (set l) a (unwrap s))

invPrism :: forall i a s. CoCartesianInvariant i => (a -> s) -> (s -> Either a s) -> i a -> i s
invPrism review preview ia = invmap (\aors -> either review identity aors) preview (invleft ia)

constructorInvPrism :: forall i a s. CoCartesianInvariant i => (a -> s) -> (s -> Maybe a) -> i a -> i s
constructorInvPrism construct deconstruct ia = invmap (\(aors :: Either a s) -> either construct identity aors) (\s -> maybe (Right s) Left (deconstruct s)) (invleft ia)

invAffineTraversal
  :: forall s a i
   . CartesianInvariant i 
  => CoCartesianInvariant i 
  => (s -> a -> s)
  -> (s -> Either s a)
  -> i a -> i s
invAffineTraversal set pre = invAffineTraversal' (\s -> Tuple (set s) (pre s))

invAffineTraversal'
  :: forall s a i
   . CartesianInvariant i 
  => CoCartesianInvariant i 
  => (s -> Tuple (a -> s) (Either s a))
  -> i a -> i s
invAffineTraversal' to pab =
  invmap (\(Tuple b f) -> either identity b f) to (invsecond (invright pab))

projection :: forall i a s . CartesianInvariant i => (s -> a) -> i a -> i s
projection f = invLens f (\s _ -> s)


-- TODO: this is not a strict optic
replace :: forall i a . Invariant i => i a -> i a -> i a
replace = const

zeroed :: forall i a s . Invariant i => Plus i => i a -> i s
zeroed = const zero
