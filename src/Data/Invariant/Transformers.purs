module Data.Invariant.Transformers where

import Prelude hiding (zero)

import Data.Invariant (class CartesianInvariant, class CoCartesianInvariant, class Invariant, invfirst, invleft, invmap, invright, invsecond)

import Data.Either (Either(..))
import Data.Function (on)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Plus (class Plus, plus, zero)
import Data.Tuple (Tuple(..))

-- Context invariant transformer:
-- for arbitrary monoid c, i (Tuple c _) preserves invariance, cartesian invariance, co-cartesian invariance of i as well as plus instance
-- TODO think of a better name for being plus instance...
newtype ContextInvT c i a = ContextInvT (i (Tuple c a))

derive instance Newtype (ContextInvT c i a) _

instance Invariant i => Invariant (ContextInvT f i) where
  invmap pre post foo = wrap $ invmap (map pre) (map post) $ unwrap foo

instance CartesianInvariant i => CartesianInvariant (ContextInvT c i) where
  invfirst foo = wrap $ let icab = invfirst $ unwrap foo in invmap (\(Tuple (Tuple c a) b) -> Tuple c (Tuple a b)) (\(Tuple c (Tuple a b)) -> Tuple (Tuple c a) b) icab
  invsecond foo = wrap $ let ibca = invsecond $ unwrap foo in invmap (\(Tuple b (Tuple c a)) -> Tuple c (Tuple b a)) (\(Tuple c (Tuple b a)) -> Tuple b (Tuple c a)) ibca

instance (CoCartesianInvariant i, Monoid c) => CoCartesianInvariant (ContextInvT c i) where
  invleft foo = wrap $ let icab = invleft $ unwrap foo in invmap (\caorb -> case caorb of
    Left (Tuple c a) -> Tuple c (Left a)
    Right b -> Tuple mempty (Right b)) (\(Tuple c aorb) -> case aorb of
    Left a -> Left (Tuple c a)
    Right b -> Right b) icab
  invright foo = wrap $ let ibca = invright $ unwrap foo in invmap (\borca -> case borca of
    Right (Tuple c a) -> Tuple c (Right a)
    Left b -> Tuple mempty (Left b)) (\(Tuple c aorb) -> case aorb of
    Right a -> Right (Tuple c a)
    Left b -> Left b) ibca

instance Plus i => Plus (ContextInvT c i) where
  zero = wrap zero
  plus foo1 foo2 = wrap $ (plus `on` unwrap) foo1 foo2
