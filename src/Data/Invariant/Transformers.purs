module Data.Invariant.Transformers where

import Prelude hiding (zero)

import Data.Array (mapMaybe)
import Data.CoApplicative (class CoApply)
import Data.Either (Either(..), either)
import Data.Function (on)
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity(..))
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, invfirst, invleft, invmap, invright, invsecond)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (NonEmpty, (:|), head, tail)
import Data.Plus (class Plus, class Plusoid, plus, zero)
import Data.Tuple (Tuple(..), fst, snd)

-- IOContext
-- For arbitrary monoid c, i (Tuple c _) preserves invariance, cartesian invariance, co-cartesian invariance of i as well as plus instance of i.
newtype IOContext c i a = IOContext (i (Tuple c a))

derive instance Newtype (IOContext c i a) _

instance Invariant i => Invariant (IOContext f i) where
  invmap pre post foo = wrap $ invmap (map pre) (map post) $ unwrap foo

instance (Invariant i, Cartesian i) => Cartesian (IOContext c i) where
  invfirst foo = wrap $ let icab = invfirst $ unwrap foo in invmap (\(Tuple (Tuple c a) b) -> Tuple c (Tuple a b)) (\(Tuple c (Tuple a b)) -> Tuple (Tuple c a) b) icab
  invsecond foo = wrap $ let ibca = invsecond $ unwrap foo in invmap (\(Tuple b (Tuple c a)) -> Tuple c (Tuple b a)) (\(Tuple c (Tuple b a)) -> Tuple b (Tuple c a)) ibca

instance (Invariant i, CoCartesian i, Monoid c) => CoCartesian (IOContext c i) where
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

instance Plusoid i => Plusoid (IOContext c i) where
  plus foo1 foo2 = wrap $ (plus `on` unwrap) foo1 foo2

instance Plus i => Plus (IOContext c i) where
  zero = wrap zero


--

liftAdapter :: forall i f a b. Invariant i => Functor f => (forall j. Invariant j => j a -> j b) -> i (f a) -> i (f b)
liftAdapter adapter ifa = unwrap (Compose ifa # adapter)

liftLens :: forall i f a b. Cartesian i => Apply f => (forall j. Cartesian j => j a -> j b) -> i (f a) -> i (f b)
liftLens lens ifa = unwrap (Compose ifa # lens)

liftPrism :: forall i f a b. CoCartesian i => CoApply f => (forall j. CoCartesian j => j a -> j b) -> i (f a) -> i (f b)
liftPrism prism ifa = unwrap (Compose ifa # prism)

-- Compose
-- For arbitrary functor f, f (i _)  preserves invariance, cartesian invariance, co-cartesian invariance of i.
-- For arbitrary applicative functor f, f (i _) preserves plus instance of i.
-- newtype Compose :: forall k1 k2. (k1 -> Type) -> (k2 -> k1) -> k2 -> Type
-- newtype Compose f i a = Compose (f (i a))
-- derive instance Newtype (Compose f i a) _

