module Data.Invariant.Transformers where

import Prelude hiding (zero)

import Data.Either (Either(..))
import Data.Function (on)
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, invfirst, invleft, invmap, invright, invsecond)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Plus (class Plus, plus, zero)
import Data.Tuple (Tuple(..))

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

instance Plus i => Plus (IOContext c i) where
  zero = wrap zero
  plus foo1 foo2 = wrap $ (plus `on` unwrap) foo1 foo2

-- Cayley
-- For arbitrary functor f, f (i _)  preserves invariance, cartesian invariance, co-cartesian invariance of i.
-- For arbitrary applicative functor f, f (i _) preserves plus instance of i.
newtype Cayley :: forall k1 k2. (k1 -> Type) -> (k2 -> k1) -> k2 -> Type
newtype Cayley f i a = Cayley (f (i a))

derive instance Newtype (Cayley f i a) _

instance (Functor f, Invariant p) => Invariant (Cayley f p) where
  invmap f g = wrap <<< map (invmap f g) <<< unwrap

instance (Functor f, Cartesian i) => Cartesian (Cayley f i) where
  invfirst  = wrap <<< map invfirst <<< unwrap
  invsecond = wrap <<< map invsecond <<< unwrap

instance (Functor f, CoCartesian i) => CoCartesian (Cayley f i) where
  invleft   = wrap <<< map invleft <<< unwrap
  invright  = wrap <<< map invright <<< unwrap

instance (Applicative f, Plus i) => Plus (Cayley f i) where
  plus c1 c2 = wrap $ plus <$> unwrap c1 <*> unwrap c2
  zero = wrap $ pure zero
