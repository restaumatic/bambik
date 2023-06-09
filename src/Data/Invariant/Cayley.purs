module Data.Invariant.Cayley where

import Prelude hiding (zero)

import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, invfirst, invleft, invmap, invright, invsecond)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Plus (class Plus, plus, zero)

newtype CayleyInvariant :: forall k1 k2. (k1 -> Type) -> (k2 -> k1) -> k2 -> Type
newtype CayleyInvariant f i a = CayleyInvariant (f (i a))

derive instance Newtype (CayleyInvariant f i a) _

instance (Functor f, Invariant p) => Invariant (CayleyInvariant f p) where
  invmap f g = wrap <<< map (invmap f g) <<< unwrap

instance (Functor f, Cartesian i) => Cartesian (CayleyInvariant f i) where
  invfirst  = wrap <<< map invfirst <<< unwrap
  invsecond = wrap <<< map invsecond <<< unwrap

instance (Functor f, CoCartesian i) => CoCartesian (CayleyInvariant f i) where
  invleft   = wrap <<< map invleft <<< unwrap
  invright  = wrap <<< map invright <<< unwrap

instance (Applicative f, Invariant i, Plus i) => Plus (CayleyInvariant f i) where
  plus c1 c2 = wrap $ plus <$> unwrap c1 <*> unwrap c2
  zero = wrap $ pure zero
