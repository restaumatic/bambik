module Data.Invariant.Transformers.Tunneled
  ( Tunneled(..)
  )
  where

import Prelude

import Data.Invariant (class InvCartesian, class InvCocartesian, class Invariant, invfirst, invleft, invmap, invright, invsecond)
import Data.Invariant.Plus (class InvPlus, class InvPlusoid, invplus, invzero)
import Data.Invariant.Transformers (class InvTrans)
import Data.Newtype (class Newtype, modify, unwrap, wrap)

newtype Tunneled :: forall k1 k2. (k1 -> Type) -> (k2 -> k1) -> k2 -> Type
newtype Tunneled f i a = Tunneled (f (i a))
derive instance Newtype (Tunneled f i a) _

instance Applicative f => InvTrans (Tunneled f) where
  invlift = wrap <<< pure
  invliftmap = modify <<< map

-- Tunneled as an invariant using underlying invariant `i a` to tunnel `f (i a)`, where i is transparent to `f`.
-- Explanation inspired by a nice definition of tunneling from polish wikipedia (translated to english):
--   tunneling – setting up a connection between remote hosts through a network that does not know the protocol that these hosts communicate with
instance (Functor f, Invariant i) => Invariant (Tunneled f i) where
  invmap f g = wrap <<< map (invmap f g) <<< unwrap

instance (Functor f, InvCartesian i) => InvCartesian (Tunneled f i) where
  invfirst  = wrap <<< map invfirst <<< unwrap
  invsecond = wrap <<< map invsecond <<< unwrap

instance (Functor f, InvCocartesian i) => InvCocartesian (Tunneled f i) where
  invleft   = wrap <<< map invleft <<< unwrap
  invright  = wrap <<< map invright <<< unwrap

instance (Apply f, InvPlus i) => InvPlusoid (Tunneled f i) where
  invplus c1 c2 = wrap $ invplus <$> unwrap c1 <*> unwrap c2

instance (Applicative f, InvPlus i) => InvPlus (Tunneled f i) where
  invzero = wrap $ pure invzero
