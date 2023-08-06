module Data.Invariant.Transformers
  ( InvIdentityTrans(..)
  , class InvTrans
  , invlift
  , invliftmap
  )
  where

import Data.Invariant (class Invariant)
import Data.Newtype (class Newtype, modify, wrap)

class InvTrans t where
  invlift :: forall i a. Invariant i => i a -> t i a
  invliftmap :: forall i b. Invariant i => (forall a. i a -> i a) -> t i b -> t i b

newtype InvIdentityTrans :: forall k. (k -> Type) -> k -> Type
newtype InvIdentityTrans i a = InvIdentityTrans (i a)

derive instance Newtype (InvIdentityTrans i a) _

instance InvTrans InvIdentityTrans where
  invlift = wrap
  invliftmap = modify
