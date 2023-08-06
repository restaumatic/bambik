module Data.Invariant.Transformers
  ( class InvTrans
  , invlift
  , invliftmap
  )
  where

import Data.Invariant (class Invariant)

class InvTrans t where
  invlift :: forall i a. Invariant i => i a -> t i a
  invliftmap :: forall i b. Invariant i => (forall a. i a -> i a) -> t i b -> t i b
