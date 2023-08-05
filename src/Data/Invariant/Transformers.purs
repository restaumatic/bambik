module Data.Invariant.Transformers
  ( class InvTrans
  , invlift
  )
  where

import Data.Invariant (class Invariant)

class InvTrans t where
  invlift :: forall i a. Invariant i => i a -> t i a
