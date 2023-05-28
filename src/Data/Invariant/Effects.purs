-- Invariant polymorphic transformers - invariant effects
module Data.Invariant.Effects
  ( InvTransformer
  )
  where

import Data.Invariant (class Invariant)

type InvTransformer i = forall a . Invariant i => i a -> i a