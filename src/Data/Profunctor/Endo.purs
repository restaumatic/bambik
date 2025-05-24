module Data.Profunctor.Endo
  ( class Endo
  , pendo
  , bind
  , discard
  )
  where

import Data.Profunctor (class Profunctor)
import Data.Unit (Unit, unit)

class Profunctor p <= Endo p where
  pendo :: forall a. p a a -> p a a -> p a a
  -- such that `pendo p (pendo q r) == pendo (pendo p q) r`
  -- TODO: should we mention that:
  -- if Zero p then `pendo pzero p ~ p ~ pendo p pzero`

-- qualified do notation for `Endo` profunctors
bind ∷ ∀ k a. Endo k ⇒ k a a → (k a a → k a a) → k a a
bind a b = a `pendo` b a
discard ∷ ∀ k a. Endo k ⇒ k a a → (Unit → k a a ) → k a a
discard a b = a `pendo` b unit
