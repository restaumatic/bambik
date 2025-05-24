module Data.Profunctor.Sum
  ( bind
  , psum
  , class Sum
  , discard
  )
  where

import Data.Profunctor (class Profunctor)
import Data.Unit (Unit, unit)

-- generalization of `Control.Plus.Alt`
class Profunctor p <= Sum p where
  psum :: forall a b . p a b -> p a b -> p a b -- such that `psum a (psum b c) == psum (psum a b) c

-- qualified do notation for `Sum` profunctors
bind ∷ ∀ f a b. Sum f ⇒ f a b → (f a b → f a b) → f a b
bind a b = a `psum` b a
discard ∷ ∀ f a b. Sum f ⇒ f a b → (Unit → f a b) → f a b
discard a b = a `psum` b unit