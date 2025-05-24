module Data.Profunctor.Endo
  ( class Endo
  , endoId
  , endoCompose
  , bind
  , discard
  )
  where

import Data.Profunctor (class Profunctor)
import Data.Unit (Unit, unit)

class Profunctor p <= Endo p where
  endoCompose :: forall a. p a a -> p a a -> p a a
  endoId :: forall a. p a a -- such that `endoCompose endoId p ~ p ~ endoCompose p endoId`

bind ∷ ∀ k a. Endo k ⇒ k a a → (k a a → k a a) → k a a
bind a b = a `endoCompose` b a

discard ∷ ∀ k a. Endo k ⇒ k a a → (Unit → k a a ) → k a a
discard a b = a `endoCompose` b unit
