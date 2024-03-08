-- | Compose `do` block entries in a `Data.Profunctor.Sum`. Example:
-- |
-- | ```purescript
-- | import QualifiedDo.SumProfunctor as Compose
-- |
-- | -- Equivalent to: a `psum` b `psum` c
-- | Compose.do
-- |   a
-- |   b
-- |   c
-- | ```
module QualifiedDo.SumProfunctor where

import Prelude

import Data.Profunctor.Sum (class Sum, psum)

bind ∷ ∀ k a b. Sum k ⇒ k a b → (k a b → k a b) → k a b
bind a b = a `psum` b a

discard ∷ ∀ k a b. Sum k ⇒ k a b → (Unit → k a b) → k a b
discard a b = a `psum` b unit
