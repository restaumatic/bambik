-- | Compose `do` block entries in a `Category` — `QualifiedDo.Semigroupoid`
-- | at `Category`, so a block states the structure it composes in: the
-- | entries chain with `>>>`, and the block's unit is `identity`
-- | (`Category.do { identity; a }` ≡ `a` ≡ `Category.do { a; identity }`).
-- | `bind`/`discard` are the ecosystem's verbatim; the stronger constraint
-- | is the point. Bambik's pipelines import it `as Category`:
-- |
-- | ```purescript
-- | import QualifiedDo.Category as Category
-- |
-- | -- Equivalent to: form >>> summary >>> actions
-- | Category.do
-- |   form
-- |   summary
-- |   actions
-- | ```
-- |
-- | A complement of `purescript-qualified-do`, which stops at
-- | `Semigroupoid`; liftable unchanged.
module QualifiedDo.Category where

import Prelude

bind ∷ ∀ k a b c. Category k ⇒ k a b → (k a b → k b c) → k a c
bind a b = a >>> b a

discard ∷ ∀ k a b c. Category k ⇒ k a b → (Unit → k b c) → k a c
discard a b = a >>> b unit
