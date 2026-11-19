-- | The coined **sum→product** strength `Retaining` — the `+ → ×` analogue of
-- | the ecosystem's `Data.Profunctor.Choice`, stated **positionally**
-- | (`Either`/`Tuple`) with no row in sight. Its co-strength is
-- | `Data.Profunctor.Coretaining`; the row layer on it is
-- | `Data.Profunctor.Row.VariantToRecord`; the optic it generates is
-- | `Data.Lens.Reel`. A complement of the ecosystem's `Data.Profunctor`
-- | family, mentioning no `PUI`, row or carrier, liftable into
-- | `purescript-profunctor` unchanged — hence the `extras/profunctor` root.
module Data.Profunctor.Retaining
  ( class Retaining
  , retain
  )
  where

import Data.Either (Either)
import Data.Profunctor (class Profunctor)
import Data.Tuple (Tuple)

-- | The **unary** sum→product strength for this shape: a **Mealy /
-- | coroutine step**, the dual of `RecordToVariant`'s `Resolving`. `retain`
-- | turns a transformer `p a b` into a step that consumes either a fresh input
-- | `a` or a resumed state `c`, emitting an output `b` together with the next
-- | state `c`:
-- |
-- | ```
-- | retain :: p a b -> p (Either a c) (Tuple b c)
-- |                        -- Left  a = fresh input
-- |                        -- Right c = resume from state
-- | ```
-- |
-- | State enters optionally (a branch of the sum input) and leaves guaranteed
-- | (product output), so the step *always* produces an output and the next
-- | state — a productive, stateful stream. Its binary form is the
-- | `VariantToRecord.variantToRecord` merge. No `(->)` instance: a
-- | stateless function has no `c` to place in the product on a fresh `Left
-- | a`, and no `b` on a `Right c` resume.
-- |
-- | **Laws.** Naturality in `a`, `b` and dinaturality in `c` come free
-- | (parametricity); the Tambara coherences are absent for the same reason
-- | as `Resolving`'s — the mixed channels do not compose into one action —
-- | so the class alone is property-light: its equational content is the
-- | seeded retraction with `Coretaining`, `coretain (seeded (Right c0) >>>
-- | retain g) ≈ g` (test/Main.purs); the consequence for the optic is
-- | `Data.Lens.Reel`'s: existential constructors are sound, completeness
-- | is not claimed. It has no row form: a stage is typed at one row
-- | (`Data.Profunctor.Row`, *What is deliberately not here*).
class Profunctor p <= Retaining p where
  retain :: forall a b c. p a b -> p (Either a c) (Tuple b c)
