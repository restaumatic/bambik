-- | The coined **sum→product** strength `Retaining` — the `+ → ×` analogue of
-- | the ecosystem's `Data.Profunctor.Choice`, stated **positionally**
-- | (`Either`/`Tuple`) with no row in sight, which is why it lives here rather
-- | than in a row module. Its co-strength is `Data.Profunctor.Coretaining`,
-- | one module over exactly as `Cochoice` sits beside `Choice`. The row layer
-- | that builds on it is `Data.Profunctor.Row.VariantToRecord`; the optic it
-- | generates is `Data.Lens.Reel`.
-- |
-- | This module is a **complement of the ecosystem's own**, not bambik's: it
-- | claims a `Data.Profunctor.*` name because it belongs in that family
-- | beside `Strong`/`Choice`/`Costrong`/`Cochoice`, and it lives under the
-- | separate `extras/profunctor` source root to say so — nothing here
-- | mentions `PUI`, a row, or a carrier, so it could be lifted into
-- | `purescript-profunctor` unchanged.
module Data.Profunctor.Retaining
  ( class Retaining
  , retain
  )
  where

import Data.Either (Either)
import Data.Profunctor (class Profunctor)
import Data.Tuple (Tuple)

-- | The **unary** sum→product strength for this direction: a **Mealy /
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
-- | state — a productive, stateful stream. Its binary, two-profunctor form is
-- | the `VariantToRecord.variantToRecord` merge.
-- |
-- | There is deliberately **no `(->)` instance**: a stateless function has no
-- | `c` to place in the product on a fresh `Left a`, and no `b` on a `Right c`
-- | resume — the product output can't be filled without retaining state.
-- |
-- | This is the **bare strength** for the `+ → ×` direction (the analogue of
-- | `Strong`/`Choice`); the row combinator built on it is
-- | `VariantToRecord.subRetaining` — exactly as `RecordToRecord.subStrong` is
-- | built on `Strong`.
class Profunctor p <= Retaining p where
  retain :: forall a b c. p a b -> p (Either a c) (Tuple b c)
