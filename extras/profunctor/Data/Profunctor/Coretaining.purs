-- | The **co-strength** of `Data.Profunctor.Retaining` — the `+ → ×` analogue
-- | of the ecosystem's `Data.Profunctor.Cochoice`, stated **positionally**
-- | (`Either`/`Tuple`) with no row in sight, and living beside its strength
-- | exactly as `Cochoice` lives beside `Choice`. The row form built on it is
-- | `Data.Profunctor.Row.VariantToRecord.unfolding`; the optic it generates is
-- | `Data.Lens.Coreel`.
-- |
-- | Like its strength, a **complement of the ecosystem's own** — hence the
-- | `Data.Profunctor.*` name and the separate `extras/profunctor` source
-- | root: nothing here mentions `PUI`, a row, or a carrier.
module Data.Profunctor.Coretaining
  ( class Coretaining
  , coretain
  )
  where

import Data.Either (Either)
import Data.Profunctor (class Profunctor)
import Data.Tuple (Tuple)

-- | The **co-strength** of `Retaining` — its retraction: where `retain`
-- | *adds* the resumable state channel `c`, `coretain` *ties* it. Every
-- | emission `Tuple b c` yields `b` and immediately re-enters the wrapped
-- | profunctor as a `Right c` resume — a **productive unfold**/generator:
-- | control loops back while output flows every step (the dual corner to
-- | `Coresolving`'s terminating fold in the trace quartet).
-- |
-- | Retraction law — in **seeded** form, because on gated carriers the raw
-- | composite `coretain (retain g)` is output-dead (`retain`'s gate waits on
-- | a resume only emissions can trigger):
-- | `coretain (seeded (Right c0) >>> retain g) ≈ g` — state must enter
-- | somewhere, and the seed is where; the row form `unfolding` takes it as
-- | its first argument. Tested in test/Main.purs.
-- |
-- | (No `(->)` instance: tying a knot takes state.)
class Profunctor p <= Coretaining p where
  coretain :: forall a b c. p (Either a c) (Tuple b c) -> p a b
