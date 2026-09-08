-- | The **co-strength** of `Data.Profunctor.Resolving` — the `× → +` analogue
-- | of the ecosystem's `Data.Profunctor.Costrong`, stated **positionally**
-- | (`Tuple`/`Either`) with no row in sight, and living beside its strength
-- | exactly as `Costrong` lives beside `Strong`. The row form built on it is
-- | `Data.Profunctor.Row.RecordToVariant.folding`; the optic it generates is
-- | `Data.Lens.Coshutter`.
-- |
-- | Like its strength, a **complement of the ecosystem's own** — hence the
-- | `Data.Profunctor.*` name and the separate `extras/profunctor` source
-- | root: nothing here mentions `PUI`, a row, or a carrier.
module Data.Profunctor.Coresolving
  ( class Coresolving
  , coresolve
  )
  where

import Data.Either (Either)
import Data.Profunctor (class Profunctor)
import Data.Tuple (Tuple)

-- | The **co-strength** of `Resolving` — its retraction: where `resolve`
-- | *adds* the loop channel `c`, `coresolve` *ties* it. A `Right c` emission
-- | is retained as the state paired with subsequent inputs; a `Left b` exits.
-- | Semantically a **terminating fold**: inputs accumulate through `c` until
-- | the wrapped profunctor decides `b` — the fourth loop flavor in the trace
-- | quartet (`Costrong` = state that emits each step, `Cochoice` = control
-- | that emits at exit, `Coresolving` = state that emits at exit,
-- | `Coretaining` = control that emits each step).
-- |
-- | Retraction law — in **seeded** form, because on gated carriers the raw
-- | composite `coresolve (resolve g)` is input-dead (each gate waits on the
-- | other; of the four traces only `Cochoice`'s `unleft (left g) = g` holds
-- | raw):
-- |
-- | ```
-- | coresolve (resolve g >>> seeded (Right c0)) ≈ debounced g
-- | ```
-- |
-- | — state must enter somewhere, and the seed is where; the row form
-- | `folding` takes it as its first argument. Tested in test/Main.purs.
-- |
-- | (No `(->)` instance: tying a knot takes state.)
class Profunctor p <= Coresolving p where
  coresolve :: forall a b c. p (Tuple a c) (Either b c) -> p a b
