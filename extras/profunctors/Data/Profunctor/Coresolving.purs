-- | The **co-strength** of `Data.Profunctor.Resolving` — the `× → +` analogue
-- | of the ecosystem's `Data.Profunctor.Costrong`, stated **positionally**
-- | (`Tuple`/`Either`) with no row in sight, and living beside its strength
-- | exactly as `Costrong` lives beside `Strong`. The row form built on it is
-- | `Data.Profunctor.Row.RecordToVariant.folding`; the optic it generates is
-- | `Data.Lens.Coshutter`.
-- |
-- | Like its strength, a **complement of the ecosystem's own** — hence the
-- | `Data.Profunctor.*` name and the separate `extras/profunctors` source
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
-- | Retraction law, shared by all four traces: `coresolve (resolve g) ≅ g` —
-- | once the state channel is primed (state must enter somewhere; the `PUI`
-- | instance is knowledge-gated like `Costrong`, withholding inputs until a
-- | first `c` exists).
-- |
-- | (No `(->)` instance: tying a knot takes state.)
class Profunctor p <= Coresolving p where
  coresolve :: forall a b c. p (Tuple a c) (Either b c) -> p a b
