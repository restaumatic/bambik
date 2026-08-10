-- | The coined **product→sum** strength `Resolving` and its co-strength
-- | `Coresolving` — the `× → +` analogue of the ecosystem's
-- | `Data.Profunctor.Strong`/`Data.Profunctor.Costrong`, and the reason this
-- | pair lives in its own module: like `Strong`/`Choice` they are stated
-- | **positionally** (`Tuple`/`Either`), with no row in sight. The row layer
-- | that builds on them is `Data.Profunctor.Row.RecordToVariant`; the optics
-- | they generate (`Shutter`/`Coshutter`) are in `Data.Profunctor.Optic`.
module Data.Profunctor.Resolving
  ( class Resolving
  , resolve
  , class Coresolving
  , coresolve
  )
  where

import Data.Either (Either)
import Data.Profunctor (class Profunctor)
import Data.Tuple (Tuple)

-- | The **unary** product→sum strength for this direction: a single **loop /
-- | iteration step**. `resolve` runs a transformer `p a b` on an input `a`
-- | alongside a carried state `c`, returning a `Step`:
-- |
-- | ```
-- | resolve :: p a b -> p (Tuple a c) (Either b c)
-- |                                      -- Left  b = Done b  (finish)
-- |                                      -- Right c = Loop c  (continue)
-- | ```
-- |
-- | State enters guaranteed (product input) and leaves optionally (a branch of
-- | the sum output), so the step may *halt*; closing the `c` channel gives `p`
-- | a terminating iteration (`tailRec`-style). It is the `identity`-pinned form
-- | of the positional product→sum base merge
-- | `p a b -> p c d -> p (Tuple a c) (Either b d)` (its second operand fixed
-- | to `identity`) — the product→sum analogue of how `RecordToRecord.subStrong` is the
-- | unary form of `recordToRecord`.
-- |
-- | With no out-of-band loop signal in the wire protocol (values are just
-- | values), the `PUI` instance derives the branch **from time**: every
-- | emission loops (`Right`) while the UI component is still moving, and the last
-- | emission resolves (`Left`) at quiescence — so
-- | `coresolve (resolve g) = debounced g ≅ g` up to time, once primed.
-- | (No `(->)` instance: a timeless carrier could only give the trivial
-- | always-`Done` step, which carries no iteration.)
-- |
-- | This is the **bare strength** for the `× → +` direction (the analogue of
-- | `Strong`/`Choice`); the row combinator built on it is `subResolving` below —
-- | exactly as `RecordToRecord.subStrong` is built on `Strong`.
class Profunctor p <= Resolving p where
  resolve :: forall a b c. p a b -> p (Tuple a c) (Either b c)

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
