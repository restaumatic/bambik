-- | The coined **sum→product** strength `Retaining` and its co-strength
-- | `Coretaining` — the `+ → ×` analogue of the ecosystem's
-- | `Data.Profunctor.Choice`/`Data.Profunctor.Cochoice`, stated
-- | **positionally** (`Either`/`Tuple`) with no row in sight, which is why
-- | the pair lives here rather than in a row module. The row layer that
-- | builds on them is `Data.Profunctor.Row.VariantToRecord`; the optics they
-- | generate (`Reel`/`Coreel`) are in `Data.Profunctor.Optic`.
module Data.Profunctor.Retaining
  ( class Retaining
  , retain
  , class Coretaining
  , coretain
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
-- | the `variantToRecord` merge below.
-- |
-- | There is deliberately **no `(->)` instance**: a stateless function has no
-- | `c` to place in the product on a fresh `Left a`, and no `b` on a `Right c`
-- | resume — the product output can't be filled without retaining state.
-- |
-- | This is the **bare strength** for the `+ → ×` direction (the analogue of
-- | `Strong`/`Choice`); the row combinator built on it is `subRetaining` below —
-- | exactly as `RecordToRecord.subStrong` is built on `Strong`.
class Profunctor p <= Retaining p where
  retain :: forall a b c. p a b -> p (Either a c) (Tuple b c)

-- | The **co-strength** of `Retaining` — its retraction: where `retain`
-- | *adds* the resumable state channel `c`, `coretain` *ties* it. Every
-- | emission `Tuple b c` yields `b` and immediately re-enters the wrapped
-- | profunctor as a `Right c` resume — a **productive unfold**/generator:
-- | control loops back while output flows every step (the dual corner to
-- | `Coresolving`'s terminating fold in the trace quartet).
-- |
-- | Retraction law: `coretain (retain g) ≅ g` — once the state channel is
-- | primed (state must enter somewhere).
-- |
-- | (No `(->)` instance: tying a knot takes state.)
class Profunctor p <= Coretaining p where
  coretain :: forall a b c. p (Either a c) (Tuple b c) -> p a b
