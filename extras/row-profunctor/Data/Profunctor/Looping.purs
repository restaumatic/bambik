-- | **Self-reference as carrier structure** — `Seeding`'s sibling: `Seeding`
-- | says *a beginning exists* (one registration moment), `Looping` says
-- | *feedback exists* (an emission can re-enter its own input). The two
-- | things a timeless carrier lacks — hence, like `Seeding`, deliberately
-- | **no `(->)` instance**: feeding a function its own output is `fix`, a
-- | fixpoint computation, not a wire.
-- |
-- | The method is the `×`-diagonal **self-trace** at record rows: feed a
-- | UI component its own emissions. A *class* because no ecosystem class
-- | reaches it: on knowledge-gated carriers `Costrong`'s `unfirst` cannot
-- | self-feed (no `c` before the first emission, no emission before the
-- | first input — the gate deadlocks), so the self-feeding special case is
-- | carrier structure, not a derivation. Row-shaped in the method itself:
-- | the looped value is an entity (a model row) — self-feeding an event
-- | diagonal would replay one-shot events; the lawful `+`-loop is
-- | `Data.Profunctor.Row.VariantToVariant.iterate`.
-- |
-- | Laws — the trace axioms restricted to the diagonal (`identity` on
-- | `Category` carriers):
-- |
-- | ```
-- | looped identity        = identity                       (yanking)
-- | looped (dimap f f g)   = dimap f f (looped g)           (f an iso — dinaturality)
-- | looped (looped g)      = looped g                       (idempotence: the guard)
-- | ```
-- |
-- | What the carrier-agnostic layer builds on it: `mvu` (the app shape, in
-- | `Data.Profunctor.Row.RecordToRecord`) and `bracketed` (the
-- | variant-editor bracket, in `Data.Profunctor.Row.VariantToVariant`).
module Data.Profunctor.Looping
  ( class Looping
  , looped
  )
  where

import Data.Profunctor (class Profunctor)

class Profunctor p <= Looping p where
  looped :: forall r. p { | r } { | r } -> p { | r } { | r }
