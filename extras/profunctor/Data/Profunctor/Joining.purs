-- | **Juxtaposition**: two components of the *same* type run over one
-- | channel pair — both are fed every input (broadcast), and either
-- | one's emission forwards unchanged (interleave, **last writer wins**
-- | under a synchronous loop). The ungated joint merge: no gate (nothing
-- | partial to await), no union (emissions arrive whole), no ownership
-- | (everyone speaks the whole value).
-- |
-- | This module is a **complement of the ecosystem's own**, not
-- | bambik's: it is `ArrowPlus`'s `<+>` — the arrows' monoid, minus
-- | `arr` — at the profunctor kind, a class the ecosystem names but
-- | never built at this kind. It claims a `Data.Profunctor.*` name
-- | because it belongs in that family beside `Strong`/`Choice`, and it
-- | lives under the separate `extras/profunctor` source root to say so —
-- | nothing here mentions `PUI`, a row, or a carrier, so it could be
-- | lifted into `purescript-profunctor` unchanged.
-- |
-- | Laws:
-- |
-- |   * associativity: `joint (joint p q) r ≅ joint p (joint q r)`
-- |     (both channels sequence left-to-right, so re-association changes
-- |     nothing observable; on a DOM carrier, registration order = code
-- |     order = DOM order);
-- |   * dinaturality: `dimap f g (joint p q) = joint (dimap f g p) (dimap f g q)`
-- |     (broadcast and interleave are label- and value-blind).
-- |
-- | **Why a class at the profunctor kind and not a `Semigroup` instance
-- | at the saturated type `p a b`**: the operation is associative and
-- | binary at
-- | fixed types, but a `Semigroup` instance at the *saturated* type is
-- | structure at the wrong kind twice over. PureScript has no quantified
-- | constraints, so `forall a b. Semigroup (p a b)` is unstatable — an
-- | instance on the application could never be *carrier structure* that a
-- | derived form abstracts over, where `Joining p` can appear in any
-- | signature beside `Strong p` and its kin. And the ecosystem's
-- | convention for function-like types is the **pointwise** semigroup
-- | (`Semigroup b => Semigroup (a -> b)` combines outputs), so claiming
-- | broadcast/interleave as *the* semigroup of the saturated type would
-- | give `<>` a different algebra on different carriers.
-- |
-- | **No unit member, deliberately**: the lawful unit differs by output
-- | shape — a record output must *announce* its `{}`, a variant output
-- | must be silent — and a shape-blind class cannot have both, so the
-- | units stay per-direction (`RecordToRecord.pempty`,
-- | `RecordToVariant.silence`). **No `(->)` instance, deliberately**: a
-- | function returns exactly one output, so it cannot interleave two
-- | emission streams — the pointwise combination exists but requires
-- | `Semigroup b` and is a different operation. Interleaving needs a
-- | duplex temporal carrier; CPS is enough (`Semigroup r => Joining (Cont r)`
-- | runs both continuations and combines the answers).
module Data.Profunctor.Joining
  ( class Joining
  , joint
  )
  where

import Data.Profunctor (class Profunctor)

class Profunctor p <= Joining p where
  joint :: forall a b. p a b -> p a b -> p a b
