-- | **Juxtaposition as carrier structure** (`Seeding` and `Looping`'s
-- | sibling): two components of the *same* type run over one channel
-- | pair — both are fed every input (broadcast), and either one's
-- | emission forwards unchanged (interleave, **last writer wins** under
-- | a synchronous loop). The ungated joint merge: no gate (nothing
-- | partial to await), no union (emissions arrive whole), no ownership
-- | (everyone speaks the whole value) — and therefore no row in sight,
-- | which is why it lives beside the carrier-structure classes rather
-- | than in `Data.Profunctor.Row.*`.
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
-- | **Why a class at the profunctor kind and not
-- | `Semigroup (PUI m a b)`**: the operation is associative and binary at
-- | fixed types, but a `Semigroup` instance at the *saturated* type is
-- | structure at the wrong kind twice over. PureScript has no quantified
-- | constraints, so `forall a b. Semigroup (p a b)` is unstatable — an
-- | instance on the application could never be *carrier structure* that a
-- | derived form abstracts over, where `Joining p` can appear in any
-- | signature beside `Looping p` and the merges. And the ecosystem's
-- | convention for function-like types is the **pointwise** semigroup
-- | (`Semigroup b => Semigroup (a -> b)` combines outputs), so claiming
-- | broadcast/interleave as *the* semigroup of the saturated type would
-- | give `<>` a different algebra on different carriers. The literature's
-- | name for this operation is `ArrowPlus`'s `<+>` (arrows' monoid,
-- | minus `arr`); the ecosystem carries no profunctor form of it, so the
-- | class is bambik's, with the pointer stated.
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
