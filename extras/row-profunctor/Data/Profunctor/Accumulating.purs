-- | The **accumulating collection**: the `+→×` member of the keyed square,
-- | `Retaining`'s analogue over a runtime key — entities fed one at a
-- | time, the array of every key's latest row released whole after each.
-- | A keyed Mealy machine: an unknown key appends a slot in
-- | first-appearance order, a known key updates its slot, an element's own
-- | emission writes its slot the same way, and the whole array leaves at
-- | once — **input-primed**, since the fed row is its slot's first value
-- | (which is why the element is `p a a`: a `+→×` operand owes no answer
-- | and must never starve, so there is no gate here), and so never `[]`
-- | before the first entity and never torn. **The key is a materialized
-- | identity field of the row** (`accumulated @l`), as for `acted @l`.
-- |
-- | Laws:
-- |
-- |   * **nothing at registration** — no emission before the first entity.
-- |   * **singleton** — fed one key only, `accumulatedBy k p` ≈ `p` with its
-- |     releases wrapped in a one-element array.
-- |   * **order** — slots in first-appearance order; a known key's entity
-- |     or emission replaces its slot in place.
-- |   * **whole, at once** — every entity and every element emission
-- |     releases the whole array; one slot changes per release, so it is
-- |     never torn.
-- |   * **no forgetting** — absence of a key is no signal; removal and
-- |     ordering stay upstream, in the array a fold builds.
-- |   * **the slot is its entity's** — `accumulated @l` re-sets field `l`
-- |     from the fed row on every emission (the `Strong` state channel).
-- |
-- | No pure instance, by the impossibility `Retaining` states: the other
-- | keys' slots are state, which a function does not hold.
-- | So this is the one keyed class with the carrier as its only member,
-- | kept because it completes the square — the stated hole, not an
-- | unwritten one. Three forms: `accumulated @l` over the entity's row;
-- | `accumulatedOf @l proj` (`PUI`, a hole-aware projection to the entity:
-- | `# accumulatedOf @"team" goal`, scoreboard); `accumulatedOn @l @c` for
-- | an entity arriving as case `c`, the `+→×` operand owning that case.
module Data.Profunctor.Accumulating
  ( class Accumulating
  , accumulatedBy
  , accumulated
  , accumulatedOn
  ) where

import Prelude

import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Row.Structural (withStructuralOrd)
import Data.Profunctor.Strong (class Strong, second)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (case_, on)
import Prim.Row (class Cons)
import Record (get, set) as Record
import Type.Proxy (Proxy(..))

-- | The class primitive: entities in one at a time, the array of every
-- | key's latest row out, keyed by a function. Carriers implement this; the
-- | vocabulary form is `accumulated @l`.
class Profunctor p <= Accumulating p where
  accumulatedBy :: forall k a. Ord k => (a -> k) -> p a a -> p a (Array a)

-- | The accumulating collection keyed by the entity's identity field `l`,
-- | the key re-set from the input on every emission (see the header).
accumulated :: forall @l r p k b. Accumulating p => Strong p => IsSymbol l => Cons l k b r => p { | r } { | r } -> p { | r } (Array { | r })
accumulated w = withStructuralOrd @k (accumulatedBy (Record.get prox)
  (dimap (\r -> Tuple (Record.get prox r) r) (\(Tuple k out) -> Record.set prox k out) (second w)))
  where
  prox = Proxy @l

-- | `accumulated` for an entity arriving as case `c` of an event row: the
-- | `+→×` operand owning that case, closed to it like every fold.
accumulatedOn :: forall @l @c r p k b v. Accumulating p => Strong p => IsSymbol l => IsSymbol c => Cons l k b r => Cons c { | r } () v => p { | r } { | r } -> p [ | v ] (Array { | r })
accumulatedOn w = lcmap (case_ # on (Proxy @c) identity) (accumulated @l w)
