-- | The **accumulating collection**: the `+→×` member of the keyed square,
-- | `Retaining`'s analogue over a runtime key — entities fed one at a time,
-- | the array of every key's latest output released whole. A keyed Mealy
-- | machine: an unknown key appends a slot, a known key re-feeds its
-- | element, and each element's emission writes its slot. The output is a
-- | product over the keys seen so far, so, like `Acting`'s, it gates.
-- |
-- | ## The key
-- |
-- | The square's (`Data.Profunctor.Keyed`, *The key*): field `l` of the
-- | entity's row. The key is **re-set** on every emission from the fed row
-- | (`keyed @l` in, `keyRestored @l` out), so a slot is its entity's.
-- |
-- | ## Laws
-- |
-- |   * **nothing at registration** — no emission before the first entity,
-- |     so never `[]`.
-- |   * **singleton** — fed one key only, `accumulating p` ≈ `p` beside its
-- |     key, each release wrapped in a one-element array.
-- |   * **order** — slots in first-appearance order; a known key's element
-- |     emission replaces its slot in place.
-- |   * **gather gate** — the array is withheld until *every* key's element
-- |     has emitted at least once; thereafter every entity and every element
-- |     emission releases the whole array from retained last outputs. A feed
-- |     is one step, so over elements that answer their feed (displays and
-- |     editors, by the `×→×` laws) each entity releases once, whole.
-- |   * **no forgetting** — absence of a key is no signal; removal and
-- |     ordering stay upstream, in the array a fold builds.
-- |
-- | ## Forms
-- |
-- | Three, like every member of the keyed square:
-- |
-- |   * `accumulated @l` — over the entity's row.
-- |   * `accumulatedOf @l proj` (`PUI`, a hole-aware projection to the
-- |     entity) — `# accumulatedOf @"team" goal`, scoreboard.
-- |   * `accumulatedOn @l @c` — for an entity arriving as case `c`, the
-- |     `+→×` operand owning that case, closed to it like every fold.
-- |
-- | ## The carrier
-- |
-- | This module holds the class, whose primitive `accumulating` takes the
-- | entity beside its key, and the label forms. The instance is the
-- | carrier's: `PUI` holds a generic `Hosting m node =>` instance, never
-- | detached or restacked, its output the one pure machine
-- | `PUI.Gate.gateStep` that `Acting` gathers with, the keys enrolled in
-- | first-appearance order.
module Data.Profunctor.Keyed.Accumulating
  ( class Accumulating
  , accumulating
  , accumulated
  , accumulatedOn
  ) where

import Prelude

import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Keyed (keyRestored, keyed)
import Data.Structural (withStructuralOrd)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple)
import Data.Variant (case_, on)
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))

-- | The class primitive: entities in one at a time, each beside its key,
-- | the keyed array of every key's latest output out. Carriers implement
-- | this; the vocabulary form is `accumulated @l`.
class Profunctor p <= Accumulating p where
  accumulating :: forall k a b. Ord k => p a b -> p (Tuple k a) (Array (Tuple k b))

-- | The accumulating collection keyed by the entity's identity field `l`,
-- | the key re-set from the input on every emission (see the header).
accumulated :: forall @l r r1 p k b1 b2. Accumulating p => IsSymbol l => Cons l k b1 r => Cons l k b2 r1 => p { | r } { | r1 } -> p { | r } (Array { | r1 })
accumulated w = withStructuralOrd @k (dimap (keyed @l) (map (keyRestored @l)) (accumulating w))

-- | `accumulated` for an entity arriving as case `c` of an event row: the
-- | `+→×` operand owning that case, closed to it like every fold.
accumulatedOn :: forall @l @c r r1 p k b1 b2 v. Accumulating p => IsSymbol l => IsSymbol c => Cons l k b1 r => Cons l k b2 r1 => Cons c { | r } () v => p { | r } { | r1 } -> p [ | v ] (Array { | r1 })
accumulatedOn w = lcmap (case_ # on (Proxy @c) identity) (accumulated @l w)
