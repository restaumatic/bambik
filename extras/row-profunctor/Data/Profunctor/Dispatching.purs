-- | The **keyed dispatch**: the `+→+` member of the keyed square, `Choice`'s
-- | analogue over a runtime key — entities fed one at a time, each reaching
-- | exactly the instance its key names, that instance's emissions leaving
-- | as they come. Where `Choice` dispatches one *static* case to its one
-- | handler, this dispatches one *runtime* case: an unknown key
-- | instantiates a new instance (a new case, not an error), a known key
-- | re-feeds its instance.
-- |
-- | ## The key
-- |
-- | **A materialized identity field of the row** (`dispatched @l`), as for
-- | `acted @l`: the entity is a data-model row and rows carry their
-- | identity, so no envelope is written. The entity is typed at its whole
-- | row on both sides, and the key is **re-set** on every emission from the
-- | fed row, so an instance cannot forge or change identity: in the class
-- | primitive the key rides beside the entity, `Tuple k a`, out of the
-- | instance's reach, as in `acting`.
-- |
-- | ## Laws
-- |
-- |   * **wire** — `dispatching identity ≈ identity`.
-- |   * **naturality** — `dispatching (dimap f g p) ≈ dimap (map f) (map g)
-- |     (dispatching p)`.
-- |   * **singleton** — fed one key only, `dispatching p ≈ second p`.
-- |   * **one instance per key** — a known key reaches the registration its
-- |     first appearance made; an unknown key makes a new one.
-- |   * **no forgetting** — absence of a key is no signal, so no instance
-- |     is ever removed.
-- |
-- | ## Forms
-- |
-- | Three, like every member of the keyed square:
-- |
-- |   * `dispatched @l` — over the entity's row.
-- |   * `dispatchedOf @l proj` (`PUI`, a hole-aware projection from the fed
-- |     value to the entity) — `# dispatchedOf @"code" arrival`, departures.
-- |   * `dispatchedOn @l @c` — for an entity arriving as case `c` of the
-- |     event, every other case passing: `focusCase @c` over the bare form.
-- |
-- | ## The carrier
-- |
-- | This module holds the class, whose primitive `dispatching` takes the
-- | entity beside its key, and the label forms. The instance is the carrier's:
-- | `PUI` holds a generic `Hosting m node =>` instance, one registration
-- | per key value, never detached or restacked.
module Data.Profunctor.Dispatching
  ( class Dispatching
  , dispatching
  , dispatched
  , dispatchedOn
  ) where

import Prelude

import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Row.Structural (withStructuralOrd)
import Data.Profunctor.Row.VariantToVariant (focusCase)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons)
import Record (get, set) as Record
import Type.Proxy (Proxy(..))

-- | The class primitive: one instance per key value, the entity riding
-- | beside its key and each emission leaving beside the key it was fed
-- | under. Carriers implement this; the vocabulary form is `dispatched @l`.
class Profunctor p <= Dispatching p where
  dispatching :: forall k a b. Ord k => p a b -> p (Tuple k a) (Tuple k b)

-- | One instance per value of the entity's identity field `l`, the entity
-- | typed at its whole row on both sides and the key re-set from the input
-- | on every emission (see the header).
dispatched :: forall @l r p k b1 b2 r1. Dispatching p => IsSymbol l => Cons l k b1 r => Cons l k b2 r1 => p { | r } { | r1 } -> p { | r } { | r1 }
dispatched w = withStructuralOrd @k
  (dimap (\r -> Tuple (Record.get prox r) r) (\(Tuple k out) -> Record.set prox k out) (dispatching w))
  where
  prox = Proxy @l

-- | `dispatched` for an entity arriving as case `c` of an event row, every
-- | other case passing: `focusCase @c` over the bare form.
dispatchedOn :: forall @l @c r p k b1 b2 r1 b3 v1 v2. Dispatching p => Choice p => IsSymbol l => IsSymbol c => Cons l k b1 r => Cons l k b2 r1 => Cons c { | r } b3 v1 => Cons c { | r1 } b3 v2 => p { | r } { | r1 } -> p [ | v1 ] [ | v2 ]
dispatchedOn w = focusCase @c (dispatched @l w)
