-- | The **keyed dispatch**: the `+→+` member of the keyed square, `Choice`'s
-- | analogue over a runtime key — one instance of a component per key
-- | value, each fed entity reaching exactly the instance its key names.
-- | Where `Choice` dispatches one *static* case to its one handler, this
-- | dispatches one *runtime* case: an unknown key instantiates a new
-- | instance (a new case, not an error), a known key re-feeds its instance,
-- | and the instance's emissions leave as they come. **The key is a
-- | materialized identity field of the row** (`dispatched @l`), as it is
-- | for `acted @l`: the entity is a data-model row and rows carry their
-- | identity, so no envelope is written.
-- |
-- | Laws:
-- |
-- |   * **wire** — `dispatchedBy k identity ≈ identity`.
-- |   * **naturality** — `dispatchedBy (k <<< f) (dimap f g p) ≈ dimap f g
-- |     (dispatchedBy k p)`.
-- |   * **singleton** — fed one key only, `dispatchedBy k p ≈ p`.
-- |   * **one instance per key** (stateful carriers) — a known key reaches
-- |     the registration its first appearance made; an unknown key makes a
-- |     new one; no key is ever forgotten, since absence is no signal.
-- |   * **the emission is its entity's** — `dispatched @l` re-sets field
-- |     `l` from the fed row on every emission, so an instance cannot forge
-- |     or change identity (the `Strong` state channel, as in `acted`).
-- |
-- | Three forms, like every member of the keyed square: `dispatched @l`
-- | over the entity's row; `dispatchedOf @l proj` (`PUI`, a hole-aware
-- | projection from the fed value to the entity: `# dispatchedOf @"code"
-- | arrival`, departures); `dispatchedOn @l @c` for an entity arriving as
-- | case `c` of the event, `focusCase @c` over the bare form.
module Data.Profunctor.Dispatching
  ( class Dispatching
  , dispatchedBy
  , dispatched
  , dispatchedOn
  ) where

import Prelude

import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Row.Structural (withStructuralOrd)
import Data.Profunctor.Row.VariantToVariant (focusCase)
import Data.Profunctor.Strong (class Strong, second)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons)
import Record (get, set) as Record
import Type.Proxy (Proxy(..))

-- | The class primitive: one instance per key value, keyed by a function.
-- | Carriers implement this; the vocabulary form is `dispatched @l`.
class Profunctor p <= Dispatching p where
  dispatchedBy :: forall k a b. Ord k => (a -> k) -> p a b -> p a b

-- | One instance per value of the entity's identity field `l`, the entity
-- | typed at its whole row on both sides and the key re-set from the input
-- | on every emission (see the header).
dispatched :: forall @l r p k b1 b2 r1. Dispatching p => Strong p => IsSymbol l => Cons l k b1 r => Cons l k b2 r1 => p { | r } { | r1 } -> p { | r } { | r1 }
dispatched w = withStructuralOrd @k (dispatchedBy (Record.get prox)
  (dimap (\r -> Tuple (Record.get prox r) r) (\(Tuple k out) -> Record.set prox k out) (second w)))
  where
  prox = Proxy @l

-- | `dispatched` for an entity arriving as case `c` of an event row, every
-- | other case passing: `focusCase @c` over the bare form.
dispatchedOn :: forall @l @c r p k b1 b2 r1 b3 v1 v2. Dispatching p => Strong p => Choice p => IsSymbol l => IsSymbol c => Cons l k b1 r => Cons l k b2 r1 => Cons c { | r } b3 v1 => Cons c { | r1 } b3 v2 => p { | r } { | r1 } -> p [ | v1 ] [ | v2 ]
dispatchedOn w = focusCase @c (dispatched @l w)
