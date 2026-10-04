-- | The **container action**: lift a UI component over a container of its focus —
-- | `p a b -> p (F a) (F b)` — here at `F = Array`, the container
-- | `μ x. 1 + a × x`. Containers are generated from `×`, `+` and fixpoints,
-- | so this class is not a fifth merge shape: its *type* is the closure
-- | of `Strong` and `Choice` under `μ` (the profunctor traversal,
-- | Jaskelioff–O'Connor). The keyed, retaining semantics is genuine extra
-- | carrier structure beyond that closure — a Strong/Choice-derived
-- | traversal would rebuild per feed where this reconciles — keyed as the
-- | species refinement: on stateful carriers reconciliation is the functorial
-- | action along partial injections of key sets — survivors re-fed in place,
-- | entrants built, leavers removed. **The key is a materialized identity
-- | field of the row** (`acted @l` — the element is a data-model row, and
-- | rows carry their identity; `Ord k` is the reconciler's Map-indexing
-- | requirement — identity semantics remain equality, keys must be unique,
-- | never rendered). Pure carriers
-- | have no identity to preserve and ignore it (`(->)`: `actedBy _ = map`).
-- | See doc/collections-profunctor-algebra.md.
-- |
-- | Laws (the `Array b` output side is a product, so per the unit and gate
-- | laws it announces and gates):
-- |
-- |   * **empty** — fed `[]`, emits `[]` (the inhabited nullary of the `μ`;
-- |     no starvation). No emission *before* the first feed: `[]` is not the
-- |     only `Array b`, so announcing it at registration would fabricate
-- |     knowledge (contrast `{}`, the only value there is, which the wire
-- |     `identity @{}` may echo freely — it says nothing).
-- |   * **singleton retraction** — fed `[a]`, behaves as the element fed `a`
-- |     and emits `[b]` per element emission `b` (yanking at the container).
-- |   * **gather gate** — `Array b` is withheld until *every* element has
-- |     emitted at least once; thereafter any element emission re-emits the
-- |     whole array from retained last outputs. This is the record merges'
-- |     knowledge gate with the row's labels supplied at runtime — on `PUI`
-- |     literally the one pure machine `PUI.Gate.gateStep`, enrolled with
-- |     the fed keys as its participants and rekeyed per feed, so a feed is
-- |     one step (a reconcile whose elements echo gathers once, whole) and
-- |     the empty law is the zero-participant release. Conformance to the
-- |     step over every script to a bound: test/Exhaustive.purs.
-- |   * **identity follows key** (stateful carriers) — re-feeding a surviving
-- |     key reuses its instance; permuting keys reorders without rebuilding.
-- |   * **wire** — `actedBy k identity ≈ identity` at `Array` (elements are
-- |     echo wires, so every fed array gathers whole immediately; tested).
-- |     Composition is deliberately **not** preserved:
-- |     `actedBy k (p >>> q) ≠ actedBy k p >>> actedBy k q` in general —
-- |     the right side gathers twice and re-feeds every `q`-element per
-- |     `p`-element emission — so lift whole element pipelines, not stages.
-- |
-- | This module is the **pure algebra** — the class (whose primitive
-- | `actedBy` takes the key as a function, the minimal carrier obligation),
-- | the label form `acted @l`, the `(->)` face, and the derived `Maybe`
-- | action. The carrier machinery lives with the carriers, exactly as the
-- | merge classes' instances do: `PUI` holds the shared keyed reconciler,
-- | the generic `Hosting m node => Acting (PUI m)` instance and the sibling
-- | collection combinators (`foreach`, `edited`, `dispatched`,
-- | `accumulated`); each display carrier holds its own `Hosting` instance.
module Data.Profunctor.Acting
  ( class Acting
  , actedBy
  , acted
  , optioned
  ) where

import Data.Profunctor.Row.Structural (withStructuralOrd)
import Prelude

import Data.Array (head) as Array
import Data.Maybe (Maybe, maybe)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Strong (class Strong, second)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons)
import Record (get, set) as Record
import Type.Proxy (Proxy(..))

-- | The class primitive: lift a UI component over the `Array` container, keyed by
-- | a function. Carriers implement this; the vocabulary form is `acted @l`.
class Profunctor p <= Acting p where
  actedBy :: forall k a b. Ord k => (a -> k) -> p a b -> p (Array a) (Array b)

-- | Pure carriers have no element identity to preserve — the key is species
-- | bookkeeping for stateful instances, so `(->)` ignores it.
instance Acting (->) where
  actedBy _ = map

-- | Lift a UI component over the keyed `Array` container (see the module header
-- | for the laws), keyed by the row's materialized identity field `@l`.
-- | Written trailing, like the merges' operands: `row # acted @"id"`.
-- |
-- | As in `edited`, the element is typed at its **whole row, key
-- | included**, on both sides — an item that reads its key beside a
-- | whole-row editor that echoes it needs exactly that — and the carrier
-- | **re-sets** the key on every emission from the element's *input* row,
-- | so an element cannot forge or change identity: whatever it emits in
-- | the key field is replaced. The guarantee is derived in the pure
-- | algebra: the input's key rides around the element on the `Strong`
-- | state channel (`second`) and is written over each emission
-- | (`Record.set`). An element's functions are typed at its row
-- | (guardrails L18).
acted :: forall @l @r p k b1 b2 r1 . Acting p => Strong p => IsSymbol l => Cons l k b1 r => Cons l k b2 r1 => p { | r } { | r1 } -> p (Array { | r }) (Array { | r1 })
acted w = withStructuralOrd @k (actedBy (Record.get prox)
  (dimap (\r -> Tuple (Record.get prox r) r) (\(Tuple k out) -> Record.set prox k out) (second w)))
  where
  prox = Proxy @l

-- | The `Maybe = 1 + a` container action, derived: `Maybe` embeds in `Array`
-- | as the at-most-one-element arrays (identity is trivial at one element, so
-- | the key is a constant). Keeps the element *fed and live* on
-- | `Nothing`-to-`Just` transitions per the carrier's retention; contrast a
-- | carrier's *detaching* visibility form, which drops the element and
-- | collapses its output.
optioned :: forall p a b. Acting p => Strong p => p a b -> p (Maybe a) (Maybe b)
optioned w = dimap (maybe [] \x -> [ { key: "the", value: x } ]) (Array.head >>> map _.value)
  (acted @"key" element)
  where
  -- the element sees and emits its whole row; the carrier re-sets the key
  element :: p { key :: String, value :: a } { key :: String, value :: b }
  element = dimap _.value { key: "the", value: _ } w
