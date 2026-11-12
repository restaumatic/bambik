-- | The **container action**: the `×→×` member of the keyed square,
-- | `Strong`'s analogue over a runtime key — the array fed whole, each
-- | element fed its own row, the array of every element's latest output
-- | released whole. It lifts a component over a container of its focus,
-- | `p a b -> p (F a) (F b)`, here at `F = Array`, the container
-- | `μ x. 1 + a × x`. Containers are generated from `×`, `+` and fixpoints,
-- | so this is not a fifth merge shape: its *type* is the closure of
-- | `Strong` and `Choice` under `μ` (the profunctor traversal,
-- | Jaskelioff–O'Connor). The keyed, retaining semantics is extra carrier
-- | structure beyond that closure — a derived traversal would rebuild per
-- | feed where this reconciles: survivors re-fed in place, entrants built,
-- | leavers removed (the functorial action along partial injections of key
-- | sets, the species refinement). See doc/collections-profunctor-algebra.md.
-- |
-- | ## The key
-- |
-- | The square's (`Data.Profunctor.Keyed`, *The key*): field `l` of the
-- | element's row. The element is typed at its whole row, key included, on
-- | both sides, and the key is **re-set** on every emission from the
-- | element's *input* row (`keyed @l` in, `keyRestored @l` out).
-- |
-- | ## Laws
-- |
-- | The `Array b` output side is a product, so, like the record merges'
-- | output, it gates.
-- |
-- |   * **empty** — fed `[]`, emits `[]` (the inhabited nullary of the `μ`;
-- |     no starvation). No emission *before* the first feed: `[]` is not the
-- |     only `Array b`, so announcing it at registration would fabricate
-- |     knowledge (contrast `{}`, the only value there is, which the wire
-- |     `identity @{}` may echo freely — it says nothing).
-- |   * **singleton** — fed `[a]`, behaves as the element fed `a` and emits
-- |     `[b]` per element emission `b` (yanking at the container).
-- |   * **gather gate** — `Array b` is withheld until *every* element has
-- |     emitted at least once; thereafter any element emission re-emits the
-- |     whole array from retained last outputs. A feed is one step: a
-- |     reconcile whose elements echo gathers once, whole.
-- |   * **identity follows key** — re-feeding a surviving key reuses its
-- |     instance; permuting keys reorders without rebuilding.
-- |   * **wire** — `actedBy k identity ≈ identity` at `Array` (elements are
-- |     echo wires, so every fed array gathers whole immediately).
-- |     Composition is deliberately **not** preserved:
-- |     `actedBy k (p >>> q) ≠ actedBy k p >>> actedBy k q` in general —
-- |     the right side gathers twice and re-feeds every `q`-element per
-- |     `p`-element emission — so lift whole element pipelines, not stages.
-- |
-- | ## Forms
-- |
-- | Three, like every member of the keyed square:
-- |
-- |   * `acted @l` — over the element's row.
-- |   * `actedOf @l proj` (`PUI`, a hole-aware projection to the array) —
-- |     the list of displays, whose releases stay inside and whose fed row
-- |     passes on (`item # actedOf @"id" rowsOf`).
-- |   * `actedAt @l @t` — over an array the model stores at field `t`
-- |     (`item # actedAt @"id" @"Setlist"`, reorder).
-- |
-- | ## The carrier
-- |
-- | This module holds the class, whose primitive `acting` takes each
-- | element beside its key (`actedBy` is it with the pairing written from
-- | a key function), and the label forms. The instance is the carrier's, as the
-- | merge classes' are: `PUI` holds the shared keyed reconciler and a
-- | generic `Hosting m node =>` instance, each display carrier its own
-- | `Hosting`. The gather gate is the record merges' knowledge gate with
-- | the row's labels supplied at runtime — literally the one pure machine
-- | `PUI.Gate.gateStep`, enrolled with the fed keys as its participants and
-- | rekeyed per feed, the empty law its zero-participant release.
-- | Conformance to the step over every script to a bound:
-- | test/Exhaustive.purs.
module Data.Profunctor.Keyed.Acting
  ( class Acting
  , acting
  , actedBy
  , acted
  , actedAt
  ) where

import Prelude

import Data.Lens.Record (prop)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Keyed (keyRestored, keyed)
import Data.Profunctor.Strong (class Strong)
import Data.Structural (withStructuralOrd)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), snd)
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))

-- | The class primitive: lift a UI component over the keyed `Array`
-- | container, every element riding beside its key. The element never sees
-- | the key, so it cannot forge it. Carriers implement this; the vocabulary
-- | form is `acted @l`.
class Profunctor p <= Acting p where
  acting :: forall k a b. Ord k => p a b -> p (Array (Tuple k a)) (Array (Tuple k b))

-- | `acting` keyed by a function of the element, the keys dropped from the
-- | output: the primitive with its pairing written for it (`Profunctor`
-- | alone).
actedBy :: forall p k a b. Acting p => Ord k => (a -> k) -> p a b -> p (Array a) (Array b)
actedBy key w = dimap (map \a -> Tuple (key a) a) (map snd) (acting w)

-- | The container action keyed by the element's identity field `l`, the
-- | element typed at its whole row on both sides and the key re-set from
-- | the input on every emission (see the header). Written trailing, like
-- | the merges' operands: `row # acted @"id"`. An element's functions are
-- | typed at its row (guardrails L18).
acted :: forall @l r p k b1 b2 r1 . Acting p => IsSymbol l => Cons l k b1 r => Cons l k b2 r1 => p { | r } { | r1 } -> p (Array { | r }) (Array { | r1 })
acted w = withStructuralOrd @k (dimap (map (keyed @l)) (map (keyRestored @l)) (acting w))

-- | `acted` over an array the model stores: keyed by field `l` of each
-- | element, the array field `t` of the fed row, both named by label
-- | (`item # actedAt @"id" @"Setlist"`), the rest of the row carried —
-- | `Strong`'s field lens, the ecosystem's `prop`, over the bare form. The
-- | MDC vocabularies' `group @t` does the same lift with its card and
-- | heading; the plain-HTML floor names the field here (reorder-html).
actedAt :: forall @l @t p k b1 b2 r1 r. Acting p => Strong p => IsSymbol l => IsSymbol t => Cons l k b1 r1 => Cons t (Array { | r1 }) b2 r => p { | r1 } { | r1 } -> p { | r } { | r }
actedAt w = prop (Proxy @t) (acted @l w)

