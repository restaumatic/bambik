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
-- |   * **wire** — `acted @l identity ≈ identity` at `Array` (elements are
-- |     echo wires, so every fed array gathers whole immediately).
-- |     Composition is deliberately **not** preserved:
-- |     `acted @l (p >>> q) ≠ acted @l p >>> acted @l q` in general —
-- |     the right side gathers twice and re-feeds every `q`-element per
-- |     `p`-element emission — so lift whole element pipelines, not stages.
-- |
-- | ## Forms
-- |
-- | Three, like every member of the keyed square:
-- |
-- |   * `acted @l` — over the element's row.
-- |   * `actedOf @l proj` (a projection to the array) —
-- |     the list of displays, whose releases stay inside and whose fed row
-- |     passes on (`item # actedOf @"id" rowsOf`).
-- |   * `actedAt @l @t` — over an array the model stores at field `t`
-- |     (`item # actedAt @"id" @"Setlist"`, reorder).
-- |
-- | and one that is this member's alone, since only an array output has
-- | somewhere to be written back:
-- |
-- |   * `actedWithin @l @t select` — over the rows `select` picks from the
-- |     array the model stores at field `t`, each release written back
-- |     where its key is stored (`form # actedWithin @"id" @"contacts"
-- |     pickedContact`, contacts).
-- |
-- | The three forms that read the fed value are **one lens over `acted`**,
-- | told apart by their setter — the three answers to the view-update
-- | problem: `actedOf` writes nothing back (`const`: a derived array is a
-- | view with no base), `actedAt` writes the array back whole (`prop`: the
-- | view is the base), `actedWithin` writes each row back by its key (the
-- | view is a selection of the base).
-- |
-- | ## The carrier
-- |
-- | This module holds the class, whose primitive `acting` takes each
-- | element beside its key, and the label forms. The instance is the carrier's, as the
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
  , acted
  , actedOf
  , actedAt
  , actedWithin
  ) where

import Prelude

import Data.Lens.Lens (lens)
import Data.Lens.Record (prop)
import Data.Map (fromFoldable, lookup) as Map
import Data.Maybe (fromMaybe)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Keyed (keyRestored, keyed)
import Data.Profunctor.Strong (class Strong)
import Data.Structural (withStructuralOrd)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple)
import Prim.Row (class Cons)
import Record (get, modify) as Record
import Type.Proxy (Proxy(..))

-- | The class primitive: lift a UI component over the keyed `Array`
-- | container, every element riding beside its key. The element never sees
-- | the key, so it cannot forge it. Carriers implement this; the vocabulary
-- | form is `acted @l`.
class Profunctor p <= Acting p where
  acting :: forall k a b. Ord k => p a b -> p (Array (Tuple k a)) (Array (Tuple k b))

-- | The container action keyed by the element's identity field `l`, the
-- | element typed at its whole row on both sides and the key re-set from
-- | the input on every emission (see the header). Written trailing, like
-- | the merges' operands: `row # acted @"id"`. An element's functions are
-- | typed at its row.
acted :: forall @l r r1 p k b1 b2. Acting p => IsSymbol l => Cons l k b1 r => Cons l k b2 r1 => p { | r } { | r1 } -> p (Array { | r }) (Array { | r1 })
acted w = withStructuralOrd @k (dimap (map (keyed @l)) (map (keyRestored @l)) (acting w))

-- | `acted` over a projection of the fed value — the **list of displays**:
-- | the lens whose getter is the projection and whose setter keeps the
-- | value fed, so the elements' gathered releases stay inside and the fed
-- | value passes on once the gate releases (`ul $ item # actedOf @"id"
-- | rowsOf`). A projected array is a view with nothing to write back; an
-- | array the model stores is `actedAt`.
actedOf :: forall @l r r1 p k b1 b2 a. Acting p => Strong p => IsSymbol l => Cons l k b1 r => Cons l k b2 r1 => (a -> Array { | r }) -> p { | r } { | r1 } -> p a a
actedOf f w = lens f const (acted @l w)

-- | `acted` over an array the model stores: keyed by field `l` of each
-- | element, the array field `t` of the fed row, both named by label
-- | (`item # actedAt @"id" @"Setlist"`), the rest of the row carried —
-- | `Strong`'s field lens, the ecosystem's `prop`, over the bare form. The
-- | MDC vocabularies' `group @t` does the same lift with its card and
-- | heading; the plain-HTML floor names the field here (reorder-html).
actedAt :: forall @l @t p k b1 b2 b3 r1 r2 r3 r. Acting p => Strong p => IsSymbol l => IsSymbol t => Cons l k b1 r1 => Cons l k b2 r2 => Cons t (Array { | r1 }) b3 r3 => Cons t (Array { | r2 }) b3 r => p { | r1 } { | r2 } -> p { | r3 } { | r }
actedAt w = prop (Proxy @t) (acted @l w)


-- | `acted` over a **selection** of an array the model stores: the rows
-- | `select` picks from field `t` of the fed row are the elements, keyed
-- | by their field `l`, and every release is written back **where its key
-- | is stored** — the rows of `t` carrying the released keys replaced, the
-- | others and the rest of the fed row kept. It is the lens whose getter
-- | is the selection and whose setter lands rows by key, over the bare
-- | form: the updatable view, between `actedOf` (nothing written back) and
-- | `actedAt` (the array written back whole). An editor's edit therefore
-- | reaches the stored array through a filter, a sort or a pick
-- | (`row # actedWithin @"title" @"movies" visibleMovies`, movie-browser;
-- | the form of the one picked row, `form # actedWithin @"id" @"contacts"
-- | pickedContact`, contacts).
-- |
-- | `select` chooses and orders rows **of `t`**; it cannot reshape one
-- | (the element is typed at the stored row) and must not invent one: a
-- | released row whose key `t` does not hold has no place to land and is
-- | not written. Laws:
-- |
-- |   * **wire** — `actedWithin @l @t select identity ≈ identity`: rows
-- |     written back unchanged are the row fed (the lens's GetPut).
-- |   * **landing** — an element's release replaces, in `t`, the row of
-- |     its key and nothing else; its key is the fed one, unforgeable.
-- |   * **agreement** — `actedWithin @l @t (get t) ≈ actedAt @l @t` over
-- |     one element row: the whole array selected is the array stored.
-- |
-- | PutGet is the selection's to keep: an edit that takes its row out of
-- | `select` lands, and the next feed drops the element — the row left
-- | the view, as a favourite unstarred leaves the favourites.
actedWithin
  :: forall @l @t p k b1 b2 r1 r
   . Acting p
  => Strong p
  => IsSymbol l
  => IsSymbol t
  => Cons l k b1 r1
  => Cons t (Array { | r1 }) b2 r
  => ({ | r } -> Array { | r1 })
  -> p { | r1 } { | r1 }
  -> p { | r } { | r }
actedWithin select w = lens select landed (acted @l w)
  where
  landed :: { | r } -> Array { | r1 } -> { | r }
  landed fed released = withStructuralOrd @k
    ( let
        byKey = Map.fromFoldable (map (keyed @l) released)
      in
        Record.modify (Proxy @t) (map \stored -> fromMaybe stored (Map.lookup (Record.get (Proxy @l) stored) byKey)) fed
    )
