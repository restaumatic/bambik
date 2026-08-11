-- | The **container action**: lift a UI component over a container of its focus —
-- | `p a b -> p (F a) (F b)` — here at `F = Array`, the container
-- | `μ x. 1 + a × x`. Containers are generated from `×`, `+` and fixpoints,
-- | so this class is not a fifth merge direction: it is the closure of
-- | `Strong` and `Choice` under `μ` (the profunctor traversal), keyed as the
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
-- |     knowledge (contrast `pempty`, whose `{}` is the only value there is).
-- |   * **singleton retraction** — fed `[a]`, behaves as the element fed `a`
-- |     and emits `[b]` per element emission `b` (yanking at the container).
-- |   * **gather gate** — `Array b` is withheld until *every* element has
-- |     emitted at least once; thereafter any element emission re-emits the
-- |     whole array from retained last outputs (the knowledge gate, at
-- |     runtime size).
-- |   * **identity follows key** (stateful carriers) — re-feeding a surviving
-- |     key reuses its instance; permuting keys reorders without rebuilding.
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

import Prelude

import Data.Array (head) as Array
import Data.Maybe (Maybe, maybe)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Row (widenRecordInput)
import Data.Profunctor.Strong (class Strong, second)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons, class Lacks, class Union)
import Record (get, insert) as Record
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
-- | As in `edited`, the element's output row **excludes the key** — each
-- | gathered row's key is re-attached from its *input* row, so an element
-- | structurally cannot forge or change identity. The guarantee is derived
-- | in the pure algebra: the input's key rides around the element on the
-- | `Strong` state channel (`second`), joining each emission.
-- |
-- | The element's *input* row **subsumes**: an element editor reading only
-- | the fields it edits (its key included, so identity can ride around it)
-- | lifts over an array of wider rows with no widening at the site.
acted
  :: forall @l p k ra a narrow extra rb b
   . Acting p => Strong p => IsSymbol l
  => Cons l k ra a => Cons l k rb b => Lacks l rb => Ord k
  => Union narrow extra a
  => p { | narrow } { | rb } -> p (Array { | a }) (Array { | b })
acted w = actedBy (Record.get prox)
  (dimap (\r -> Tuple (Record.get prox r) r) (\(Tuple k out) -> Record.insert prox k out) (second (widenRecordInput w)))
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
  -- the annotation pins `acted`'s subsuming element row to the whole row
  element :: p { key :: String, value :: a } { value :: b }
  element = dimap _.value { value: _ } w
