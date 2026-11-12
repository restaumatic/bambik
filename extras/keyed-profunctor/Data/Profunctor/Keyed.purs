-- | The shared floor of the **keyed square** — what every keyed shape's
-- | module (`Data.Profunctor.Keyed.*`) stands on, as `Data.Profunctor.Row`
-- | is the floor of the row shapes.
-- |
-- | ## The square
-- |
-- | A pair of tensors — `⊗` on the input, `⊕` on the output, each `×` or
-- | `+` — fixes a strength, `p a b -> p (a ⊗ c) (b ⊕ c)`, a row merge
-- | (`Data.Profunctor.Row.*`) and a **keyed lift**, the strength's analogue
-- | over a runtime key:
-- |
-- |     p a b -> p (⊗ₖ a) (⊕ₖ b)
-- |
-- | The `×`-power over the keys is the keyed array, `Array (Tuple k a)`:
-- | every key's value at once. The `+`-power is one keyed entity,
-- | `Tuple k a`: one key's value at a time. So the four members are one
-- | formula read at four tensor pairs, each a minimal **unary** carrier
-- | primitive:
-- |
-- | | Shape | Strength | Class | Primitive |
-- | | --- | --- | --- | --- |
-- | | `×→×` | `Strong` | `Acting` | `acting :: p a b -> p (Array (Tuple k a)) (Array (Tuple k b))` |
-- | | `+→+` | `Choice` | `Dispatching` | `dispatching :: p a b -> p (Tuple k a) (Tuple k b)` |
-- | | `×→+` | `Resolving` | `Gathering` | `gathering :: p a b -> p (Array (Tuple k a)) (Tuple k b)` |
-- | | `+→×` | `Retaining` | `Accumulating` | `accumulating :: p a b -> p (Tuple k a) (Array (Tuple k b))` |
-- |
-- | What a side's tensor decides is the same at every member. A `×` input
-- | is the whole key set, so it is **reconciled**: survivors re-fed in
-- | place, entrants built, leavers removed. A `+` input is one key, so an
-- | unknown key builds an instance, a known key re-feeds it, and absence is
-- | no signal — nothing is ever removed. A `×` output is a product over
-- | the keys, so it **gates**: withheld until every key's element has
-- | spoken, whole thereafter (the record merges' gate with the labels
-- | supplied at runtime). A `+` output is one element's emission,
-- | **passed** as it comes.
-- |
-- | Rows are the finitary `μ`-free fragment of the container grammar, and
-- | `Array = μ x. 1 + a × x` is one `μ` later, so the square stands beside
-- | the row shapes, not under them — in its own source root,
-- | `extras/keyed-profunctor`, which imports nothing from the row root's
-- | (doc/collections-profunctor-algebra.md).
-- |
-- | ## The key
-- |
-- | **A materialized identity field of the row**: an element is a
-- | data-model row, and rows carry their identity, so a label form names
-- | the field (`acted @"id"`) and no envelope is written. `Ord k` is the
-- | carrier's Map-indexing requirement — identity semantics remain
-- | equality — and the label forms supply it structurally
-- | (`Data.Structural`), so no instance is asked of the key's type. Keys
-- | must be unique and are never rendered.
-- |
-- | In the class primitives the key rides **beside** the element, in a
-- | `Tuple`, out of the element's reach: an element cannot forge or change
-- | identity. The label forms are `dimap`s over the primitives with the
-- | two functions this module holds: `keyed @l` pairs a row with its field
-- | `l` on the way in, `keyRestored @l` writes the key back over field `l`
-- | of an emission on the way out — whatever the element emitted there is
-- | replaced. Law: `keyRestored @l (keyed @l r) = r`.
-- |
-- | ## The forms
-- |
-- | Three per member, so the twelve words read alike:
-- |
-- |   * the **bare** word, over the element typed at its whole row, key
-- |     field included — `acted @l`, `dispatched @l`, `gathered @l`,
-- |     `accumulated @l`;
-- |   * `…Of @l proj`, over a projection of the fed value — written in
-- |     `PUI`, since a hole projection is absent (guardrails L18);
-- |   * the **stored** form: `…At @l @t` over an array the model stores at
-- |     field `t`, for a `×` input (`actedAt`, `gatheredAt`); `…On @l @c`
-- |     over the case `c` an entity arrives in, for a `+` input
-- |     (`dispatchedOn`, `accumulatedOn`).
-- |
-- | Each member's module carries its class, its laws and its bare and
-- | stored forms; the instances are the carrier's, as the merge classes'
-- | are. No keyed module imports a row shape's: a stored form that keeps
-- | the rest of what it was fed is its strength's ecosystem optic over the
-- | bare form (`actedAt` is `prop`, `dispatchedOn` is `prism`), the other
-- | two a plain `lcmap`.
module Data.Profunctor.Keyed
  ( keyed
  , keyRestored
  ) where

import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons)
import Record (get, set) as Record
import Type.Proxy (Proxy(..))

-- | A row beside its identity field `l`: the input half of every label
-- | form.
keyed :: forall @l k b r. IsSymbol l => Cons l k b r => { | r } -> Tuple k { | r }
keyed r = Tuple (Record.get (Proxy @l) r) r

-- | The key written back over field `l` of an emission: the output half
-- | of the label forms whose output is the element's row.
keyRestored :: forall @l k b r. IsSymbol l => Cons l k b r => Tuple k { | r } -> { | r }
keyRestored (Tuple k out) = Record.set (Proxy @l) k out
