-- | The **container action**: lift a widget over a container of its focus —
-- | `p a b -> p (F a) (F b)` — here at `F = Array`, the container
-- | `μ x. 1 + a × x`. Containers are generated from `×`, `+` and fixpoints,
-- | so this class is not a fifth merge direction: it is the closure of
-- | `Strong` and `Choice` under `μ` (the profunctor traversal), with the
-- | **key function as the species refinement** — shapes carry a finite key
-- | set (`a -> String`), and on stateful carriers reconciliation is the
-- | functorial action along partial injections of key sets: survivors re-fed
-- | in place, entrants built, leavers removed. Pure carriers have no identity
-- | to preserve, so they ignore the key (`(->)`: `acted _ = map`).
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
-- | This module is the **pure algebra** — the class, its `(->)` face, and the
-- | derived `Maybe` action. The carrier machinery lives with the carriers,
-- | exactly as the merge classes' instances do: `PUI` holds the shared keyed
-- | reconciler, the generic `Hosting m node => Acting (PUI m)` instance and
-- | the collapsed (sum-flavored, forwarding) form `collapsed`; `PUI.Web`
-- | holds the DOM `Hosting` instance.
module Data.Profunctor.Acting
  ( class Acting
  , acted
  , optioned
  ) where

import Prelude

import Data.Array (head) as Array
import Data.Maybe (Maybe, maybe)
import Data.Profunctor (class Profunctor, dimap)

-- | Lift a widget over the keyed `Array` container (see the module header
-- | for the laws). Written trailing, like the merges' operands:
-- | `row # acted _.id`.
class Profunctor p <= Acting p where
  acted :: forall a b. (a -> String) -> p a b -> p (Array a) (Array b)

-- | Pure carriers have no element identity to preserve — the key is species
-- | bookkeeping for stateful instances, so `(->)` ignores it.
instance Acting (->) where
  acted _ = map

-- | The `Maybe = 1 + a` container action, derived: `Maybe` embeds in `Array`
-- | as the at-most-one-element arrays. Keeps the element *fed and live* on
-- | `Nothing`-to-`Just` transitions per the carrier's retention; contrast
-- | `PUI.HTML.provided`, the *detaching* visibility form with collapsed
-- | output.
optioned :: forall p a b. Acting p => p a b -> p (Maybe a) (Maybe b)
optioned = dimap (maybe [] pure) Array.head <<< acted (const "the")
