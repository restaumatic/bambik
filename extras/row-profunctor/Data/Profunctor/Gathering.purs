-- | The **gathering collection**: the `×→+` member of the keyed square,
-- | `Resolving`'s analogue over a runtime key — the array fed whole, each
-- | element fed its own row, every element's emission leaving on the one
-- | shared channel as it happens. It shares `Acting`'s keyed, retaining
-- | reconciliation (survivors re-fed in place, entrants built, leavers
-- | removed), so a clickable row or a cell reporting its key keeps its node
-- | across feeds; what differs is the output side, a variant, which no gate
-- | can wait on.
-- |
-- | ## The key
-- |
-- | **A materialized identity field of the row** (`gathered @l`), as for
-- | `acted @l`. The class primitive emits each event beside its element's
-- | key, `Tuple k b`; the label form drops it, the output being an event
-- | and not the element's row, so there is no key field to re-set.
-- |
-- | ## Laws
-- |
-- |   * **empty** — fed `[]`, nothing: there is no event to fabricate.
-- |   * **singleton** — fed `[a]`, behaves as the element fed `a`.
-- |   * **passage** — each element emission is forwarded at once, untagged
-- |     and ungated; nothing is retained for the channel.
-- |   * **arming** — a feed fires nothing: the `×→+` Answer law. It is the
-- |     elements' to keep, so the class is lawful over operands that arm
-- |     (emitters; `editsOf` makes an editor one by dropping its answers
-- |     inside the feed) — as `Resolving` is lawful only at `PUI`'s
-- |     quiescence. There is no wire law: `gathering identity` would
-- |     forward each element's echo inside the feed.
-- |   * **monotonicity** — `p ⊑ p'` implies `gathering p ⊑ gathering p'`.
-- |
-- | ## Forms
-- |
-- | Three, like every member of the keyed square:
-- |
-- |   * `gathered @l` — over the element's row.
-- |   * `gatheredOf @l proj` (`PUI`, a hole-aware projection to the array) —
-- |     `tile @"Key entered" @"key" … # gatheredOf @"key" keyPad`;
-- |     MDC2/MDC3's `listOf` is this over picked rows.
-- |   * `gatheredAt @l @t` — over an array the model stores at field `t`
-- |     (`# gatheredAt @"key" @"cells"`, cells).
-- |
-- | ## The carrier
-- |
-- | This module holds the class, whose primitive `gathering` takes each
-- | element beside its key, and the label forms. The instance is the carrier's:
-- | `PUI` holds a generic `Hosting m node =>` instance over the keyed
-- | reconciler it shares with `Acting`, with passage where `Acting` gates.
module Data.Profunctor.Gathering
  ( class Gathering
  , gathering
  , gathered
  , gatheredAt
  ) where

import Prelude

import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Structural (withStructuralOrd)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), snd)
import Prim.Row (class Cons)
import Record (get) as Record
import Type.Proxy (Proxy(..))

-- | The class primitive: the keyed array in, each element's emission out
-- | beside its element's key. Carriers implement this; the vocabulary form
-- | is `gathered @l`.
class Profunctor p <= Gathering p where
  gathering :: forall k a b. Ord k => p a b -> p (Array (Tuple k a)) (Tuple k b)

-- | The gathering collection keyed by the element's identity field `l`.
gathered :: forall @l r p k b v. Gathering p => IsSymbol l => Cons l k b r => p { | r } [ | v ] -> p (Array { | r }) [ | v ]
gathered w = withStructuralOrd @k
  (dimap (map \r -> Tuple (Record.get (Proxy @l) r) r) snd (gathering w))

-- | `gathered` over an array the model stores: keyed by field `l` of each
-- | element, the array field `t` of the fed row, both named by label
-- | (`# gatheredAt @"key" @"cells"`, cells). An array derived from the row
-- | is a projection, `gatheredOf`.
gatheredAt :: forall @l @t p k b1 b2 r1 r v. Gathering p => IsSymbol l => IsSymbol t => Cons l k b1 r1 => Cons t (Array { | r1 }) b2 r => p { | r1 } [ | v ] -> p { | r } [ | v ]
gatheredAt w = lcmap (Record.get (Proxy @t)) (gathered @l w)
