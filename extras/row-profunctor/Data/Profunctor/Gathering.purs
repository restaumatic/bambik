-- | The **gathering collection**: the `×→+` member of the keyed square,
-- | `Resolving`'s analogue over a runtime key — the array fed whole, every
-- | element's emission leaving on the one shared channel as it happens,
-- | ungated. It shares `Acting`'s keyed, retaining reconciliation (survivors
-- | re-fed in place, entrants built, leavers removed), so a clickable row
-- | or a cell reporting its key keeps its node across feeds; what differs
-- | is the output side, a variant, which no gate can wait on. **The key is
-- | a materialized identity field of the row** (`gathered @l`), as for
-- | `acted @l`.
-- |
-- | Laws:
-- |
-- |   * **empty** — fed `[]`, nothing: there is no event to fabricate.
-- |   * **singleton** — fed `[a]`, behaves as the element fed `a`.
-- |   * **passage** — each element emission is forwarded at once, untagged
-- |     and ungated; nothing is retained for the channel.
-- |   * **arming** — a feed fires nothing: the `×→+` Answer law. It is the
-- |     elements' to keep, so the class is lawful over operands that arm
-- |     (emitters; `editsOf` makes an editor one by dropping its answers
-- |     inside the feed) — as `Resolving` is lawful only at `PUI`'s
-- |     quiescence. There is no wire law: `gatheredBy k identity` would
-- |     forward each element's echo inside the feed.
-- |   * **monotonicity** — `p ⊑ p'` implies `gatheredBy k p ⊑ gatheredBy k p'`.
-- |
-- | No `(->)` instance: a function returns one value and cannot forward
-- | many elements' emissions; `Cont r` has one under `Monoid r`, every
-- | element's run combined, as its `RecordToVariant` instance is. Three
-- | forms: `gathered @l` over the element's row; `gatheredOf @l proj`
-- | (`PUI`, a hole-aware projection to the array: `tile @"Key entered"
-- | @"key" … # gatheredOf @"key" keyPad`); `gatheredAt @l @t` over an
-- | array the model stores at field `t` (cells). MDC2/MDC3's `listOf` is
-- | `gatheredOf` over picked rows.
module Data.Profunctor.Gathering
  ( class Gathering
  , gatheredBy
  , gathered
  , gatheredAt
  ) where

import Prelude

import Data.Profunctor (class Profunctor, lcmap)
import Data.Profunctor.Row.Structural (withStructuralOrd)
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons)
import Record (get) as Record
import Type.Proxy (Proxy(..))

-- | The class primitive: the array in, each element's emission out, keyed
-- | by a function. Carriers implement this; the vocabulary form is
-- | `gathered @l`.
class Profunctor p <= Gathering p where
  gatheredBy :: forall k a b. Ord k => (a -> k) -> p a b -> p (Array a) b

-- | The gathering collection keyed by the element's identity field `l`.
gathered :: forall @l r p k b v. Gathering p => IsSymbol l => Cons l k b r => p { | r } [ | v ] -> p (Array { | r }) [ | v ]
gathered w = withStructuralOrd @k (gatheredBy (Record.get (Proxy @l)) w)

-- | `gathered` over an array the model stores: keyed by field `l` of each
-- | element, the array field `t` of the fed row, both named by label
-- | (`# gatheredAt @"key" @"cells"`, cells). An array derived from the row
-- | is a projection, `gatheredOf`.
gatheredAt :: forall @l @t p k b1 b2 r1 r v. Gathering p => IsSymbol l => IsSymbol t => Cons l k b1 r1 => Cons t (Array { | r1 }) b2 r => p { | r1 } [ | v ] -> p { | r } [ | v ]
gatheredAt w = lcmap (Record.get (Proxy @t)) (gathered @l w)
