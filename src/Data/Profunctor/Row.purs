module Data.Profunctor.Row
  ( class Row
  , widenRecordInput
  , narrowVariantInput
  , narrowRecordOutput
  , widenVariantOutput
  , widenInputProperty
  , widenOutputCase
  , narrowInputCase
  , narrowOutputProperty
  )
  where

import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord)
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant)
import Data.Profunctor.Row.VariantToRecord (class VariantToRecord)
import Data.Profunctor.Row.VariantToVariant (class VariantToVariant)
import Data.Variant (expand)
import Prim.Row (class Cons, class Union) as Row
import Unsafe.Coerce (unsafeCoerce)

class (RecordToRecord p, RecordToVariant p, VariantToRecord p, VariantToVariant p) <= Row p

-- =====================================================================
-- Primitive value-level row reshapings.
--
-- Exported so the binary `recordToRecord` / `recordToVariant` /
-- `variantToRecord` / `variantToVariant` instances in `UI.purs` can
-- reuse the unary combinators defined below.
-- =====================================================================

-- =====================================================================
-- Unary row-to-row transformations, derivable from `dimap` alone.
--
-- Naming: `<direction><RowKind><Side>`. `widen` enlarges a row,
-- `narrow` shrinks it. Each combinator is parametric in the side it
-- doesn't touch, so each covers two of the four R/V × R/V shapes.
--
-- The transformations NOT provided here cannot be derived from
-- `dimap` alone:
--   * widening Record output (needs defaults for the extra fields)
--   * narrowing Variant output (needs a fallback for discarded cases)
-- =====================================================================

widenRecordInput :: forall p narrow extra wider o.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p { | narrow } o -> p { | wider } o
widenRecordInput = lcmap unsafeCoerce

narrowVariantInput :: forall p narrow extra wider o.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p [ | wider ] o -> p [ | narrow ] o
narrowVariantInput = lcmap expand

narrowRecordOutput :: forall p i narrow extra wider.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p i { | wider } -> p i { | narrow }
narrowRecordOutput = rmap unsafeCoerce

widenVariantOutput :: forall p i narrow extra wider.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p i [ | narrow ] -> p i [ | wider ]
widenVariantOutput = rmap expand

-- =====================================================================
-- Single-field / single-case specializations of the four reshapings
-- above (the `extra` row pinned to one labeled field/case via `Cons`).
-- Same bodies; each adds or drops exactly ONE phantom field/case.
--
-- Unlike `recordToProperty`/`eliminateProperty` (which wrap `first`/
-- `second` and take a value-operand that produces/consumes the field),
-- these take no operand — nothing flows through the field/case; they are
-- pure structural adapters. They are the single-field reshapings the
-- *mixed* families lean on, where the diagonal focus combinators can't reach:
--   * Record→Variant (widen/widen): `widenInputProperty` + `widenOutputCase`
--   * Variant→Record (narrow/narrow): `narrowInputCase` + `narrowOutputProperty`
-- (each is side-specific, so it also applies to the matching diagonal family).
-- =====================================================================

-- `Cons l a () one` pins `one` to the singleton row `(l :: a)`; `Union narrow
-- one wider` then says `wider = narrow` plus that one field/case (and gives
-- `expand` the `Union` it needs — `Cons` alone wouldn't).

widenInputProperty :: forall @l p a one narrow wider o.
  Profunctor p =>
  Row.Cons l a () one =>
  Row.Union narrow one wider =>
  p { | narrow } o -> p { | wider } o
widenInputProperty = lcmap unsafeCoerce

widenOutputCase :: forall @l p a one narrow wider i.
  Profunctor p =>
  Row.Cons l a () one =>
  Row.Union narrow one wider =>
  p i [ | narrow ] -> p i [ | wider ]
widenOutputCase = rmap expand

narrowInputCase :: forall @l p a one narrow wider o.
  Profunctor p =>
  Row.Cons l a () one =>
  Row.Union narrow one wider =>
  p [ | wider ] o -> p [ | narrow ] o
narrowInputCase = lcmap expand

narrowOutputProperty :: forall @l p a one narrow wider i.
  Profunctor p =>
  Row.Cons l a () one =>
  Row.Union narrow one wider =>
  p i { | wider } -> p i { | narrow }
narrowOutputProperty = rmap unsafeCoerce
