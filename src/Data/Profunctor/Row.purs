module Data.Profunctor.Row
  ( class Row
  , widenRecordInput
  , narrowVariantInput
  , narrowRecordOutput
  , widenVariantOutput
  )
  where

import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord)
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant)
import Data.Profunctor.Row.VariantToRecord (class VariantToRecord)
import Data.Profunctor.Row.VariantToVariant (class VariantToVariant)
import Data.Variant (Variant, expand)
import Prim.Row (class Union) as Row
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
  p (Record narrow) o -> p (Record wider) o
widenRecordInput = lcmap unsafeCoerce

narrowVariantInput :: forall p narrow extra wider o.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p (Variant wider) o -> p (Variant narrow) o
narrowVariantInput = lcmap expand

narrowRecordOutput :: forall p i narrow extra wider.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p i (Record wider) -> p i (Record narrow)
narrowRecordOutput = rmap unsafeCoerce

widenVariantOutput :: forall p i narrow extra wider.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p i (Variant narrow) -> p i (Variant wider)
widenVariantOutput = rmap expand
