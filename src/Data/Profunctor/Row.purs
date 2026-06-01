module Data.Profunctor.Row
  ( class Row
  , widenRecordInput
  , narrowVariantInput
  , narrowRecordOutput
  , widenVariantOutput
  , widenRecordToVariant
  , narrowVariantToRecord
  )
  where

import Data.Profunctor (class Profunctor, dimap, lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord, pickRecord)
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant)
import Data.Profunctor.Row.VariantToRecord (class VariantToRecord)
import Data.Profunctor.Row.VariantToVariant (class VariantToVariant)
import Data.Variant (Variant, expand)
import Prim.Row (class Union) as Row

class (RecordToRecord p, RecordToVariant p, VariantToRecord p, VariantToVariant p) <= Row p

-- =====================================================================
-- Primitive value-level row reshapings.
--
-- Exported so the binary `recordToRecord` / `recordToVariant` /
-- `variantToRecord` / `variantToVariant` instances in `UI.purs` route
-- through the same helpers as the unary combinators below.
-- =====================================================================

-- `pickRecord` (record sub-projection) is imported from
-- `Data.Profunctor.Row.RecordToRecord` and used by the reshapings below.
-- It is defined there — that module has no dependency on this umbrella
-- module, so it is the cycle-free home for the shared helper.

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
widenRecordInput = lcmap pickRecord

narrowVariantInput :: forall p narrow extra wider o.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p (Variant wider) o -> p (Variant narrow) o
narrowVariantInput = lcmap expand

narrowRecordOutput :: forall p i narrow extra wider.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p i (Record wider) -> p i (Record narrow)
narrowRecordOutput = rmap pickRecord

widenVariantOutput :: forall p i narrow extra wider.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p i (Variant narrow) -> p i (Variant wider)
widenVariantOutput = rmap expand

-- ---------------------------------------------------------------------
-- Both-sides reshapings for the two mixed shapes.
--
-- These touch *both* sides at once (a single `dimap`), and the free
-- direction is forced by variance: `Record → Variant` sits on the
-- widen/widen side, `Variant → Record` on the narrow/narrow side.
--
-- They are *reshapes*, not *focuses* — two orthogonal axes:
--   * direction  — widen (grow) vs narrow (shrink)
--   * complement — a reshape drops the complement (pure `dimap`,
--                  `Profunctor`-only); a focus (`focusRecord`/
--                  `focusVariant`) threads it across, needing strength.
-- `focusRecord` is itself a widen that *also* threads the complement, so
-- the contrast with `widenRecordToVariant` is the complement axis, not
-- direction. The through-threading focus does not exist for mixed kinds:
-- `Strong` applies its argument unconditionally, `Choice` only on a gated
-- input branch, so a complement can be carried only when input and output
-- share that conditionality (the diagonals). Crossing it forces a
-- conversion that costs defaults (fill the product a sum left empty,
-- `Variant → Record`) or fallback (collapse a product into the sum's one
-- slot, `Record → Variant`) — the irreducible binary merge instead.
-- See doc/row-profunctors.md, "The break, sharpened: unconditional vs gated".
-- ---------------------------------------------------------------------

-- Widen both sides at once: `sub → s` on input, `subO → t` on output.
-- = `widenVariantOutput ∘ widenRecordInput`.
widenRecordToVariant :: forall p sub rest s subO restOut t.
  Profunctor p =>
  Row.Union sub rest s =>
  Row.Union subO restOut t =>
  p (Record sub) (Variant subO) -> p (Record s) (Variant t)
widenRecordToVariant = dimap pickRecord expand

-- Narrow both sides at once: `s → sub` on input, `t → subO` on output —
-- the categorical dual of `widenRecordToVariant` (arrows reversed swaps
-- the maps and flips the direction). = `narrowVariantInput ∘ narrowRecordOutput`.
narrowVariantToRecord :: forall p sub rest s subO restOut t.
  Profunctor p =>
  Row.Union sub rest s =>
  Row.Union subO restOut t =>
  p (Variant s) (Record t) -> p (Variant sub) (Record subO)
narrowVariantToRecord = dimap expand pickRecord
