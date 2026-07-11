-- | The shared floor of the row layer — what every direction module
-- | (`Data.Profunctor.Row.*`) stands on:
-- |
-- |   * **row-constraint vocabulary** — `InclusiveRows` (overlapping rows,
-- |     deduped union: record inputs, variant outputs), `ExclusiveRows`
-- |     (disjoint partition: variant inputs, record outputs),
-- |     `DispatchableVariants` (runtime tag evidence for variant dispatch).
-- |     Their meanings come from the row-profunctor reading: everyone may
-- |     read a record field / offer a variant case, but each variant case
-- |     must have exactly one handler and each record field exactly one
-- |     producer.
-- |   * **reshapings** — `dimap`-only structural adapters that grow or
-- |     shrink one row-typed side, with nothing flowing through the added
-- |     or dropped labels. Everything needs only `Profunctor`; the
-- |     strengths (`Strong`/`Choice`/`Resolving`/`Retaining`) and the
-- |     merges build above.
-- |
-- | Reshaping naming: `widen` enlarges a row, `narrow` shrinks it;
-- | whole-row forms are `<direction><RowKind><Side>`, single-label forms
-- | `<direction><Side><Label>`. Each combinator is parametric in the side
-- | it doesn't touch, so each serves the two direction modules sharing
-- | that side.
-- |
-- | Reshape vs focus (doc/row-profunctors.md, "Reshape vs focus"): a
-- | reshape *drops* the complement — extra record fields are simply never
-- | read (free coercion), extra variant cases are never emitted (`expand`)
-- | — while a focus *threads* it (`Strong`/`Choice`). The two corners no
-- | reshape can reach need more than `dimap`:
-- |   * widening a Record **output** (needs defaults for the extra fields —
-- |     see `withRecordOutputDefault`)
-- |   * narrowing a Variant **output** (needs a fallback for discarded cases)
module Data.Profunctor.Row
  ( class InclusiveRows
  , class ExclusiveRows
  , class DispatchableVariants
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
import Data.Variant (expand)
import Data.Variant.Internal (class VariantTags)
import Prim.Row (class Cons, class Nub, class Union) as Row
import Prim.RowList (class RowToList, RowList)
import Unsafe.Coerce (unsafeCoerce)

-- =====================================================================
-- Row-constraint vocabulary
-- =====================================================================

-- r1 and r2 may overlap; r is their deduped union; both r1 ⊆ r and r2 ⊆ r.
-- Witness rows: r12 = r1 ∪ r2 (pre-nub), r1x = r ∖ r1, r2x = r ∖ r2.
class InclusiveRows :: forall k. Row k -> Row k -> Row k -> Row k -> Row k -> Row k -> Constraint
class
  ( Row.Union r1 r2 r12
  , Row.Nub r12 r
  , Row.Union r1 r1x r
  , Row.Union r2 r2x r
  ) <= InclusiveRows r1 r2 r r12 r1x r2x

instance
  ( Row.Union r1 r2 r12
  , Row.Nub r12 r
  , Row.Union r1 r1x r
  , Row.Union r2 r2x r
  ) => InclusiveRows r1 r2 r r12 r1x r2x

-- r1 and r2 are disjoint; their union is r.
class ExclusiveRows :: forall k. Row k -> Row k -> Row k -> Constraint
class
  ( Row.Union r1 r2 r
  , Row.Union r2 r1 r
  ) <= ExclusiveRows r1 r2 r

instance
  ( Row.Union r1 r2 r
  , Row.Union r2 r1 r
  ) => ExclusiveRows r1 r2 r

-- Variants r1 and r2 carry runtime tag info for dispatch.
-- Witness lists: r1l = RowToList r1, r2l = RowToList r2.
class DispatchableVariants :: forall k1 k2. Row k1 -> Row k2 -> RowList k1 -> RowList k2 -> Constraint
class
  ( RowToList r1 r1l
  , VariantTags r1l
  , RowToList r2 r2l
  , VariantTags r2l
  ) <= DispatchableVariants r1 r2 r1l r2l

instance
  ( RowToList r1 r1l
  , VariantTags r1l
  , RowToList r2 r2l
  , VariantTags r2l
  ) => DispatchableVariants r1 r2 r1l r2l

-- =====================================================================
-- Whole-row reshapings
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
-- these take no operand — nothing flows through the field/case.
--
-- `Cons l f () lf` pins `lf` to the singleton row `(l :: f)`; `Union b lf s`
-- then says the shot `s` = background `b` plus that one focus field/case (and
-- gives `expand` the `Union` it needs — `Cons` alone wouldn't). The untouched
-- opposite side is the reality `r`.
-- =====================================================================

widenInputProperty :: forall @l p f lf b s r.
  Profunctor p =>
  Row.Cons l f () lf =>
  Row.Union b lf s =>
  p { | b } r -> p { | s } r
widenInputProperty = lcmap unsafeCoerce

widenOutputCase :: forall @l p f lf b s r.
  Profunctor p =>
  Row.Cons l f () lf =>
  Row.Union b lf s =>
  p r [ | b ] -> p r [ | s ]
widenOutputCase = rmap expand

narrowInputCase :: forall @l p f lf b s r.
  Profunctor p =>
  Row.Cons l f () lf =>
  Row.Union b lf s =>
  p [ | s ] r -> p [ | b ] r
narrowInputCase = lcmap expand

narrowOutputProperty :: forall @l p f lf b s r.
  Profunctor p =>
  Row.Cons l f () lf =>
  Row.Union b lf s =>
  p r { | s } -> p r { | b }
narrowOutputProperty = rmap unsafeCoerce
