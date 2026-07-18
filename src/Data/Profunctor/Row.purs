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
-- |     producer. `MergeableRecords` adds the **runtime-exactness** evidence the
-- |     gated merges use to trim operand emissions to their declared
-- |     output rows (`exactRow`).
-- |   * **reshapings** — `dimap`-only structural adapters that grow or
-- |     shrink one row-typed side, with nothing flowing through the added
-- |     or dropped labels.
-- |
-- | Everything needs only `Profunctor`; the strengths
-- | (`Strong`/`Choice`/`Resolving`/`Retaining`) and the merges build above.
-- |
-- | Reshape vs focus (doc/row-profunctors.md, "Reshape vs focus"): a
-- | reshape *drops* the complement — extra record fields are simply never
-- | read (free coercion), extra variant cases are never emitted (`expand`)
-- | — while a focus *threads* it (`Strong`/`Choice`).
module Data.Profunctor.Row
  ( class InclusiveRows
  , class ExclusiveRows
  , class DispatchableVariants
  , class MergeableRecords
  , class FieldNames
  , class SharedRecordInputs
  , class SharedVariantOutputs
  , class OwnedVariantInputs
  , class OwnedRecordOutputs
  , class DisjointLabels
  , class LabelAbsent
  , class LabelAbsentK
  , class NoDuplicateLabels
  , class NoDuplicateLabelsK
  , exactRow
  , fieldNames
  , widenRecordInput
  , widenVariantOutput
  )
  where

import Prelude (identity, (<<<))

import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Symbol (class IsSymbol)
import Data.Variant (expand)
import Data.Variant.Internal (class VariantTags)
import Prim.Ordering (Ordering, LT, EQ, GT)
import Prim.Row (class Cons, class Lacks, class Nub, class Union) as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList (Cons, Nil) as RL
import Prim.Symbol (class Compare) as Symbol
import Prim.TypeError (class Fail, Above, Beside, Text)
import Record (get) as Record
import Record.Builder (Builder)
import Record.Builder (buildFromScratch, insert) as Builder
import Type.Proxy (Proxy(..))
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
-- Runtime exactness
-- =====================================================================

-- | Rebuild a record field-by-field so its **runtime** shape is exactly its
-- | row — no more, no less. A record's type never guarantees its runtime
-- | object carries only the declared labels: the widening reshapings above
-- | are coercions, so a widget that echoes or lens-rebuilds its input emits
-- | an object runtime-carrying every field of the *merged* row while typed
-- | at its own narrow slice. The gated merges use `exactRow` to trim each
-- | operand's emission to its declared output row before the left-biased
-- | `Record.union`, so stale runtime copies of *sibling* fields can never
-- | shadow the siblings' genuine contributions.
exactRow :: forall r rl. RowToList r rl => FieldNames rl r r => { | r } -> { | r }
exactRow r = Builder.buildFromScratch (fieldNames (Proxy @rl) r)

-- | Rows o1 and o2 carry runtime rebuild evidence for the gated merges'
-- | exactness trim (`exactRow`). Witness lists: o1l = RowToList o1,
-- | o2l = RowToList o2 — the `DispatchableVariants` pattern, so the merge
-- | instances can discharge `exactRow`'s constraints from the givens'
-- | superclasses.
class MergeableRecords :: Row Type -> Row Type -> RowList Type -> RowList Type -> Constraint
class
  ( RowToList o1 o1l
  , FieldNames o1l o1 o1
  , RowToList o2 o2l
  , FieldNames o2l o2 o2
  ) <= MergeableRecords o1 o2 o1l o2l

instance
  ( RowToList o1 o1l
  , FieldNames o1l o1 o1
  , RowToList o2 o2l
  , FieldNames o2l o2 o2
  ) => MergeableRecords o1 o2 o1l o2l

-- | `RowList`-indexed worker for `exactRow`: copies exactly the listed
-- | labels out of `from` into a freshly built record.
class FieldNames :: RowList Type -> Row Type -> Row Type -> Constraint
class FieldNames rl from to | rl -> to where
  fieldNames :: Proxy rl -> { | from } -> Builder {} { | to }

instance FieldNames RL.Nil from () where
  fieldNames _ _ = identity

instance
  ( IsSymbol l
  , Row.Cons l a fromRest from
  , Row.Cons l a toRest to
  , Row.Lacks l toRest
  , FieldNames rl from toRest
  ) => FieldNames (RL.Cons l a rl) from to where
  fieldNames _ r = Builder.insert (Proxy @l) (Record.get (Proxy @l) r) <<< fieldNames (Proxy @rl) r

-- =====================================================================
-- Side vocabulary: one constraint per merge side
-- =====================================================================
--
-- The four merges' constraints factor exactly by side, under one law:
-- **sharing is inclusive, responsibility is exclusive** — and runtime
-- label evidence appears only on the exclusive sides, where the merge's
-- runtime action is label-driven (dispatch, union) rather than
-- label-blind (broadcast, expand). Records are read-shared but
-- write-owned; variants are emit-shared but handle-owned. Each merge
-- signature is then two words, one per side:
--
--   recordToRecord   : SharedRecordInputs  + OwnedRecordOutputs
--   recordToVariant  : SharedRecordInputs  + SharedVariantOutputs
--   variantToVariant : OwnedVariantInputs  + SharedVariantOutputs
--   variantToRecord  : OwnedVariantInputs  + OwnedRecordOutputs

-- | A merge's **record-input side**: everyone may read a field, so operand
-- | rows may overlap. The merge action is a label-blind broadcast — no
-- | runtime evidence needed.
class SharedRecordInputs :: Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class InclusiveRows i1 i2 i i12 i1x i2x <= SharedRecordInputs i1 i2 i i12 i1x i2x

instance InclusiveRows i1 i2 i i12 i1x i2x => SharedRecordInputs i1 i2 i i12 i1x i2x

-- | A merge's **variant-output side**: anyone may emit a case, so operand
-- | rows may overlap. The merge action is a label-blind `expand` — no
-- | runtime evidence needed.
class SharedVariantOutputs :: Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class InclusiveRows o1 o2 o o12 o1x o2x <= SharedVariantOutputs o1 o2 o o12 o1x o2x

instance InclusiveRows o1 o2 o o12 o1x o2x => SharedVariantOutputs o1 o2 o o12 o1x o2x

-- =====================================================================
-- Custom diagnostics
-- =====================================================================
--
-- A duplicated label on an owned merge side otherwise dies deep inside the
-- exactness evidence as an anonymous `Lacks` failure (doc/type-errors.md
-- #2). This detector walks both label lists and, on the first shared
-- label, fails with a message that *names* it.

class DisjointLabels :: forall k1 k2. RowList k1 -> RowList k2 -> Constraint
class DisjointLabels l1 l2

instance DisjointLabels RL.Nil l2
instance (LabelAbsent l l2, DisjointLabels rest l2) => DisjointLabels (RL.Cons l a rest) l2

class LabelAbsent :: forall k. Symbol -> RowList k -> Constraint
class LabelAbsent l rl

instance LabelAbsent l RL.Nil
instance (Symbol.Compare l l' ord, LabelAbsentK ord l rest) => LabelAbsent l (RL.Cons l' a rest)

class LabelAbsentK :: forall k. Ordering -> Symbol -> RowList k -> Constraint
class LabelAbsentK ord l rest

instance Fail
  ( Above
      (Beside (Beside (Text "Two merge operands own the label \"") (Text l)) (Text "\"."))
      (Above
        (Text "On an owned merge side each label belongs to exactly one operand: every record-output field has ONE producer, every variant-input case has ONE handler.")
        (Text "Look for the duplicated `asField`/`field`/`forCase` label in this `do` block. (doc/type-errors.md #2)"))
  ) => LabelAbsentK EQ l rest
instance LabelAbsent l rest => LabelAbsentK LT l rest
instance LabelAbsent l rest => LabelAbsentK GT l rest

-- The same defect can also surface *within* one operand's inferred row:
-- unification can build a single row carrying a label twice (e.g. the tail
-- of a `do` block against a pinned total row). `RowToList` sorts, so
-- duplicates are adjacent — one pass catches them.

class NoDuplicateLabels :: forall k. RowList k -> Constraint
class NoDuplicateLabels rl

instance NoDuplicateLabels RL.Nil
instance NoDuplicateLabels (RL.Cons l a RL.Nil)
instance (Symbol.Compare l l' ord, NoDuplicateLabelsK ord l (RL.Cons l' b rest)) => NoDuplicateLabels (RL.Cons l a (RL.Cons l' b rest))

class NoDuplicateLabelsK :: forall k. Ordering -> Symbol -> RowList k -> Constraint
class NoDuplicateLabelsK ord l rest

instance Fail
  ( Above
      (Beside (Beside (Text "Two merge operands own the label \"") (Text l)) (Text "\"."))
      (Above
        (Text "On an owned merge side each label belongs to exactly one operand: every record-output field has ONE producer, every variant-input case has ONE handler.")
        (Text "Look for the duplicated `asField`/`field`/`forCase` label in this `do` block. (doc/type-errors.md #2)"))
  ) => NoDuplicateLabelsK EQ l rest
instance NoDuplicateLabels rest => NoDuplicateLabelsK LT l rest
instance NoDuplicateLabels rest => NoDuplicateLabelsK GT l rest

-- | A merge's **variant-input side**: every case has exactly one handler
-- | (disjoint rows), and routing a value to its handler is label-driven —
-- | `DispatchableVariants` supplies the runtime tags `contract` compares.
class OwnedVariantInputs :: Row Type -> Row Type -> Row Type -> RowList Type -> RowList Type -> Constraint
class
  ( NoDuplicateLabels i1l
  , NoDuplicateLabels i2l
  , DisjointLabels i1l i2l
  , ExclusiveRows i1 i2 i
  , DispatchableVariants i1 i2 i1l i2l
  ) <= OwnedVariantInputs i1 i2 i i1l i2l

instance
  ( RowToList i1 i1l
  , RowToList i2 i2l
  , NoDuplicateLabels i1l
  , NoDuplicateLabels i2l
  , DisjointLabels i1l i2l
  , ExclusiveRows i1 i2 i
  , DispatchableVariants i1 i2 i1l i2l
  ) => OwnedVariantInputs i1 i2 i i1l i2l

-- | A merge's **record-output side**: every field has exactly one producer
-- | (disjoint rows), and combining contributions is label-driven —
-- | `MergeableRecords` supplies the runtime field names `exactRow` trims
-- | with before the gates' union.
class OwnedRecordOutputs :: Row Type -> Row Type -> Row Type -> RowList Type -> RowList Type -> Constraint
class
  ( NoDuplicateLabels o1l
  , NoDuplicateLabels o2l
  , DisjointLabels o1l o2l
  , ExclusiveRows o1 o2 o
  , MergeableRecords o1 o2 o1l o2l
  ) <= OwnedRecordOutputs o1 o2 o o1l o2l

instance
  ( RowToList o1 o1l
  , RowToList o2 o2l
  , NoDuplicateLabels o1l
  , NoDuplicateLabels o2l
  , DisjointLabels o1l o2l
  , ExclusiveRows o1 o2 o
  , MergeableRecords o1 o2 o1l o2l
  ) => OwnedRecordOutputs o1 o2 o o1l o2l

-- =====================================================================
-- Whole-row reshapings
-- =====================================================================

widenRecordInput :: forall p narrow extra wider o.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p { | narrow } o -> p { | wider } o
widenRecordInput = lcmap unsafeCoerce

widenVariantOutput :: forall p i narrow extra wider.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p i [ | narrow ] -> p i [ | wider ]
widenVariantOutput = rmap expand
