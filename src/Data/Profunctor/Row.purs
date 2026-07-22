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
-- | Two laws govern every direction (the four merges here and the container
-- | action in `Data.Profunctor.Acting` alike), both decided by the
-- | **output side**:
-- |
-- |   * **units are forced, not designed** — a direction's nullary merge
-- |     announces iff its output side is a product: `1_× = {}` (and `[]` at
-- |     runtime) is inhabited, so the unit announces its canonical value
-- |     (`pempty = announce {}`); `1_+ = Variant ()` (zero emitters) is
-- |     uninhabited, so the unit is silence, by parametricity. Every
-- |     starvation symptom is a sum-output unit standing where a
-- |     product-output unit was required.
-- |   * **gates are the cost of laxity over streams** — pairing two output
-- |     streams into one stream of pairs has one canonical implementation:
-- |     retain each side's last value, withhold until every side has spoken.
-- |     So every (·,×)-direction gates and retains (`recordToRecord`,
-- |     `variantToRecord`, `acted`'s gather) and no (·,+)-direction does
-- |     (injections need no pairing).
-- |
-- | See doc/collections-profunctor-algebra.md §1.
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
  , class LabelsDoc
  , class NoDuplicateLabels
  , class NoDuplicateLabelsK
  , class RowLabels
  , exactRow
  , fieldNames
  , rowLabels
  , widenRecordInput
  , widenVariantOutput
  )
  where

import Prelude (identity, (<<<), (<>))

import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (expand)
import Data.Variant.Internal (class VariantTags)
import Prim.Ordering (Ordering, LT, EQ, GT)
import Prim.Row (class Cons, class Lacks, class Nub, class Union) as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList (Cons, Nil) as RL
import Prim.Symbol (class Append, class Compare) as Symbol
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
  , RowLabels o1l
  , RowToList o2 o2l
  , FieldNames o2l o2 o2
  , RowLabels o2l
  ) <= MergeableRecords o1 o2 o1l o2l

instance
  ( RowToList o1 o1l
  , FieldNames o1l o1 o1
  , RowLabels o1l
  , RowToList o2 o2l
  , FieldNames o2l o2 o2
  , RowLabels o2l
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

-- | Reify a `RowList`'s labels as runtime strings — the evidence the gated
-- | merges' **starvation diagnostics** use to *name* the fields a gate is
-- | still waiting for (the compile-time sibling of `FieldNames`, which
-- | copies the fields' values).
class RowLabels :: forall k. RowList k -> Constraint
class RowLabels rl where
  rowLabels :: Proxy rl -> Array String

instance RowLabels RL.Nil where
  rowLabels _ = []

instance (IsSymbol l, RowLabels rest) => RowLabels (RL.Cons l a rest) where
  rowLabels _ = [ reflectSymbol (Proxy @l) ] <> rowLabels (Proxy @rest)

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
-- label, fails with a message that *names* it — and, so the offending
-- operand can be found at a glance, renders **both operands' full label
-- sets** into the error via `LabelsDoc`.

-- | Render a `RowList`'s labels as one type-level `Symbol` — `"a, b, c"` —
-- | for use inside `Fail` messages (the `Text`-level sibling of
-- | `RowLabels`).
class LabelsDoc :: forall k. RowList k -> Symbol -> Constraint
class LabelsDoc rl s | rl -> s

instance LabelsDoc RL.Nil ""
else instance LabelsDoc (RL.Cons l a RL.Nil) l
else instance (LabelsDoc rest s, Symbol.Append ", " s s', Symbol.Append l s' out) => LabelsDoc (RL.Cons l a rest) out

-- The walker threads the *original* two lists alongside (callers pass the
-- lists twice: `DisjointLabels l1 l2 l1 l2`), so the failure instance can
-- render both operands' complete label sets.
class DisjointLabels :: forall k1 k2. RowList k1 -> RowList k2 -> RowList k1 -> RowList k2 -> Constraint
class DisjointLabels walk l2 own other

instance DisjointLabels RL.Nil l2 own other
instance (LabelAbsent l l2 own other, DisjointLabels rest l2 own other) => DisjointLabels (RL.Cons l a rest) l2 own other

class LabelAbsent :: forall k1 k2. Symbol -> RowList k2 -> RowList k1 -> RowList k2 -> Constraint
class LabelAbsent l rl own other

instance LabelAbsent l RL.Nil own other
instance (Symbol.Compare l l' ord, LabelAbsentK ord l rest own other) => LabelAbsent l (RL.Cons l' a rest) own other

class LabelAbsentK :: forall k1 k2. Ordering -> Symbol -> RowList k2 -> RowList k1 -> RowList k2 -> Constraint
class LabelAbsentK ord l rest own other

instance
  ( LabelsDoc own ownDoc
  , LabelsDoc other otherDoc
  , Fail
      ( Above
          (Beside (Beside (Text "Two merge operands own the label \"") (Text l)) (Text "\"."))
          (Above
            (Beside (Beside (Beside (Beside (Text "One operand owns { ") (Text ownDoc)) (Text " }, the other { ")) (Text otherDoc)) (Text " }."))
            (Above
              (Text "On an owned merge side each label belongs to exactly one operand: every record-output field has ONE producer, every variant-input case has ONE handler.")
              (Text "Look for the duplicated `asField`/`field`/`forCase` label in this `do` block. (doc/type-errors.md #2)")))
      )
  ) => LabelAbsentK EQ l rest own other
instance LabelAbsent l rest own other => LabelAbsentK LT l rest own other
instance LabelAbsent l rest own other => LabelAbsentK GT l rest own other

-- The same defect can also surface *within* one operand's inferred row:
-- unification can build a single row carrying a label twice (e.g. the tail
-- of a `do` block against a pinned total row). `RowToList` sorts, so
-- duplicates are adjacent — one pass catches them; the original list rides
-- along (callers pass the list twice: `NoDuplicateLabels rl rl`) so the
-- failure names the whole row.

class NoDuplicateLabels :: forall k. RowList k -> RowList k -> Constraint
class NoDuplicateLabels walk orig

instance NoDuplicateLabels RL.Nil orig
else instance NoDuplicateLabels (RL.Cons l a RL.Nil) orig
else instance (Symbol.Compare l l' ord, NoDuplicateLabelsK ord l (RL.Cons l' b rest) orig) => NoDuplicateLabels (RL.Cons l a (RL.Cons l' b rest)) orig

class NoDuplicateLabelsK :: forall k. Ordering -> Symbol -> RowList k -> RowList k -> Constraint
class NoDuplicateLabelsK ord l rest orig

instance
  ( LabelsDoc orig origDoc
  , Fail
      ( Above
          (Beside (Beside (Text "A merge operand's row owns the label \"") (Text l)) (Text "\" twice."))
          (Above
            (Beside (Beside (Text "The row is { ") (Text origDoc)) (Text " }."))
            (Above
              (Text "On an owned merge side each label belongs to exactly one operand: every record-output field has ONE producer, every variant-input case has ONE handler.")
              (Text "Look for the duplicated `asField`/`field`/`forCase` label in this `do` block. (doc/type-errors.md #2)")))
      )
  ) => NoDuplicateLabelsK EQ l rest orig
instance NoDuplicateLabels rest orig => NoDuplicateLabelsK LT l rest orig
instance NoDuplicateLabels rest orig => NoDuplicateLabelsK GT l rest orig

-- | A merge's **variant-input side**: every case has exactly one handler
-- | (disjoint rows), and routing a value to its handler is label-driven —
-- | `DispatchableVariants` supplies the runtime tags `contract` compares.
class OwnedVariantInputs :: Row Type -> Row Type -> Row Type -> RowList Type -> RowList Type -> Constraint
class
  ( NoDuplicateLabels i1l i1l
  , NoDuplicateLabels i2l i2l
  , DisjointLabels i1l i2l i1l i2l
  , ExclusiveRows i1 i2 i
  , DispatchableVariants i1 i2 i1l i2l
  ) <= OwnedVariantInputs i1 i2 i i1l i2l

instance
  ( RowToList i1 i1l
  , RowToList i2 i2l
  , NoDuplicateLabels i1l i1l
  , NoDuplicateLabels i2l i2l
  , DisjointLabels i1l i2l i1l i2l
  , ExclusiveRows i1 i2 i
  , DispatchableVariants i1 i2 i1l i2l
  ) => OwnedVariantInputs i1 i2 i i1l i2l

-- | A merge's **record-output side**: every field has exactly one producer
-- | (disjoint rows), and combining contributions is label-driven —
-- | `MergeableRecords` supplies the runtime field names `exactRow` trims
-- | with before the gates' union.
class OwnedRecordOutputs :: Row Type -> Row Type -> Row Type -> RowList Type -> RowList Type -> Constraint
class
  ( NoDuplicateLabels o1l o1l
  , NoDuplicateLabels o2l o2l
  , DisjointLabels o1l o2l o1l o2l
  , ExclusiveRows o1 o2 o
  , MergeableRecords o1 o2 o1l o2l
  ) <= OwnedRecordOutputs o1 o2 o o1l o2l

instance
  ( RowToList o1 o1l
  , RowToList o2 o2l
  , NoDuplicateLabels o1l o1l
  , NoDuplicateLabels o2l o2l
  , DisjointLabels o1l o2l o1l o2l
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
