## Module Data.Profunctor.Row

The shared floor of the row layer — what every direction module
(`Data.Profunctor.Row.*`) stands on:

  * **row-constraint vocabulary** — `InclusiveRows` (overlapping rows,
    deduped union: record inputs, variant outputs), `ExclusiveRows`
    (disjoint partition: variant inputs, record outputs),
    `DispatchableVariants` (runtime tag evidence for variant dispatch).
    Their meanings come from the row-profunctor reading: everyone may
    read a record field / offer a variant case, but each variant case
    must have exactly one handler and each record field exactly one
    producer. `MergeableRecords` adds the **runtime-exactness** evidence the
    gated merges use to trim operand emissions to their declared
    output rows (`exactRow`).
  * **reshapings** — `dimap`-only structural adapters that grow or
    shrink one row-typed side, with nothing flowing through the added
    or dropped labels.

Everything needs only `Profunctor`; the strengths
(`Strong`/`Choice`/`Resolving`/`Retaining`) and the merges build above.

Reshape vs focus (doc/row-profunctors.md, "Reshape vs focus"): a
reshape *drops* the complement — extra record fields are simply never
read (free coercion), extra variant cases are never emitted (`expand`)
— while a focus *threads* it (`Strong`/`Choice`).

#### `InclusiveRows`

``` purescript
class (Union r1 r2 r12, Nub r12 r, Union r1 r1x r, Union r2 r2x r) <= InclusiveRows r1 r2 r r12 r1x r2x 
```

##### Instances
``` purescript
(Union r1 r2 r12, Nub r12 r, Union r1 r1x r, Union r2 r2x r) => InclusiveRows r1 r2 r r12 r1x r2x
```

#### `ExclusiveRows`

``` purescript
class (Union r1 r2 r, Union r2 r1 r) <= ExclusiveRows r1 r2 r 
```

##### Instances
``` purescript
(Union r1 r2 r, Union r2 r1 r) => ExclusiveRows r1 r2 r
```

#### `DispatchableVariants`

``` purescript
class (RowToList r1 r1l, VariantTags r1l, RowToList r2 r2l, VariantTags r2l) <= DispatchableVariants r1 r2 r1l r2l 
```

##### Instances
``` purescript
(RowToList r1 r1l, VariantTags r1l, RowToList r2 r2l, VariantTags r2l) => DispatchableVariants r1 r2 r1l r2l
```

#### `MergeableRecords`

``` purescript
class (RowToList o1 o1l, FieldNames o1l o1 o1, RowLabels o1l, RowToList o2 o2l, FieldNames o2l o2 o2, RowLabels o2l) <= MergeableRecords o1 o2 o1l o2l 
```

Rows o1 and o2 carry runtime rebuild evidence for the gated merges'
exactness trim (`exactRow`). Witness lists: o1l = RowToList o1,
o2l = RowToList o2 — the `DispatchableVariants` pattern, so the merge
instances can discharge `exactRow`'s constraints from the givens'
superclasses.

##### Instances
``` purescript
(RowToList o1 o1l, FieldNames o1l o1 o1, RowLabels o1l, RowToList o2 o2l, FieldNames o2l o2 o2, RowLabels o2l) => MergeableRecords o1 o2 o1l o2l
```

#### `FieldNames`

``` purescript
class FieldNames rl from to | rl -> to where
  fieldNames :: Proxy rl -> Record from -> Builder (Record ()) (Record to)
```

`RowList`-indexed worker for `exactRow`: copies exactly the listed
labels out of `from` into a freshly built record.

##### Instances
``` purescript
FieldNames Nil from ()
(IsSymbol l, Cons l a fromRest from, Cons l a toRest to, Lacks l toRest, FieldNames rl from toRest) => FieldNames (Cons l a rl) from to
```

#### `SharedRecordInputs`

``` purescript
class (InclusiveRows i1 i2 i i12 i1x i2x) <= SharedRecordInputs i1 i2 i i12 i1x i2x 
```

A merge's **record-input side**: everyone may read a field, so operand
rows may overlap. The merge action is a label-blind broadcast — no
runtime evidence needed.

##### Instances
``` purescript
(InclusiveRows i1 i2 i i12 i1x i2x) => SharedRecordInputs i1 i2 i i12 i1x i2x
```

#### `SharedVariantOutputs`

``` purescript
class (InclusiveRows o1 o2 o o12 o1x o2x) <= SharedVariantOutputs o1 o2 o o12 o1x o2x 
```

A merge's **variant-output side**: anyone may emit a case, so operand
rows may overlap. The merge action is a label-blind `expand` — no
runtime evidence needed.

##### Instances
``` purescript
(InclusiveRows o1 o2 o o12 o1x o2x) => SharedVariantOutputs o1 o2 o o12 o1x o2x
```

#### `OwnedVariantInputs`

``` purescript
class (NoDuplicateLabels i1l i1l, NoDuplicateLabels i2l i2l, DisjointLabels i1l i2l i1l i2l, ExclusiveRows i1 i2 i, DispatchableVariants i1 i2 i1l i2l) <= OwnedVariantInputs i1 i2 i i1l i2l 
```

A merge's **variant-input side**: every case has exactly one handler
(disjoint rows), and routing a value to its handler is label-driven —
`DispatchableVariants` supplies the runtime tags `contract` compares.

##### Instances
``` purescript
(RowToList i1 i1l, RowToList i2 i2l, NoDuplicateLabels i1l i1l, NoDuplicateLabels i2l i2l, DisjointLabels i1l i2l i1l i2l, ExclusiveRows i1 i2 i, DispatchableVariants i1 i2 i1l i2l) => OwnedVariantInputs i1 i2 i i1l i2l
```

#### `OwnedRecordOutputs`

``` purescript
class (NoDuplicateLabels o1l o1l, NoDuplicateLabels o2l o2l, DisjointLabels o1l o2l o1l o2l, ExclusiveRows o1 o2 o, MergeableRecords o1 o2 o1l o2l) <= OwnedRecordOutputs o1 o2 o o1l o2l 
```

A merge's **record-output side**: every field has exactly one producer
(disjoint rows), and combining contributions is label-driven —
`MergeableRecords` supplies the runtime field names `exactRow` trims
with before the gates' union.

##### Instances
``` purescript
(RowToList o1 o1l, RowToList o2 o2l, NoDuplicateLabels o1l o1l, NoDuplicateLabels o2l o2l, DisjointLabels o1l o2l o1l o2l, ExclusiveRows o1 o2 o, MergeableRecords o1 o2 o1l o2l) => OwnedRecordOutputs o1 o2 o o1l o2l
```

#### `DisjointLabels`

``` purescript
class DisjointLabels walk l2 own other 
```

##### Instances
``` purescript
DisjointLabels Nil l2 own other
(LabelAbsent l l2 own other, DisjointLabels rest l2 own other) => DisjointLabels (Cons l a rest) l2 own other
```

#### `LabelAbsent`

``` purescript
class LabelAbsent l rl own other 
```

##### Instances
``` purescript
LabelAbsent l Nil own other
(Compare l l' ord, LabelAbsentK ord l rest own other) => LabelAbsent l (Cons l' a rest) own other
```

#### `LabelAbsentK`

``` purescript
class LabelAbsentK ord l rest own other 
```

##### Instances
``` purescript
(LabelsDoc own ownDoc, LabelsDoc other otherDoc, Fail (Above (Beside (Beside (Text "Two merge operands own the label \"") (Text l)) (Text "\".")) (Above (Beside (Beside (Beside (Beside (Text "One operand owns { ") (Text ownDoc)) (Text " }, the other { ")) (Text otherDoc)) (Text " }.")) (Above (Text "On an owned merge side each label belongs to exactly one operand: every record-output field has ONE producer, every variant-input case has ONE handler.") (Text "Look for the duplicated `asField`/`field`/`forCase` label in this `do` block. (doc/type-errors.md #2)"))))) => LabelAbsentK EQ l rest own other
(LabelAbsent l rest own other) => LabelAbsentK LT l rest own other
(LabelAbsent l rest own other) => LabelAbsentK GT l rest own other
```

#### `LabelsDoc`

``` purescript
class LabelsDoc rl s | rl -> s
```

Render a `RowList`'s labels as one type-level `Symbol` — `"a, b, c"` —
for use inside `Fail` messages (the `Text`-level sibling of
`RowLabels`).

##### Instances
``` purescript
LabelsDoc Nil ""
LabelsDoc (Cons l a Nil) l
(LabelsDoc rest s, Append ", " s s', Append l s' out) => LabelsDoc (Cons l a rest) out
```

#### `NoDuplicateLabels`

``` purescript
class NoDuplicateLabels walk orig 
```

##### Instances
``` purescript
NoDuplicateLabels Nil orig
NoDuplicateLabels (Cons l a Nil) orig
(Compare l l' ord, NoDuplicateLabelsK ord l (Cons l' b rest) orig) => NoDuplicateLabels (Cons l a (Cons l' b rest)) orig
```

#### `NoDuplicateLabelsK`

``` purescript
class NoDuplicateLabelsK ord l rest orig 
```

##### Instances
``` purescript
(LabelsDoc orig origDoc, Fail (Above (Beside (Beside (Text "A merge operand\'s row owns the label \"") (Text l)) (Text "\" twice.")) (Above (Beside (Beside (Text "The row is { ") (Text origDoc)) (Text " }.")) (Above (Text "On an owned merge side each label belongs to exactly one operand: every record-output field has ONE producer, every variant-input case has ONE handler.") (Text "Look for the duplicated `asField`/`field`/`forCase` label in this `do` block. (doc/type-errors.md #2)"))))) => NoDuplicateLabelsK EQ l rest orig
(NoDuplicateLabels rest orig) => NoDuplicateLabelsK LT l rest orig
(NoDuplicateLabels rest orig) => NoDuplicateLabelsK GT l rest orig
```

#### `RowLabels`

``` purescript
class RowLabels rl  where
  rowLabels :: Proxy rl -> Array String
```

Reify a `RowList`'s labels as runtime strings — the evidence the gated
merges' **starvation diagnostics** use to *name* the fields a gate is
still waiting for (the compile-time sibling of `FieldNames`, which
copies the fields' values).

##### Instances
``` purescript
RowLabels Nil
(IsSymbol l, RowLabels rest) => RowLabels (Cons l a rest)
```

#### `exactRow`

``` purescript
exactRow :: forall r rl. RowToList r rl => FieldNames rl r r => Record r -> Record r
```

Rebuild a record field-by-field so its **runtime** shape is exactly its
row — no more, no less. A record's type never guarantees its runtime
object carries only the declared labels: the widening reshapings above
are coercions, so a widget that echoes or lens-rebuilds its input emits
an object runtime-carrying every field of the *merged* row while typed
at its own narrow slice. The gated merges use `exactRow` to trim each
operand's emission to its declared output row before the left-biased
`Record.union`, so stale runtime copies of *sibling* fields can never
shadow the siblings' genuine contributions.

#### `widenRecordInput`

``` purescript
widenRecordInput :: forall p narrow extra wider o. Profunctor p => Union narrow extra wider => p (Record narrow) o -> p (Record wider) o
```

#### `widenVariantOutput`

``` purescript
widenVariantOutput :: forall p i narrow extra wider. Profunctor p => Union narrow extra wider => p i (Variant narrow) -> p i (Variant wider)
```


