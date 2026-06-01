# Row Profunctors: Focus vs Merge

bambik builds profunctor UIs over `Record`-shaped (**product** — all fields present at once)
and `Variant`-shaped (**sum** — mutually exclusive cases) types. Two complementary families
of **row profunctors** do this, both under [`src/Data/Profunctor/Row/`](../src/Data/Profunctor/Row/):

- **Focus** — `StrongRecordToRecord`/`ChoiceVariantToVariant`, the row-typed `Strong`/`Choice`: zoom into a **sub**-record/sub-variant, carrying the rest of the row. The single-field/single-case combinators (`introduceProperty`, `editCase`, …) build on them.
- **Merge** — `recordToRecord`/`variantToVariant`/…: binary merges of **complete** row-shaped sub-profunctors. N-ary, tree-shaped.

They produce the **same profunctor values** from different angles; this note explains the relationship. The focus class for each row-kind lives *alongside* the merge class of the same kind: `StrongRecordToRecord` in [RecordToRecord.purs](../src/Data/Profunctor/Row/RecordToRecord.purs), `ChoiceVariantToVariant` in [VariantToVariant.purs](../src/Data/Profunctor/Row/VariantToVariant.purs).

## The idea in one screen

The punchline the code embodies: **the focus combinators are mostly just `Strong` and `Choice`, relabeled to rows.**

- **`StrongRecordToRecord`** (`focusRecord`) and **`ChoiceVariantToVariant`** (`focusVariant`) are the row-typed `Strong`/`Choice` — they operate on rows on **both sides**, embedding a whole **sub-Record/sub-Variant** profunctor (`p (Record sub) (Record sub')`) into a bigger row and carrying the complement. Each is *equivalent* to its positional original (generic `instance Strong p => StrongRecordToRecord p`, `Choice p => ChoiceVariantToVariant p`), so every `Strong`/`Choice` profunctor is one for free.
- **Product** (`Record`) combinators — `introduceProperty`, `eliminateProperty`, `editProperty` — rest on `StrongRecordToRecord` (`first`/`second` + insert/delete; `editProperty` is the value-level single-field lens).
- **Sum** (`Variant`) combinators — `eliminateCase`, `editCase` — rest on `ChoiceVariantToVariant` (`left`; `editCase` is the value-level single-case prism). There is one operation that *would* fall outside `Choice` — introducing a *fresh* case from a spontaneous source (a case the input never carries; see the rationale below) — but in this codebase that is built via the `VariantToVariant` composition path from sources that emit variants, not a dedicated focus combinator.
- A single-field/grow combinator is an **identity-pinned** merge; a merge is an **iterated** single-field combinator. Same values, two granularities — and since they share a row-kind, each focus class sits in the same module as its merge class (`StrongRecordToRecord` with `RecordToRecord`, `ChoiceVariantToVariant` with `VariantToVariant`).

See ["Materialized in code"](#materialized-in-code) for the module layout.

## What they share

| | |
|---|---|
| Type domain | Both build `p (X i) (Y o)` for `X, Y ∈ {Record, Variant}` |
| Row mechanics | Both use `RowToList`, `Prim.Row.{Cons,Union,Lacks}`, and the constraints in `src/Type/Row/Constraints.purs` |
| Semantic role of types | `Record` = entity (product, all fields present at once); `Variant` = event channel (sum, mutually exclusive cases) |
| Final values | A given profunctor value inhabits the same type either way (modulo `p` having the requisite instances) |

A submit-form profunctor built via `RecordToRecord.do` and the same one built by chaining single-field combinators (`introduceProperty … >>> introduceProperty …`) are the *same inhabitant* of `p (Record …) (Record …)`. The two strategies are not two different theories — they are two different ways of writing one theory down.

## Where they diverge

### 1. Granularity of one step

The canonical signatures sit side-by-side:

```purescript
-- Merge: binary merge of two complete row-shaped sub-profunctors.
-- src/Data/Profunctor/Row/RecordToRecord.purs:37
class Profunctor p <= RecordToRecord p where
  recordToRecord ::
    forall i1 o1 i2 o2 i12 i1x i2x i o.
    InclusiveRows i1 i2 i i12 i1x i2x =>
    ExclusiveRows o1 o2 o =>
    p (Record i1) (Record o1) ->
    p (Record i2) (Record o2) ->
    p (Record i) (Record o)
```

```purescript
-- Single-field combinator: unary lift of one value-source into one new field.
-- src/Data/Profunctor/Row/RecordToRecord.purs:96
introduceProperty
  :: forall p @l prop s t
   . IsSymbol l
  => Cons l prop s t
  => Lacks l s
  => StrongRecordToRecord p
  => Optic p (Record s) (Record t) (Record s) prop
-- expanded: p (Record s) prop -> p (Record s) (Record t)
```

`recordToRecord` consumes two row-shaped arguments at once. `introduceProperty` consumes one sub-profunctor that reads the whole record `s` and threads its value into an accumulator that grows by one field per step.

### 2. Composition shape

- **Merge** is a **tree**: `recordToRecord (recordToRecord a b) c` and `recordToRecord a (recordToRecord b c)` are both valid foldings; associativity holds modulo the row-disjointness constraints (`ExclusiveRows` on outputs, `InclusiveRows` on inputs).
- **Single-field combinators** are a **list**: `introduceProperty @"a" pa >>> introduceProperty @"b" pb >>> introduceProperty @"c" pc`. Linear, order-driven, accumulates one cell per step.

### 3. Typeclass surface on `p`

- **Merge** needs the four classes `RecordToRecord`, `RecordToVariant`, `VariantToRecord`, `VariantToVariant` (plus the umbrella `Row` aggregator in `src/Data/Profunctor/Row.purs:18`). Each is one method with a heavy row-constraint signature.
- **Single-field combinators** rest on the two focus classes `StrongRecordToRecord`/`ChoiceVariantToVariant` (the row-typed `Strong`/`Choice`). Because those classes have generic instances, every `Strong`/`Choice` profunctor supports them for free.

### 4. Type-inference cost

- Merge: each merge node solves a non-trivial row-union problem (`InclusiveRows` on input, `ExclusiveRows` on output). The inferrer must unify rows from both arguments. Error messages carry the full solved-row terms.
- Single-field combinators: each step solves only `Cons l a s t`. Smaller, more local constraint solving. Error messages stay short and field-local.

## The precise correspondence

For any `p` that supports both families, the two satisfy a fold/degeneracy duality at the value level.

**Merge combinators are folds of single-field combinators over a row-list.**
A binary `recordToRecord p1 p2 :: p (Record i) (Record o)` is equivalent to taking the row-list of the union output, and for each label `l : a` inserting an `introduceProperty @l` step into a chained pipeline, with `p1` and `p2` decomposed into their per-field constituents. The fold gives the same value as the binary merge.

**Single-field combinators are degenerate binary merges with identity.**
`introduceProperty @l q :: p (Record s) (Record t)` is the binary merge of identity (`p (Record s) (Record s)` doing nothing) with the single-field lift of `q`. The merge machinery reduces to "do nothing on the left, attach this one field on the right."

So **merge = iterated single-field, single-field = degenerate merge**.

At the **typeclass level**, neither implements the other polymorphically, because:

- Merge can't reach inside an opaque `p (Record i1) (Record o1)` argument to find its per-field atoms (the typeclass dictionary is parametric in the row shape, not in the row contents — there is no `RowList`-driven value-level recursion available without further machinery).
- Single-field chaining can't fuse a row of atoms into one binary merge without iterating field-by-field, which still requires `RowToList`-driven dispatch.

The relation lives at the **value-coincidence level**: the profunctor values denote the same thing, even though the typeclass machinery describing how to *build* them is not interchangeable.

## The four row classes as one discipline rule

The four merge classes are the 2×2 of `{Record, Variant}` input × `{Record, Variant}` output, and **each side's constraint is a function of that side's row-kind alone**:

| class | input | output |
|---|---|---|
| `recordToRecord` | `InclusiveRows` (Record-in) | `ExclusiveRows` (Record-out) |
| `recordToVariant` | `InclusiveRows` (Record-in) | `InclusiveRows` (Variant-out) |
| `variantToRecord` | `ExclusiveRows`+`DispatchableVariants` (Variant-in) | `ExclusiveRows` (Record-out) |
| `variantToVariant` | `ExclusiveRows`+`DispatchableVariants` (Variant-in) | `InclusiveRows` (Variant-out) |

The rule (input position is contravariant, so each kind imposes *opposite* disciplines in/out):

- **Record** ⇒ `InclusiveRows` when input (**share**, `Δ` — fields coexist, feed both branches) / `ExclusiveRows` when output (**disjoin** — concatenate non-colliding fields).
- **Variant** ⇒ `ExclusiveRows`+`DispatchableVariants` when input (**dispatch** — route the one live case) / `InclusiveRows` when output (**merge**, `∇` — branches may emit overlapping cases).

So the two diagonal classes are mixed Inclusive/Exclusive, and the two mixed classes are uniform:

- **`recordToVariant` = Inclusive/Inclusive** — the **form → event** shape (read shared form, merge emitted events); the type of the business atoms (`p (Record …) (Variant …)`).
- **`variantToRecord` = Exclusive/Exclusive** — the **event → display** shape (dispatch on which response occurred, fill disjoint fields).

### Only diagonals have focus

`introduceProperty @l f ≡ recordToRecord identity (rmap (\r -> {l: r}) f)` pins the **left operand to `identity`** — a `p a a`, which only typechecks when input and output are the **same kind**. The mixed classes' operands have *different* kinds (`p (Record …) (Variant …)`), so **no `identity` can sit there**.

> Only the two **diagonal** classes admit `identity`, so only they collapse to single-field combinators. The two **mixed** classes are **irreducibly binary** — crossing the product/sum boundary is exactly what an opaque business profunctor (e.g. a save-order action) does atomically, composed in with `>>>`.

### Reshape vs focus: two axes, not a trio

The mixed kinds still admit *unary* reshapings — just not focuses. `Data.Profunctor.Row` exports the four one-sided reshapings (`widenRecordInput`, `narrowVariantInput`, `narrowRecordOutput`, `widenVariantOutput`); a both-sides reshape for a mixed shape is just their composition (`widenVariantOutput ∘ widenRecordInput` for `Record → Variant`, `narrowVariantInput ∘ narrowRecordOutput` for `Variant → Record`) — pure `dimap`, no dedicated combinator needed. It is tempting to read `widen`/`narrow`/`focus` as a flat trio of analogue names; they are not. They sit on **two orthogonal axes**:

- **direction** — *widen* (grow, `sub → wider`) vs *narrow* (shrink, `wider → sub`).
- **complement** — *reshape* drops the complement (pure `dimap`, `Profunctor`-only) vs *focus* threads it across the input→output boundary (needs `Strong`/`Choice`).

| operation | direction | complement | strength |
|---|---|---|---|
| `focusRecord` / `focusVariant` | widen | **carried** | Strong / Choice |
| `Record → Variant` reshape (`widenVariantOutput ∘ widenRecordInput`) | widen | dropped | Profunctor |
| `Variant → Record` reshape (`narrowVariantInput ∘ narrowRecordOutput`) | narrow | dropped | Profunctor |

The tell: `focusRecord` is *itself* a widen-direction operation (`Record sub → Record s`, `s = sub ∪ rest`) — it merely *also* threads `rest`. So the real contrast between `focusRecord` and the `Record → Variant` reshape is the **complement** column, not direction; `widen`/`narrow` are the genuine duals (one axis), and `focus` is a widen *plus* complement-threading (the other axis). This is exactly why the mixed kinds get only reshape: with input and output of different kinds there is no same-kind `rest` to thread, so the product complement has no image in the sum one — sharpened to *unconditional vs gated* in the next section. Which free *direction* a mixed shape gets is then forced by variance — `Record → Variant` sits on the widen/widen side, `Variant → Record` on the narrow/narrow side, and each shape's *opposite* direction is the irreducible corner (needs fallback/defaults, collapsing to the binary merge above).

### The break, sharpened: unconditional vs gated

"A product complement has no image in a sum one" and "there is no inhabited case-introduction combinator" are **one** phenomenon, and it is sharper than coexist-vs-one-live. It is about *how the profunctor argument gets applied*:

```purescript
first f (a, c) = (f a, c)        second f (c, a) = (c, f a)     -- f ALWAYS applied
left  f (Left a)  = Left (f a)   left   f (Right c) = Right c   -- f applied ONLY on the selected branch
```

`Strong` runs its argument **unconditionally**; `Choice` runs it **gated by an input selector**. Products are unconditional (all fields present at once), sums are gated (one case, input-selected); `Strong` inherits the former, `Choice` the latter. A complement-carrying **focus** exists iff input and output *share that conditionality* — exactly the diagonals:

| shape | input | output | carry complement via | works? |
|---|---|---|---|---|
| R→R | unconditional | unconditional | `first` — rest always passes | ✓ |
| V→V | gated | gated | `left`/`right` — one branch live on **both** sides | ✓ |
| V→R | gated | **unconditional** | — mismatch — | ✗ needs **defaults** |
| R→V | **unconditional** | gated | — mismatch — | ✗ needs **fallback** |

When the sides differ (mixed), carrying the complement forces a conversion between unconditional and gated, and *that conversion is the fabrication*:

- **V→R** (gated in, unconditional out): only one input branch is live, but the product output demands every field — the un-selected branch's fields have no producer ⇒ **defaults** (fill the product the sum left empty).
- **R→V** (unconditional in, gated out): the record carries all fields, but the variant emits one case — you must *choose* which and discard the rest ⇒ **fallback** (collapse the product into the sum's one slot).

These two fabrications are themselves the product/sum dual pair: **fill** ↔ **collapse**. And inhabited case-introduction is the *degenerate unary case*: introducing into a sum needs the gate's input selector, but a spontaneous source supplies none, so even *with* a producer the case can never be emitted — whereas `second` (ungated) always emits its field, which is why `introduceProperty` exists and there is no `introduceCase`.

Note what stays free: **phantom**-widening a variant output (`widenVariantOutput`/`expand`) adds a case that is *never emitted* — no gate to satisfy — so it costs nothing. Only *inhabited* introduction is irreducible.

## Introduce vs eliminate: each isolates one row-discipline

A full `recordToRecord` does two things: **decompose** its input (`InclusiveRows`) and **assemble** its output (`ExclusiveRows`). Each single-field combinator isolates exactly one:

- **introduce** = the **output-assembly** half (grow one field/case; input passes through).
- **eliminate** = the **input-decomposition** half (split off one field/case; output passes through).

Concretely, `eliminateProperty` rides the input split — `lcmap \s -> Tuple (get l s) (delete l s)`; `eliminateCase` rides the input dispatch — `lcmap (on l Left Right)`. The split-off field/case is handed to a **sink whose output is discarded** — `p prop Unit` for `eliminateProperty` (the `Unit` is dropped via `snd`) and `p case_ Void` for `eliminateCase`. Those two sink-output types are the recurring `Unit`(terminal)/`Void`(initial) split: the product side *chooses* `Unit` (any type would do — `snd` throws it away — so it's pinned to `Unit` to make the discard explicit), while the sum side has `Void` *forced* on it (`left` routes the handled branch into the `Left` slot of `Either Void (Variant t)`, and only an uninhabited slot lets `either absurd identity` collapse it back to `Variant t`).

This is what "single-field combinator = degenerate merge with identity" means concretely: `introduceProperty l f ≡ recordToRecord identity (rmap (\r -> {l: r}) f)`, with the one-field operand a genuine record-reading sub-profunctor (the `p (Record s) prop` shape — it may read the whole record).

## When to use which

Both strategies build the same values; pick by the granularity of the pieces you start from.

### Single-field-combinator style

```purescript
introduceProperty @"shortId"  shortIdSource
  >>> introduceProperty @"orderId"  orderIdSource
  >>> introduceProperty @"customer" customerSource
```

Each step composes an atomic value-source with a single-field combinator that introduces one field; `>>>` chains them, growing the record one cell per step. Sub-records nest naturally — a field's source can itself be a chain of single-field steps.

This reads as "this record has these fields, one per line." It is the right style at the **leaf level** — when you start from atomic value-sources.

### Merge style — `src/Data/Profunctor/Row/Example.purs`

```purescript
-- src/Data/Profunctor/Row/Example.purs:102-108
recordToRecordExample :: MyRowToRowProfunctor
  (Record ( in1 :: MyData , in2 :: MyData , in3 :: MyData ))
  (Record ( out1 :: MyData , out2 :: MyData , out3 :: MyData ))
recordToRecordExample = RecordToRecord.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ("in1" :: MyData)) (Record ("out1" :: MyData)))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ("in1" :: MyData, "in2" :: MyData)) (Record ("out2" :: MyData)))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ("in3" :: MyData)) (Record ("out3" :: MyData)))
```

Each line is a *complete sub-profunctor with its own multi-field input and output row*. `RecordToRecord.do` merges them, solving `InclusiveRows` on inputs and `ExclusiveRows` on outputs.

This reads as "this record is the side-by-side combination of these pre-built sub-records." It is the right style at the **mid level** — when you have already-assembled row-shaped pieces and want to combine them.

### Mixing them

Idiomatic code uses **both** at different scales:

- Single-field combinators to build small records and variants from atomic value-sources.
- Merge to combine those pre-built sub-shapes into a larger composite.

The merge style shines once you have separate sub-records (say a customer block, a payment block, an address block) already built and want to glue them. Pick the granularity that matches the sentence you want to write.

## One-line summary

> Merge and single-field combinators are dual construction strategies for the same family of `Record`/`Variant`-shaped profunctors. Merge builds by **binary combination of complete sub-shapes**; single-field combinators build by **linear chaining of single-field atoms**. Both rest on the row-typed `Strong`/`Choice` focus classes (`focusRecord`/`focusVariant`): product focus carries its complement **unconditionally** (`first`), variant focus carries it **gated** (`left`/`right`), and the mixed kinds get no focus because crossing the product/sum boundary leaves no same-kind complement to thread. Neither family subsumes the other at the typeclass level, but their value-level denotations coincide, and a single-field combinator is an exact `identity`-pinned merge.

## Materialized in code

The repository implements this in `Data.Profunctor.Row.*`. Focus and merge are two distinct disciplines, but the focus class for each row-kind **lives in the same module as its merge class** (they share the row-kind and its constraints):

- **`StrongRecordToRecord`** (in [Row/RecordToRecord.purs](../src/Data/Profunctor/Row/RecordToRecord.purs), alongside `RecordToRecord`) — `class Strong p <= StrongRecordToRecord p` with `focusRecord :: p (Record sub) (Record sub') -> p (Record s) (Record t)` (`ExclusiveRows sub rest s`, `ExclusiveRows sub' rest t`), the row-typed `first`/`second`. The generic `instance Strong p => StrongRecordToRecord p` splits `s` into `(sub, rest)`, runs the argument on `sub` via `first`, and re-merges, so `StrongRecordToRecord p` is interchangeable with `Strong p`.
- **`ChoiceVariantToVariant`** (in [Row/VariantToVariant.purs](../src/Data/Profunctor/Row/VariantToVariant.purs), alongside `VariantToVariant`) — `class Choice p <= ChoiceVariantToVariant p` with `focusVariant :: p (Variant sub) (Variant sub') -> p (Variant s) (Variant t)`, the row-typed `left`/`right`; the generic `instance Choice p => ChoiceVariantToVariant p` dispatches via `Data.Variant.contract`, runs the argument via `left`, and re-merges via `expand`.
- **Combinators** — `introduceProperty`/`eliminateProperty`/`editProperty` (in [RecordToRecord.purs](../src/Data/Profunctor/Row/RecordToRecord.purs), on `StrongRecordToRecord`) and `eliminateCase`/`editCase` (in [VariantToVariant.purs](../src/Data/Profunctor/Row/VariantToVariant.purs), on `ChoiceVariantToVariant`) — each single-field/case combinator sits in the same module as the merge + focus class it builds on. `editProperty`/`editCase` are the value-level single field/case lens/prism. Because the classes have generic instances, all of these work on any `Strong`/`Choice` profunctor directly.
- **Case-introduction** — injecting a *fresh* variant case (the one operation outside `Choice`, see the rationale above) is *not* a dedicated combinator here: there is no inhabitant for it, and in practice it's built via the `VariantToVariant` composition path.
- **Merge classes** — `RecordToRecord`/`RecordToVariant`/`VariantToRecord`/`VariantToVariant`; the two diagonal modules additionally host their focus class.
- **Tests**: [test/Main.purs](../test/Main.purs) exercises both classes on `(->)` — `focusRecord`/`editProperty`/`introduceProperty`/`eliminateProperty` (incl. the introduce-then-eliminate identity that encodes the focus = identity-pinned merge claim) and `focusVariant`/`editCase`/`eliminateCase`.

## References

Source locations cited in this document:

- Merge classes:
  - `src/Data/Profunctor/Row/RecordToRecord.purs:37`
  - `src/Data/Profunctor/Row/RecordToVariant.purs`
  - `src/Data/Profunctor/Row/VariantToRecord.purs`
  - `src/Data/Profunctor/Row/VariantToVariant.purs`
  - Umbrella aggregator: `src/Data/Profunctor/Row.purs:18`
- Merge examples: `src/Data/Profunctor/Row/Example.purs`
- Default single-field lifts: `src/Data/Profunctor/Row/Default.purs`
- Row focus profunctors:
  - `src/Data/Profunctor/Row/RecordToRecord.purs` (`class StrongRecordToRecord`, `focusRecord`); `.../VariantToVariant.purs` (`class ChoiceVariantToVariant`, `focusVariant`)
  - `introduceProperty`/`eliminateProperty`/`editProperty` live in `RecordToRecord.purs`; `eliminateCase`/`editCase` live in `VariantToVariant.purs` (each beside the merge + focus class it builds on)
- Row constraints: `src/Type/Row/Constraints.purs`
