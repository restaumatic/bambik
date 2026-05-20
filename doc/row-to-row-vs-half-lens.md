# Row-to-Row Profunctors vs Half-Lenses

This note explains the relationship between the two construction families bambik provides for building profunctor UIs over `Record`-shaped and `Variant`-shaped types:

- **Row-to-row** combinators in `src/Data/Profunctor/RowToRow/` — binary merges of complete row-shaped sub-profunctors.
- **Half-lens** primitives in `src/Data/Profunctor/` (`IntroVarP`, `ExceptP`, `ReadP`, `WriteP`, `EditPropP`) together with the composition-style classes `Endo`, `Sum`, `Zero`, `One`, `Product`, `ProductToSum` — unary lifts of atomic single-field cells, chained linearly.

Both families exist on purpose, both are used in real demos, and they produce the same profunctor values from different angles.

## TL;DR

Row-to-row and half-lens are **dual construction strategies for the same family of profunctor values**. Row-to-row composes *complete sub-shapes by binary merge*; half-lens composes *atomic single-field cells by linear chaining*. Same target, different traversal. Neither subsumes the other at the typeclass level, but the values they produce coincide, and idiomatic UI code uses them as complementary tools at different scales of composition.

## What they share

| | |
|---|---|
| Type domain | Both build `p (X i) (Y o)` for `X, Y ∈ {Record, Variant}` |
| Row mechanics | Both use `RowToList`, `Prim.Row.{Cons,Union,Lacks}`, and the constraints in `src/Type/Row/Constraints.purs` |
| Semantic role of types | `Record` = entity (product, all fields present at once); `Variant` = event channel (sum, mutually exclusive cases) |
| Final values | A given profunctor value inhabits the same type either way (modulo `p` having the requisite instances) |

A submit-form profunctor built via `RecordToRecord.do` and the same one built via `Endo.do { introduceProperty … ; introduceProperty … }` are the *same inhabitant* of `p (Record …) (Record …)`. The frameworks are not two different theories — they are two different ways of writing one theory down.

## Where they diverge

### 1. Granularity of one step

The canonical signatures sit side-by-side:

```purescript
-- Row-to-row: binary merge of two complete row-shaped sub-profunctors.
-- src/Data/Profunctor/RowToRow/RecordToRecord.purs:13
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
-- Half-lens: unary lift of one atomic value-source into one new field.
-- src/Data/Profunctor/ReadP.purs:52
introduceProperty ::
  forall p @l prop s t.
  IsSymbol l =>
  Cons l prop s t =>
  ReadP p => FormP p =>
  Optic p (Record s) (Record t) (Variant ()) prop
-- expanded: p (Variant ()) prop -> p (Record s) (Record t)
```

Row-to-row consumes two row-shaped arguments at once. Half-lens consumes one atomic argument and threads it into an accumulator that grows by one field per step.

### 2. Where atoms enter

- **Row-to-row**: atoms (text inputs, click sources) live *outside* the framework. To enter the combinators they must first be lifted into single-field rows. `src/Data/Profunctor/RowToRow/Default.purs` provides exactly those single-field lifts via `dimap`. Once lifted, atoms are indistinguishable from any other row-shaped value.
- **Half-lens**: the atom-to-row lift is *built into* each primitive. `introduceProperty` directly accepts a `p (Variant ()) prop` and emits a row-shaped result. No external lift step is needed.

### 3. The empty-row placeholders

This is the observation that clarifies the relation. In half-lens signatures you see `Record ()` and `Variant ()` on the *atom* side:

```purescript
introduceProperty :: ... => p (Variant ()) prop -> p (Record s) (Record t)
introduceCase     :: ... => p (Record ()) case_ -> p (Variant s) (Variant t)
```

These are *not* semantic empty entities or empty events:

- `Variant ()` ≅ `Void` (or `Unit` in a covariant position) — it marks "no input choice to dispatch on; the value source has no row structure attached."
- `Record ()` ≅ `Unit` — it marks "no input context to read from; this side carries no row structure yet."

They are syntactic markers of the boundary where row structure ends and bare values begin. Row-to-row never needs these placeholders because by the time a value reaches its combinators it has already been wrapped into a single-field row.

### 4. Composition shape

- **Row-to-row** is a **tree**: `recordToRecord (recordToRecord a b) c` and `recordToRecord a (recordToRecord b c)` are both valid foldings; associativity holds modulo the row-disjointness constraints (`ExclusiveRows` on outputs, `InclusiveRows` on inputs).
- **Half-lens** is a **list**: `introduceProperty @"a" pa >>> introduceProperty @"b" pb >>> introduceProperty @"c" pc`. Linear, order-driven, accumulates one cell per step. The `Endo.do` and `Sum.do` qualified-do blocks sugar exactly this chaining via `pendo`/`psum`.

### 5. Typeclass surface on `p`

- Row-to-row needs the four classes `RecordToRecord`, `RecordToVariant`, `VariantToRecord`, `VariantToVariant` (plus the umbrella `RowToRow` aggregator in `src/Data/Profunctor/RowToRow/RowToRow.purs:33`). Each is one method with a heavy row-constraint signature.
- Half-lens needs more, smaller classes — `IntroVarP`, `ExceptP`, `ReadP`, `WriteP`, `EditPropP`, plus the composition-style `Endo`, `Sum`, `Zero`, `One`, `Product`, `ProductToSum`. Each method is structurally simpler.

### 6. Type-inference cost

- Row-to-row: each merge node solves a non-trivial row-union problem (`InclusiveRows` on input, `ExclusiveRows` on output). Inferrer must unify rows from both arguments. Error messages carry the full solved-row terms.
- Half-lens: each step solves only `Cons l a s t`. Smaller, more local constraint solving. Error messages stay short and field-local.

## The precise correspondence

For any `p` that supports both families, the two satisfy a fold/degeneracy duality at the value level.

**Row-to-row combinators are folds of half-lens primitives over a row-list.**
A binary `recordToRecord p1 p2 :: p (Record i) (Record o)` is equivalent to taking the row-list of the union output, and for each label `l : a` inserting an `introduceProperty @l` (or `writeProp @l`) step into a chained pipeline, with `p1` and `p2` decomposed into their per-field constituents. The fold gives the same value as the binary merge.

**Half-lens primitives are degenerate binary merges with identity.**
`introduceProperty @l q :: p (Record s) (Record t)` is the binary merge of identity (`p (Record s) (Record s)` doing nothing) with the single-field lift of `q`. The row-to-row machinery reduces to "do nothing on the left, attach this one field on the right."

So **row-to-row = iterated half-lens, half-lens = degenerate row-to-row**.

At the **typeclass level**, neither implements the other polymorphically, because:

- Row-to-row can't reach inside an opaque `p (Record i1) (Record o1)` argument to find its per-field atoms (the typeclass dictionary is parametric in the row shape, not in the row contents — there is no `RowList`-driven value-level recursion available without further machinery).
- Half-lens can't fuse a row of atoms into one binary merge without iterating field-by-field, which still requires `RowToList`-driven dispatch and additional capabilities (e.g. `FormP`, `XP`, `YP`, `ZP` in `src/Data/Profunctor/ReadP.purs`).

The relation lives at the **value-coincidence level**: the profunctor values denote the same thing, even though the typeclass machinery describing how to *build* them is not interchangeable.

## When to use which (in this codebase)

The codebase has live examples of both:

### Half-lens style — `demo/1/Main.purs`

```purescript
-- demo/1/Main.purs:23-30 (excerpt)
MDC.card Endo.do
  MDC.caption $ staticText "Identifier"
  shortId $ MDC.filledTextField { floatingLabel: "Short ID" }
  orderId $ MDC.filledTextField { floatingLabel: "Unique ID" }
customer $ MDC.card Endo.do
  MDC.caption $ staticText "Customer"
  firstName $ MDC.filledTextField { floatingLabel: "First name" }
  lastName $ MDC.filledTextField { floatingLabel: "Last name" }
```

Each line is an *atomic widget composed with a half-lens* (`shortId`, `firstName`, ...) that introduces one field. `Endo.do` chains them via `pendo`. Sub-forms nest naturally: `customer $ MDC.card Endo.do { ... }` says "the `customer` field's value is itself a record built by chained half-lens steps."

This reads as "this form has these fields, here, one per line." It is the right style at the **leaf level** — when you start from atomic widgets.

### Row-to-row style — `src/Data/Profunctor/RowToRow/Example.purs`

```purescript
-- src/Data/Profunctor/RowToRow/Example.purs:89-95
recordToRecordExample :: MyRowToRowProfunctor
  (Record ( in1 :: MyData , in2 :: MyData , in3 :: MyData ))
  (Record ( out1 :: MyData , out2 :: MyData , out3 :: MyData ))
recordToRecordExample = RecordToRecord.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ("in1" :: MyData)) (Record ("out1" :: MyData)))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ("in1" :: MyData, "in2" :: MyData)) (Record ("out2" :: MyData)))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ("in3" :: MyData)) (Record ("out3" :: MyData)))
```

Each line is a *complete sub-profunctor with its own multi-field input and output row*. `RecordToRecord.do` merges them, solving `InclusiveRows` on inputs and `ExclusiveRows` on outputs.

This reads as "this form is the side-by-side combination of these pre-built sub-forms." It is the right style at the **mid level** — when you have already-assembled row-shaped pieces and want to combine them.

### Mixing them

Idiomatic bambik code uses **both** at different scales:

- Half-lens to build small records and variants from atomic widgets.
- Row-to-row to combine those pre-built sub-forms into a larger composite.

`demo/1/Main.purs` is half-lens throughout because the entire order form is assembled directly from atomic widgets via `Endo.do`/`Sum.do`. The row-to-row style would shine in a different demo where, say, you have a separate `customerForm`, `paymentForm`, and `addressForm` already built and want to glue them.

The framework is bilingual on purpose. Pick the granularity that matches the sentence you want to write.

## One-line summary

> Row-to-row and half-lens are dual construction strategies for the same family of `Record`/`Variant`-shaped profunctors. Row-to-row builds by **binary merge of complete sub-shapes**; half-lens builds by **linear chaining of single-field atoms**. The empty-row placeholders (`Variant ()`, `Record ()`) in half-lens types are not entities or events but markers for "no row structure here — bare value attaches here." Neither family subsumes the other at the typeclass level, but their value-level denotations coincide.

## References

Source locations cited in this document:

- Row-to-row classes:
  - `src/Data/Profunctor/RowToRow/RecordToRecord.purs:13`
  - `src/Data/Profunctor/RowToRow/RecordToVariant.purs`
  - `src/Data/Profunctor/RowToRow/VariantToRecord.purs`
  - `src/Data/Profunctor/RowToRow/VariantToVariant.purs`
  - Umbrella aggregator: `src/Data/Profunctor/RowToRow/RowToRow.purs:33`
- Row-to-row examples: `src/Data/Profunctor/RowToRow/Example.purs`
- Default single-field lifts: `src/Data/Profunctor/RowToRow/Default.purs`
- Half-lens primitives:
  - `src/Data/Profunctor/ReadP.purs:28` (`ReadP`), `:52` (`introduceProperty`), `:77` (`introduceCase`)
  - `src/Data/Profunctor/WriteP.purs:20` (`WriteP`)
  - `src/Data/Profunctor/IntroVarP.purs:22` (`IntroVarP`)
  - `src/Data/Profunctor/ExceptP.purs:20` (`ExceptP`)
  - `src/Data/Profunctor/EditPropP.purs:13` (`EditPropP`)
- Composition-style classes: `src/Data/Profunctor/Endo.purs:12`, `src/Data/Profunctor/Sum.purs:13`, `src/Data/Profunctor/Zero.purs`, `src/Data/Profunctor/One.purs`, `src/Data/Profunctor/Product.purs`, `src/Data/Profunctor/ProductToSum.purs:15`
- Row constraints: `src/Type/Row/Constraints.purs`
- Half-lens usage example: `demo/1/Main.purs`
- Core UI type and binary combinator instances: `src/UI.purs` (binary instances around lines 197–268)
