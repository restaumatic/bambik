# Row Profunctors: Focus vs Merge

bambik builds profunctor UIs over `Record`-shaped (**product** — all fields present at once)
and `Variant`-shaped (**sum** — mutually exclusive cases) types. Two complementary families
of **row profunctors** do this, both under [`src/Data/Profunctor/Row/`](../src/Data/Profunctor/Row/):

- **Focus** — `StrongRecordToRecord`/`ChoiceVariantToVariant`, the row-typed `Strong`/`Choice`: zoom into a **sub**-record/sub-variant, carrying the rest of the row. The single-field/single-case combinators (`introduceProperty`, `editCase`, …) build on them.
- **Merge** — `recordToRecord`/`variantToVariant`/…: binary merges of **complete** row-shaped sub-profunctors. N-ary, tree-shaped.

They produce the **same profunctor values** from different angles; this note explains the relationship — and the rationale behind the current focus/merge layout. The focus class now lives *alongside* the merge class of the same row-kind: `StrongRecordToRecord` in [RecordToRecord.purs](../src/Data/Profunctor/Row/RecordToRecord.purs), `ChoiceVariantToVariant` in [VariantToVariant.purs](../src/Data/Profunctor/Row/VariantToVariant.purs).

## The idea in one screen

The punchline the code embodies: **the focus combinators are mostly just `Strong` and `Choice`, relabeled to rows.**

- **`StrongRecordToRecord`** (`focusRecord`) and **`ChoiceVariantToVariant`** (`focusVariant`) are the row-typed `Strong`/`Choice` — they operate on rows on **both sides**, embedding a whole **sub-Record/sub-Variant** profunctor (`p (Record sub) (Record sub')`) into a bigger row and carrying the complement. Each is *equivalent* to its positional original (generic `instance Strong p => StrongRecordToRecord p`, `Choice p => ChoiceVariantToVariant p`), so every `Strong`/`Choice` profunctor — including `UI` — is one for free.
- **Product** (`Record`) combinators — `introduceProperty`, `eliminateProperty`, `editProperty` — rest on `StrongRecordToRecord` (`first`/`second` + insert/delete; `editProperty` is the value-level single-field lens).
- **Sum** (`Variant`) combinators — `eliminateCase`, `editCase` — rest on `ChoiceVariantToVariant` (`left`; `editCase` is the value-level single-case prism). There is one operation that *would* fall outside `Choice` — introducing a *fresh* case from a spontaneous source (a case the input never carries; see the rationale below) — but in this codebase that is built via the `Sum`/`VariantToVariant` composition path from widgets that emit variants, not a dedicated focus combinator.
- A focus/grow combinator is an **identity-pinned** merge; a merge is an **iterated** focus. Same values, two granularities — and since they share a row-kind, each focus class now sits in the same module as its merge class (`StrongRecordToRecord` with `RecordToRecord`, `ChoiceVariantToVariant` with `VariantToVariant`).

See ["Materialized in code"](#materialized-in-code) for the module layout.

## How to read the rest

Everything between here and "Materialized in code" is the **design rationale**, written *before*
the focus-class refactor. It starts from the original, unit/void-pinned primitive
shapes (`p Unit r`, the `ReadP`/`WriteP`/`FormP` classes — what this note historically called
"half-optics") and derives why the code landed on the `Strong`/`Choice` design above. So code
shown as "current" / "the file's claim" in those sections is **pre-refactor** (it lives in git
history); the present-day API is the `Row.*` modules summarized above. The payoff is
understanding *why* the structure is what it is — especially why the sum side is
almost-but-not-quite `Choice`.

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
-- src/Data/Profunctor/Row/RecordToRecord.purs:13
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

- **Row-to-row**: atoms (text inputs, click sources) live *outside* the framework. To enter the combinators they must first be lifted into single-field rows. `src/Data/Profunctor/Row/Default.purs` provides single-field seed/default/tag adapters (`withRecordDefault`, `tagVariantInput`, …, via `lcmap`/`rmap`) for that boundary. Once lifted, atoms are indistinguishable from any other row-shaped value.
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

- Row-to-row needs the four classes `RecordToRecord`, `RecordToVariant`, `VariantToRecord`, `VariantToVariant` (plus the umbrella `Row` aggregator in `src/Data/Profunctor/Row.purs:33`). Each is one method with a heavy row-constraint signature.
- Half-lens needs more, smaller classes — `IntroVarP`, `ExceptP`, `ReadP`, `WriteP`, `EditPropP`, plus the composition-style `Endo`, `Sum`, `Zero`, `One`, `Product`, `ProductToSum`. Each method is structurally simpler.

These half-lens classes are not arbitrary: as "The primitive level" part shows, `ReadP`/`WriteP`/`IntroVarP`/`ExceptP` are the four corners of one `Strong`/`Choice` fanout with a unit-pinned slot, and `FormP`/`XP`/`YP`/`ZP` are the boundary adaptors between `Unit` and `Void`. The "not interchangeable at the typeclass level" verdict softens once `ReadP` is read as a *unit-pinned weakening of `Strong`* — see that part for where the equivalence does and does not hold.

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

The relation lives at the **value-coincidence level**: the profunctor values denote the same thing, even though the typeclass machinery describing how to *build* them is not interchangeable. But that "not interchangeable" is a statement about the *current* typeclass signatures, not about the constructions themselves — at the **primitive level** (next part) the relation is much sharper, and turns on a single design choice in the half-optic signatures.

## The primitive level: half-optics as pinned `second'`

The whole half-optic family is built from four primitives, and each is the Strong/Choice fanout operation with one slot **pinned to a monoidal unit**. Seeing the pin is the key to the row-to-row relation.

### The read primitive, and the `p Unit r` vs `p s r` question

```purescript
-- the original (pre-refactor) read primitive
liftRead :: forall s r. p Unit r -> p s (Tuple s r)   -- "r read, s preserved, Unit for control flow"
```

The source's *input* is pinned to `Unit`: the introduced value cannot see the accumulator `s`. The natural generalization unpins it:

```purescript
liftReadCtx :: forall s r. p s r -> p s (Tuple s r)   -- the introduced value may read s
```

`liftReadCtx` is **exactly `second'`** (i.e. it is *equivalent* to `Strong`, not merely derivable from it):

```purescript
-- forward: Strong ⇒ liftReadCtx, precompose the diagonal Δ
liftReadCtx f = lcmap (\s -> Tuple s s) (second' f)

-- back: liftReadCtx ⇒ Strong, feed the whole input and project the redundant copy away
second' f = liftReadCtx (lcmap snd f) # rmap (\(Tuple (Tuple c _) b) -> Tuple c b)
```

The original `liftRead` **cannot** reach `second'`: its source input is fixed to `Unit`, so it can never receive the focus value to transform. That was precisely the pre-refactor file's own claim — *"ReadP is a superclass of Strong but not vice versa"* (`ReadP.purs`, witnessed by `strongToReadP`). So:

> **the original `liftRead` (`p Unit r`) ⊊ Strong** — and that gap *is* "the half". **`liftReadCtx` (`p s r`) = Strong**, full strength. The code took the latter.

### The comonoid framing

Both shapes are `second'` precomposed with a **comonoid map** on the Cartesian input `s` (a comonoid has copy `Δ : s → s×s` and discard `! : s → Unit`):

| primitive | precomposed map | source sees input? | strength |
|---|---|---|---|
| `liftRead` (current) | `s ↦ (s, !s)` — **discard** | no (`Unit`) | **⊊ Strong** |
| `liftReadCtx` (proposed) | `s ↦ (s, s)` — **copy / Δ** | yes (full `s`) | **= Strong** |

The entire design is "`second'` parameterized by which comonoid operation feeds it." Discard → context-free half-lens; copy → full Strong.

### Four primitives: a 2×2×pin

The eliminate primitives are the **transposes** (arrows reversed) of the introduce primitives, and the sum primitives are the product primitives with `(Unit, Tuple)` swapped for `(Void, Either)`:

```purescript
-- introduce / read (grow output)              -- eliminate / write (consume input)
liftRead     :: p Unit r -> p s (Tuple  s r)   liftWrite  :: p w Unit -> p (Tuple  w s) s
liftIntroVar :: p Void r -> p s (Either s r)   liftExcept :: p w Void -> p (Either w s) s
```

|  | introduce / read (grow output) | eliminate / write (consume input) |
|---|---|---|
| **product** (×, `Tuple`, Strong) | `liftRead : p Unit r → p s (s×r)` — pin source **input** = `Unit` | `liftWrite : p w Unit → p (w×s) s` — pin sink **output** = `Unit` |
| **sum** (+, `Either`, Choice) | `liftIntroVar : p Void r → p s (s+r)` — pin source **input** = `Void` | `liftExcept : p w Void → p (w+s) s` — pin sink **output** = `Void` |

- **Columns** are transposes (`liftWrite = liftRead`ᵀ, `liftExcept = liftIntroVar`ᵀ); the comments mirrored — *"r read, s preserved"* ↔ *"w written, s preserved"*.
- **Rows** are product ↔ coproduct.
- **Pin position** flips with the column: read pins the source *input*; write pins the sink *output*.
- The isos confirmed the pins: `ReadP ↔ Reader r` / `WriteP ↔ Writer w` used `Unit`; `IntroVarP ↔ IntroVar r` / `ExceptP ↔ Except w` used `Void`.

### Product upgradable, sum forced

The `Unit`-pinned product primitives are proper weakenings of Strong (`strongToReadP`, `strongToWriteP` witness both directions of the "superclass of Strong" claim). Unpinning them recovers full Strong:

- `liftReadCtx :: p s r -> p s (s×r)` — the introduced field may **read** the accumulator.
- `liftWriteCtx :: p w s -> p (w×s) s` — the consuming step may **rewrite** the surviving state instead of emit-and-vanish. This is the read-*and*-write power — the field lens, now `editProperty` (and the sub-record `RecordToRecord.focusRecord`) ([Row/RecordToRecord.purs](../src/Data/Profunctor/Row/RecordToRecord.purs); formerly the `EditPropP` class).

The `Void`-pinned **sum** primitives admit no such `p s r` upgrade: when one variant case is active, all others are absent (mutual exclusion), so the introduced case has no sibling to read and the eliminated case exits with no surviving continuation. `Void` is **forced by case-exclusivity**, not an artifact.

But "no `p s r` upgrade" is a different question from "derivable from `Choice`", and here the two sum primitives part ways. `liftExcept` *is* `Choice`-derivable — `liftExcept f = rmap (either absurd identity) (left f)` — exactly as the `Unit`-pinned `liftWrite` is `Strong`-derivable; the `Void` lands in `left`'s untouched output slot and `absurd` discharges it. `liftIntroVar` is **not**: `Choice`'s `right` only fires its branch on a `Right` *input*, but the introduced case is never an input — the source emits it spontaneously — so no faithful `Choice` term has its shape. Hence the eliminate/focus sum half-optics fold onto `Choice` (`left`/`right`), and only **sum-introduce** is genuinely irreducible — incomparable to `Choice`, not merely weaker. (The code does not carry a class for it; see "Materialized in code".)

## The four row classes as one discipline rule

The four row-to-row classes are the 2×2 of `{Record, Variant}` input × `{Record, Variant}` output, and **each side's constraint is a function of that side's row-kind alone**:

| class | input | output |
|---|---|---|
| `recordToRecord` | `InclusiveRows` (Record-in) | `ExclusiveRows` (Record-out) |
| `recordToVariant` | `InclusiveRows` (Record-in) | `InclusiveRows` (Variant-out) |
| `variantToRecord` | `ExclusiveRows`+`Dispatchable` (Variant-in) | `ExclusiveRows` (Record-out) |
| `variantToVariant` | `ExclusiveRows`+`Dispatchable` (Variant-in) | `InclusiveRows` (Variant-out) |

The rule (input position is contravariant, so each kind imposes *opposite* disciplines in/out):

- **Record** ⇒ `InclusiveRows` when input (**share**, `Δ` — fields coexist, feed both branches) / `ExclusiveRows` when output (**disjoin** — concatenate non-colliding fields).
- **Variant** ⇒ `ExclusiveRows`+`Dispatchable` when input (**dispatch** — route the one live case) / `InclusiveRows` when output (**merge**, `∇` — branches may emit overlapping cases).

So the two diagonal classes are mixed Inclusive/Exclusive, and the two mixed classes are uniform:

- **`recordToVariant` = Inclusive/Inclusive** — the **form → event** shape (read shared form, merge emitted events); the type of the business atoms (`p (Record …) (Variant …)`).
- **`variantToRecord` = Exclusive/Exclusive** — the **event → display** shape (dispatch on which response occurred, fill disjoint fields).

### Only diagonals have half-optics

`introduceProperty ≡ recordToRecord identity (oneField …)` and `introduceCase ≡ variantToVariant identity (oneCase …)` both pin the **left operand to `identity`** — a `p a a`, which only typechecks when input and output are the **same kind**. The mixed classes' operands have *different* kinds (`p (Record …) (Variant …)`), so **no `identity` can sit there**.

> Only the two **diagonal** classes admit `identity`, so only they collapse to single-field half-optics. The two **mixed** classes are **irreducibly binary** — crossing the product/sum boundary is exactly what an opaque business profunctor (`saveOrder`, `someAction`) does atomically, composed in with `>>>`.

## Half = exactly one row-discipline

A full `recordToRecord` does two things: **decompose** its input (`InclusiveRows`) and **assemble** its output (`ExclusiveRows`). Each half-optic isolates exactly one:

- **introduce** = the **output-assembly** half (grow one field/case; input passes through).
- **eliminate** = the **input-decomposition** half (split off one field/case; output passes through).

Concretely, `eliminateProperty` rides the input split — `lcmap \s -> Tuple (get l s) (delete l s)`; `eliminateCase` rides the input dispatch — `lcmap (on l Left Right)`.

This sharpens the earlier *"half-lens = degenerate row-to-row with identity"*: under the **`p s r`** shape it is literally exact — `introduceProperty l f ≡ recordToRecord identity (rmap (\r -> {l: r}) f)`, with the one-field operand a genuine record-reading sub-profunctor. Under the current `p Unit r` shape the operand is a context-free source `p Void prop` that cannot read the record, so the equation only holds after laundering through `Void`/`FormP` (next section) — the symptom of the strength mismatch (row-to-row is Strong-strength, the current half-optic is not).

## The `FormP` / `XP` / `YP` / `ZP` quartet

The four boundary adaptors classify cleanly by **which map they require** between the unit (`Unit`, terminal) and the co-unit (`Void`, initial):

| class | signature | needs | verdict |
|---|---|---|---|
| `FormP` | `p Void r → p Unit r` (input) | `Unit → Void` — **impossible** | **genuine** (used by `introduceProperty`) |
| `ZP` | `p a Unit → p a Void` (output) | `Unit → Void` — **impossible** | **genuine** (used by `eliminateCase`) |
| `XP` | `p Unit a → p Void a` (input) | `Void → Unit` = `absurd` | free, `= lcmap absurd` (`introduceCase`) |
| `YP` | `p a Void → p a Unit` (output) | `Void → Unit` = `absurd` | free, `= rmap absurd` (`eliminateProperty`) |

The only **genuine** capabilities are `FormP` and `ZP` — the two that must conjure the impossible `Unit → Void`. `FormP` fabricates an output from no input (a **source**); `ZP` fabricates the no-return of a **diverging sink**. Both are *removed* in the current design, by different routes:

- **`FormP` (product introduce)** is **dissolved** by the `p s r` shape — the source reads `s` instead of fabricating from `Void`, so no `Void`/`Unit` laundering remains (a source becomes a `p s r` that ignores `s`, and `lcmap (const _)` is free).
- **`ZP` (sum eliminate)** is **sidestepped** — `eliminateCase` takes its diverging handler as `p case Void` directly (no `Unit → Void` step), after which it folds onto `Choice` via `left`. So sum-*eliminate* is reducible, not irreducible.
- The free `XP`/`YP` are inlined as `lcmap`/`rmap absurd`; `eliminateProperty` needs only `YP`, gone with the product `Strong` fold.

Note this corrects a tempting symmetry: the irreducible thing on the sum side is **not** an adaptor but the *introduce* primitive `liftIntroVar` itself (incomparable to `Choice`, previous section) — the eliminate side is plain `Choice`.

## What adopting `p s r` changed

(This is the decision the code took — see ["Materialized in code"](#materialized-in-code).)

The context-reading question — *can the introduced/edited thing see the accumulator?* — is decided by the **input kind alone**:

| | output: Record (disjoin) | output: Variant (merge) |
|---|---|---|
| **input: Record (share)** — `p s r` ✓, `FormP` removable | `recordToRecord` — has half-optic | `recordToVariant` — no half-optic |
| **input: Variant (dispatch)** — `Void` forced | `variantToRecord` — no half-optic | `variantToVariant` — has half-optic |

So `p s r`'s reach is **exactly the two Record-*input* classes**. Adopting it made the product half-optic an exact `identity`-pinned `recordToRecord`, unified the half-optic and row-to-row input-sharing under one `Strong` fanout, and retired `FormP`.

The cost paid: `ReadP` stopped being *weaker* than `Strong` (it simply *is* `Strong`, so the bespoke class is gone), and the clean `Reader r` iso — which depended on the focus input being `Unit` — degrades toward `Asker` (reader-with-context). Because the core `UI m` is already `Strong` ([UI.purs:76](../src/UI.purs#L76)), the weaker `p Unit r` bought nothing *for `UI`*; the only thing it protected was giving the read capability to a profunctor that is *not* `Strong` — which this codebase has no use for. That made the trade-off easy.

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

### Row-to-row style — `src/Data/Profunctor/Row/Example.purs`

```purescript
-- src/Data/Profunctor/Row/Example.purs:105-113
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

> Row-to-row and half-lens are dual construction strategies for the same family of `Record`/`Variant`-shaped profunctors. Row-to-row builds by **binary merge of complete sub-shapes**; half-lens builds by **linear chaining of single-field atoms**. The empty-row placeholders (`Variant ()`, `Record ()`) in half-lens types are not entities or events but markers for "no row structure here — bare value attaches here." At the primitive level each half-optic is `second'`/`liftIntroVar` with one slot pinned to a monoidal **unit** — `Unit` (terminal) for products, `Void` (initial) for sums; the product pins are weakenings of `Strong` that `p s r`/`p w s` upgrade to full strength (read → edit), while the sum pins are **forced by case-exclusivity**. Neither family subsumes the other at the typeclass level, but their value-level denotations coincide, and under the `p s r` shape the half-optic becomes an exact `identity`-pinned row-to-row.

## Materialized in code

The repository implements this in `Data.Profunctor.Row.*`. Focus and merge are still two distinct disciplines, but the focus class for each row-kind now **lives in the same module as its merge class** (they share the row-kind and its constraints):

- **`StrongRecordToRecord`** (in [Row/RecordToRecord.purs](../src/Data/Profunctor/Row/RecordToRecord.purs), alongside `RecordToRecord`) — `class Strong p <= StrongRecordToRecord p` with `focusRecord :: p (Record sub) (Record sub') -> p (Record s) (Record t)` (`ExclusiveRows sub rest s`, `ExclusiveRows sub' rest t`), the row-typed `first`/`second`. The generic `instance Strong p => StrongRecordToRecord p` splits `s` into `(sub, rest)`, runs the argument on `sub` via `first`, and re-merges, so `StrongRecordToRecord p` is interchangeable with `Strong p`.
- **`ChoiceVariantToVariant`** (in [Row/VariantToVariant.purs](../src/Data/Profunctor/Row/VariantToVariant.purs), alongside `VariantToVariant`) — `class Choice p <= ChoiceVariantToVariant p` with `focusVariant :: p (Variant sub) (Variant sub') -> p (Variant s) (Variant t)`, the row-typed `left`/`right`; the generic `instance Choice p => ChoiceVariantToVariant p` dispatches via `Data.Variant.contract`, runs the argument via `left`, and re-merges via `expand`.
- **Combinators** — `introduceProperty`/`eliminateProperty`/`editProperty` (in [RecordToRecord.purs](../src/Data/Profunctor/Row/RecordToRecord.purs), on `StrongRecordToRecord`) and `eliminateCase`/`editCase` (in [VariantToVariant.purs](../src/Data/Profunctor/Row/VariantToVariant.purs), on `ChoiceVariantToVariant`) — each single-field/case combinator sits in the same module as the merge + focus class it builds on. `editProperty`/`editCase` are the value-level single field/case lens/prism (`Commons.property`/`variant`). Because the classes have generic instances, all of these work on `UI` directly.
- **Case-introduction** — injecting a *fresh* variant case (the one operation outside `Choice`, see the rationale above) is *not* a dedicated combinator here: there was no inhabitant for it, and in practice it's built via the `Sum`/`VariantToVariant` composition path. (An earlier `IntroVarP` class materialized it but carried no instances and went unused; removed.)
- **Merge classes** — `RecordToRecord`/`RecordToVariant`/`VariantToRecord`/`VariantToVariant` are unchanged; the two diagonal modules now additionally host their focus class.
- **Tests**: [test/Main.purs](../test/Main.purs) exercises both classes on `(->)` — `focusRecord`/`editProperty`/`introduceProperty`/`eliminateProperty` (incl. the introduce-then-eliminate identity that encodes the focus = identity-pinned merge claim) and `focusVariant`/`editCase`/`eliminateCase`.

> Note: the `file:line` citations below that point at `ReadP.purs`/`WriteP.purs`/`EditPropP.purs` and the `p Unit r`/`FormP` shapes describe the **pre-refactor** design that motivated this note. That code has been superseded (it lives in git history); the analysis above stands as the rationale for the current `Row.*` focus layout.

## References

Source locations cited in this document (★ = pre-refactor, see note above):

- Row-to-row classes:
  - `src/Data/Profunctor/Row/RecordToRecord.purs:13`
  - `src/Data/Profunctor/Row/RecordToVariant.purs`
  - `src/Data/Profunctor/Row/VariantToRecord.purs`
  - `src/Data/Profunctor/Row/VariantToVariant.purs`
  - Umbrella aggregator: `src/Data/Profunctor/Row.purs:33`
- Row-to-row examples: `src/Data/Profunctor/Row/Example.purs`
- Default single-field lifts: `src/Data/Profunctor/Row/Default.purs`
- Row focus profunctors (current):
  - `src/Data/Profunctor/Row/RecordToRecord.purs` (`class StrongRecordToRecord`, `focusRecord`); `.../VariantToVariant.purs` (`class ChoiceVariantToVariant`, `focusVariant`)
  - `introduceProperty`/`eliminateProperty`/`editProperty` live in `RecordToRecord.purs`; `eliminateCase`/`editCase` live in `VariantToVariant.purs` (each beside the merge + focus class it builds on)
- ★ Pre-refactor half-lens primitives (git history): `ReadP.purs` (`ReadP`/`FormP`/`XP`/`YP`/`ZP`, `introduceProperty`, `introduceCase`), `WriteP.purs` (`WriteP`), `EditPropP.purs` (`EditPropP`)
- Composition-style classes: `src/Data/Profunctor/Endo.purs:12`, `src/Data/Profunctor/Sum.purs:13`, `src/Data/Profunctor/Zero.purs`, `src/Data/Profunctor/One.purs`, `src/Data/Profunctor/Product.purs`, `src/Data/Profunctor/ProductToSum.purs:15`
- Row constraints: `src/Type/Row/Constraints.purs`
- Half-lens usage example: `demo/1/Main.purs`
- Core UI type and binary combinator instances: `src/UI.purs` (binary instances around lines 193–265)
