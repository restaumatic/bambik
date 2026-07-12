# Row Profunctors: Focus vs Merge

bambik builds profunctor UIs over `Record`-shaped (**product** — all fields present at once)
and `Variant`-shaped (**sum** — mutually exclusive cases) types. Two complementary families
of **row profunctors** do this, both under [`src/Data/Profunctor/Row/`](../src/Data/Profunctor/Row/):

- **Focus** — `focusRecord`/`focusVariant`, the row-typed `Strong`/`Choice`: zoom into a **sub**-record/sub-variant, carrying the rest of the row. The single-field/single-case combinators (`property`, `case_`, …) are their single-label forms.
- **Merge** — `recordToRecord`/`variantToVariant`/…: binary merges of **complete** row-shaped sub-profunctors. N-ary, tree-shaped.

They produce the **same profunctor values** from different angles; this note explains the relationship. Each row-kind's focus machinery lives *alongside* the merge class of the same kind: `focusRecord` in [RecordToRecord.purs](../src/Data/Profunctor/Row/RecordToRecord.purs), `case_` (the single-case form; the sub-row `focusVariant` is pruned) in [VariantToVariant.purs](../src/Data/Profunctor/Row/VariantToVariant.purs).

> **Syntax note.** Variant types appear throughout in the fork's `[ … ]` sugar — `[ a :: X | r ]` is `Variant ( a :: X | r )`, `[ | r ]` is `Variant r`, `[]` is `Variant ()`; records use stock `{ … }`. bambik builds on a **forked `purs`** that adds this (plus `.label` constructor and `case _ of .label` pattern) sugar — see [variant-sugar.md](./variant-sugar.md).

> **Code status.** The modules keep only what the demos reach (plus the mixed-strength layer, kept deliberately). Some combinators this note *derives* — `recordToProperty`, `eliminateProperty`, `caseToVariant`, `focusVariant`, `lensE`, the default lifts, the narrowing and single-label reshapings — are **pruned from the code**; they remain in this note because the algebra needs their names to state the laws and dualities. The living inventory is in ["Materialized in code"](#materialized-in-code).

## The idea in one screen

The punchline the code embodies: **the focus combinators are mostly just `Strong` and `Choice`, relabeled to rows.**

- **`focusRecord`** and **`focusVariant`** are the row-typed `Strong`/`Choice` — they operate on rows on **both sides**, embedding a whole **sub-Record/sub-Variant** profunctor (`p { | f } { | f' }`) into a bigger row and carrying the complement. Each is a plain function over its positional original (`Strong p` / `Choice p`), so every such profunctor has them for free.
- **Product** (`Record`) combinators — `recordToProperty`, `eliminateProperty`, `property` — rest directly on `Strong` (`first`/`second` + insert/delete; `property` is the value-level single-field lens).
- **Sum** (`Variant`) combinators — `case_`, `caseToVariant` — rest directly on `Choice` (`left`; `case_` is the value-level single-case prism). There is one operation that *would* fall outside `Choice` — introducing a *fresh* case from a spontaneous source (a case the input never carries; see the rationale below) — but in this codebase that is built via the `VariantToVariant` composition path from sources that emit variants, not a dedicated focus combinator.
- A single-field/grow combinator is an **identity-pinned** merge; a merge is an **iterated** single-field combinator. Same values, two granularities — and since they share a row-kind, each row-kind's focus machinery sits in the same module as its merge class (`focusRecord` with `RecordToRecord`; `case_` with `VariantToVariant`).

See ["Materialized in code"](#materialized-in-code) for the module layout.

## What they share

| | |
|---|---|
| Type domain | Both build `p (X i) (Y o)` for `X, Y ∈ {Record, Variant}` |
| Row mechanics | Both use `RowToList`, `Prim.Row.{Cons,Union,Nub}`, and the constraints in `Data.Profunctor.Row` |
| Semantic role of types | `Record` = entity (product, all fields present at once); `Variant` = event channel (sum, mutually exclusive cases) |
| Final values | A given profunctor value inhabits the same type either way (modulo `p` having the requisite instances) |

A submit-form profunctor built via `RecordToRecord.do` and the same one built by chaining single-field combinators (`recordToProperty … >>> recordToProperty …`) are the *same inhabitant* of `p { … } { … }`. The two strategies are not two different theories — they are two different ways of writing one theory down.

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
    p { | i1 } { | o1 } ->
    p { | i2 } { | o2 } ->
    p { | i } { | o }
```

```purescript
-- Single-field combinator: unary lift of one value-source into one new field.
-- src/Data/Profunctor/Row/RecordToRecord.purs:96
recordToProperty
  :: forall @l p b s f
   . IsSymbol l
  => Cons l f b s      -- shot s = focus f at label l, against background b
  => Strong p
  => p { | b } f
  -> p { | b } { | s }
```

`recordToRecord` consumes two row-shaped arguments at once. `recordToProperty` consumes one sub-profunctor that reads the whole record `s` and threads its value into an accumulator that grows by one field per step.

### 2. Composition shape

- **Merge** is a **tree**: `recordToRecord (recordToRecord a b) c` and `recordToRecord a (recordToRecord b c)` are both valid foldings; associativity holds modulo the row-disjointness constraints (`ExclusiveRows` on outputs, `InclusiveRows` on inputs).
- **Single-field combinators** are a **list**: `recordToProperty @"a" pa >>> recordToProperty @"b" pb >>> recordToProperty @"c" pc`. Linear, order-driven, accumulates one cell per step.

### 3. Typeclass surface on `p`

- **Merge** needs the four classes `RecordToRecord`, `RecordToVariant`, `VariantToRecord`, `VariantToVariant`, each carrying the binary merge and its nullary unit `pempty` under a heavy row-constraint signature.
- **Single-field combinators** rest directly on `Strong`/`Choice`, so every such profunctor supports them for free — as do the *sub-row* focus functions `focusRecord`/`focusVariant`.

### 4. Type-inference cost

- Merge: each merge node solves a non-trivial row-union problem (`InclusiveRows` on input, `ExclusiveRows` on output). The inferrer must unify rows from both arguments. Error messages carry the full solved-row terms.
- Single-field combinators: each step solves only `Cons l a s t`. Smaller, more local constraint solving. Error messages stay short and field-local.

## The precise correspondence

For any `p` that supports both families, the two satisfy a fold/degeneracy duality at the value level.

**Merge combinators are folds of single-field combinators over a row-list.**
A binary `recordToRecord p1 p2 :: p { | i } { | o }` is equivalent to taking the row-list of the union output, and for each label `l : a` inserting an `recordToProperty @l` step into a chained pipeline, with `p1` and `p2` decomposed into their per-field constituents. The fold gives the same value as the binary merge.

**Single-field combinators are degenerate binary merges with identity.**
`recordToProperty @l q :: p { | s } { | t }` is the binary merge of identity (`p { | s } { | s }` doing nothing) with the single-field lift of `q`. The merge machinery reduces to "do nothing on the left, attach this one field on the right."

So **merge = iterated single-field, single-field = degenerate merge**.

At the **typeclass level**, neither implements the other polymorphically, because:

- Merge can't reach inside an opaque `p { | i1 } { | o1 }` argument to find its per-field atoms (the typeclass dictionary is parametric in the row shape, not in the row contents — there is no `RowList`-driven value-level recursion available without further machinery).
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

- **`recordToVariant` = Inclusive/Inclusive** — the **form → event** shape (read shared form, merge emitted events); the type of the business atoms (`p { … } [ … ]`).
- **`variantToRecord` = Exclusive/Exclusive** — the **event → display** shape (dispatch on which response occurred, fill disjoint fields).

### Only diagonals have focus

`recordToProperty @l f ≡ recordToRecord identity (rmap (\r -> {l: r}) f)` pins the **left operand to `identity`** — a `p a a`, which only typechecks when input and output are the **same kind**. The mixed classes' operands have *different* kinds (`p { … } [ … ]`), so **no `identity` can sit there**.

> Only the two **diagonal** classes admit `identity`, so only they collapse to a complement-carrying focus. The two **mixed** *merges* are **irreducibly binary** in that sense — crossing the product/sum boundary is exactly what an opaque business profunctor (e.g. a save-order action) does atomically, composed in with `>>>`. (But each mixed *direction* still has its **own** unary strength — a non-focus, mode-crossing one; see ["The mixed directions' own strength"](#the-mixed-directions-own-strength-resolve-and-retain).)

### Reshape vs focus: two axes, not a trio

The mixed kinds still admit *unary* reshapings (and their own mode-crossing strengths, ["below"](#the-mixed-directions-own-strength-resolve-and-retain)) — just not focuses. `Data.Profunctor.Row` — the shared floor of the row layer — exports the two widening reshapings the `UI` merge instances build on (`widenRecordInput`, `widenVariantOutput`); their narrowing duals and single-label `Cons`-pinned forms are pruned (all are `dimap` one-liners, reconstructible on demand). A both-sides reshape for a mixed shape is just a composition (`widenVariantOutput ∘ widenRecordInput` for `Record → Variant`) — pure `dimap`, no dedicated combinator needed. It is tempting to read `widen`/`narrow`/`focus` as a flat trio of analogue names; they are not. They sit on **two orthogonal axes**:

- **direction** — *widen* (grow, `f → s`) vs *narrow* (shrink, `s → f`).
- **complement** — *reshape* drops the complement (pure `dimap`, `Profunctor`-only) vs *focus* threads it across the input→output boundary (needs `Strong`/`Choice`).

| operation | direction | complement | strength |
|---|---|---|---|
| `focusRecord` / `focusVariant` | widen | **carried** | Strong / Choice |
| `Record → Variant` reshape (`widenVariantOutput ∘ widenRecordInput`) | widen | dropped | Profunctor |
| `Variant → Record` reshape (`narrowVariantInput ∘ narrowRecordOutput`) | narrow | dropped | Profunctor |

The tell: `focusRecord` is *itself* a widen-direction operation (`{ | f } → { | s }`, `s = f ∪ b`) — it merely *also* threads the background `b`. So the real contrast between `focusRecord` and the `Record → Variant` reshape is the **complement** column, not direction; `widen`/`narrow` are the genuine duals (one axis), and `focus` is a widen *plus* complement-threading (the other axis). This is exactly why the mixed kinds get only reshape: with input and output of different kinds there is no same-kind background to thread, so the product complement has no image in the sum one — sharpened to *unconditional vs gated* in the next section. Which free *direction* a mixed shape gets is then forced by variance — `Record → Variant` sits on the widen/widen side, `Variant → Record` on the narrow/narrow side, and each shape's *opposite* direction is the irreducible corner (needs fallback/defaults, collapsing to the binary merge above).

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

These two fabrications are themselves the product/sum dual pair: **fill** ↔ **collapse**. And inhabited case-introduction is the *degenerate unary case*: introducing into a sum needs the gate's input selector, but a spontaneous source supplies none, so even *with* a producer the case can never be emitted — whereas `second` (ungated) always emits its field, which is why `recordToProperty` exists and there is no `introduceCase` *in the diagonal directions*. (The `× → +` direction, where nothing else emits and the computed case fires unconditionally, does have it: `recordToCase` — plain `rmap (inj l)`, no strength needed.)

Note what stays free: **phantom**-widening a variant output (`widenVariantOutput`/`expand`) adds a case that is *never emitted* — no gate to satisfy — so it costs nothing. Only *inhabited* introduction is irreducible.

### The mixed directions' own strength: resolve and retain

Carrying a *same-kind* complement is what the mixed directions can't do — but each still has its **own** unary strength, one that threads the residual `c` *across* the product/sum boundary, letting it **change mode** (`×` on one side, `+` on the other):

| direction | strength (class / method) | shape | semantics |
|---|---|---|---|
| R→V | `Resolving` / `resolve` | `p a b -> p (a × c) (b + c)` | **loop / iteration** step — `Either b c` reads as `Done b`/`Loop c` |
| V→R | `Retaining` / `retain` | `p a b -> p (a + c) (b × c)` | **Mealy / coroutine** step — `Tuple b c` is output + next state |

(`Resolving`/`Retaining` are the **bare strengths**, the `× → +` / `+ → ×` analogues of `Strong`/`Choice`. Each has a **row-typed** focus function on top of it — `shutterWrap` and `reelWrap` — exactly as `focusRecord` sits on `Strong`; see the row existential constructors below.)

These are the product↔sum-crossing analogues of `Strong`/`Choice` — *not* focuses (they carry no same-kind complement) and *not* the merge. Neither has a `(->)` instance: a stateless function can't loop (`resolve` would be the trivial always-`Done`) or retain state (`retain`'s product output has no producer for the missing component). Their binary counterparts are the merges one level up — `resolve` is the identity-pinned form of the positional product→sum merge `p a b -> p c d -> p (Tuple a c) (Either b d)` (its second operand fixed to `identity`), and `retain` is the unary form of `variantToRecord`.

Each strength also **induces an optic** (its `p a b -> p s t` form, with the residual `c` eliminated by co-Yoneda) — the mixed-action cousins of the `Lens` (from `Strong`) and `Prism` (from `Choice`) the diagonals induce:

| strength | induced optic (constructor) | concrete form |
|---|---|---|
| `resolve` | **`Shutter`** (`shutter`) | `(view : s→a) × (build : b→t) × (escape : s→t)` |
| `retain` | **`Reel`** (`reel`) | `s → Either a (b→t)` |

A **`Shutter`** is a lens that can *snap shut* — run the focus and rebuild (`Done`), or `escape` straight to `t` (`Loop`/short-circuit); like a camera shutter that opens, loops while held, then snaps to one captured value. A **`Reel`** is a wound transport that *holds its position and never finishes* — each step emits an output and the next state; like a film reel you scroll through. `Shutter`/`shutter` live beside `resolve` in [RecordToVariant.purs](../src/Data/Profunctor/Row/RecordToVariant.purs); `Reel`/`reel` beside `retain` in [VariantToRecord.purs](../src/Data/Profunctor/Row/VariantToRecord.purs).

And they give the mixed directions the **`edit`-position single-field combinator** the diagonals have (`property`/`case_`) — here threading one label *across* the boundary instead of in place:

- `resolveProperty @l :: p { | b } [ | b' ] -> p { l ∷ f | b } [ l ∷ f | b' ]` — field `l` either escapes directly to output case `l` (`Loop`), or the wrapped profunctor runs on the background (`Done`).
- `retainCase @l :: p [ | b ] { | b' } -> p [ l ∷ f | b ] { l ∷ f | b' }` — input case `l` resumes into output field `l`; otherwise the wrapped profunctor runs on the background and `l` is filled from the carrier's retained state.

So the four directions are symmetric after all — each has a **merge**, a **unary strength**, and *two* `edit`-position single-label operations: **refocus** (transform the focus, background fixed) and **re-background** (transform the background, focus fixed). The diagonals name their refocusers — `property`/`case_`; their re-backgrounders need no dedicated combinator, being `focusRecord`/`focusVariant` at the singleton complement `(l ∷ f)`. The mixed directions name their re-backgrounders — `resolveProperty`/`retainCase`; their refocusers are `propertyToCase`/`caseToProperty`, which pay the wrapper label `@w`. Only the *kind* of strength differs: a complement-carrying **focus** on the diagonals, a mode-crossing **resolve/retain** on the mixed.

### The four optics as one square: three encodings

`Lens`, `Prism`, `Shutter`, `Reel` are one family — the 2×2 of a single existential template, one connective chosen on each side:

```
optic ≅ ∃c. (decon : s → a ⟨in⟩ c) × (recon : b ⟨out⟩ c → t)      ⟨in⟩, ⟨out⟩ ∈ { ×, + }
```

| | `recon : b × c → t` | `recon : b + c → t` |
|---|---|---|
| `decon : s → a × c` | **Lens** (`first`) | **Shutter** (`resolve`) |
| `decon : s → a + c` | **Reel** (`retain`) | **Prism** (`left`) |

The same optic admits **three interchangeable encodings**, bridged by (co-)Yoneda:

- **existential** `∃c. (decon) × (recon)` — the symmetric form above. The constructors `prismE`/`shutterE`/`reelE` (and the pruned `lensE`) are its eliminators (`forall c. (decon) -> (recon) -> _`), each literally `dimap decon recon (carrier g)` with `carrier ∈ {first, left, resolve, retain}`.
- **profunctor** `∀p. C p ⇒ p a b → p s t` — the type aliases (`Shutter`/`Reel`, and `Data.Lens`'s `Lens`/`Prism`); the symmetry lives entirely in the constraint `C ∈ {Strong, Choice, Resolving, Retaining}`.
- **explicit** — the co-Yoneda *collapse* of the existential at a fixed witness: `lens` / `prism` / `shutter view build escape` / `reel dispatch`, at `c := s` / `c := t` / `c := s` / `c := b → t` respectively.

**Symmetries** form a Klein four-group acting on the square: **F** flips `× ↔ +` (swaps Lens↔Prism and Shutter↔Reel); **T** reverses arrows / relabels `s↔t, a↔b` (fixes Lens and Prism, swaps Shutter↔Reel); **R = T∘F** is the standard optic op-duality (swaps Lens↔Prism, *fixes* the mixed pair — Shutter and Reel are each **self-dual**). The symmetry is manifest in the existential and profunctor encodings; the **explicit** collapse hides it, because co-Yoneda eliminates `c` on whichever side admits it (input-split for Lens/Shutter, output for Prism, output-curry for Reel) — which is also why `shutter` collapses to three maps and `reel` to one.

**Row existential constructors.** The residual `c` is the *rest of the input row*. `property @l` / `case_ @l` focus a single field/case and keep the complement same-kind (`c := { | rest }` / `[ | rest ]`). The mixed directions **wrap** the complement to cross the `×/+` boundary, focusing a whole sub-row exactly as `focusRecord`/`focusVariant` do: `shutterWrap @w` focuses a **sub-Record `i`** and sends the leftover `{ | rest }` into the output `Variant` as a single case `w`; `reelWrap @w` focuses a **sub-Variant `i`** and sends the leftover `[ | rest ]` into the output `Record` as a single field `w`. So the mixed directions *do* get a complement-carrying focus after all — refining ["Only diagonals have focus"](#only-diagonals-have-focus) above: not via a same-kind complement (no `first`/`left`), but via the strength plus a wrapper label `w`. These two are plain functions atop `Resolving` / `Retaining`, just as `focusRecord` is a plain function atop `Strong`.

The mixed directions admit **two** combinators that make opposite splits of the input row, and the names signal which:

| | focus | residual crossing the boundary |
|---|---|---|
| `resolveProperty @l` / `retainCase @l` | the **background** | the field/case **value** at `l` (threaded) |
| `shutterWrap` / `reelWrap` | the focus sub-Record / sub-Variant | the **background sub-row** (wrapped at `w`) |

So `resolve`/`retain` thread a **value**; the `*Wrap` pair wraps a **sub-row**. (`shutterWrap` focuses a sub-Record like `focusRecord`; `reelWrap` is its exact dual, focusing a sub-Variant like `focusVariant`.)

A worked `shutterWrap` (price an order line, carrying the unpriced remainder):

```purescript
-- focus the (item, qty) sub-Record; wrap the leftover field `note` as case `draft`
checkout
  :: Shutter
       { item :: String, qty :: Int, note :: String }    -- i'  full input
       [ priced :: Money, draft :: { note :: String } ]  -- o'  full output  (o + case `draft`)
       { item :: String, qty :: Int }                    -- i   sub-Record focus
       [ priced :: Money ]                               -- o   inner output
checkout = shutterWrap (Proxy @"draft")
```

`checkout` turns any inner `p { item, qty } [ priced :: Money ]` (which prices the focused part) into one over the full record: on `Done` the inner's `priced` case is `expand`ed through; on `Loop` the unprocessed remainder `{ note }` is injected into the `draft` case — the leftover is carried out, not dropped. The split `i' = i ⊎ rest` (`ExclusiveRows`) and the output extension `o' = o + (draft :: { | rest })` (`Cons`/`Union`) are all inferred; only the wrapper label `Proxy @"draft"` is the caller's choice (no `Cons`/`Union` fundep recovers a `Symbol` from the rows). The `Data.Lens` lens for the same `(item, qty)` focus would be `focusRecord` — `shutterWrap` is its `× → +` analogue, wrapping the complement instead of carrying it same-kind.

### In a UI: gestures and local state

For a profunctor UI like `UI m i o` — which pushes `i` to the screen and captures `o` from the user — these two strengths add exactly what the plain `Strong`/`Choice` form lacks. The threaded `c` becomes a **feedback loop inside the widget**: the next-state `c` it emits is fed straight back as its own input and re-renders, invisible to the parent. (A pure function has no such loop — the same reason neither class has a `(->)` instance.)

**`Resolving` = a transient gesture or flow that resolves.** The archetype is a **drag** — literally a loop with a start and an end:

```
mousedown        → start, state c = initial position
mousemove (×N)   → Loop c   (re-render the ghost at the new position, keep waiting)
mouseup          → Done b   (drop result — emit upward, loop ends)
```

The same `Loop … until Done` shape covers drawing a stroke (mousemoves accumulate the path), rubber-band selection, dragging a slider/splitter (commit on release), autocomplete (each keystroke re-queries; picking an entry finishes), and a modal wizard (`Next` loops, `Finish` is `Done`). The `c` is **ephemeral** — it exists only during the gesture and dies at `Done`.

**`Retaining` = persistent view-state the widget just keeps.** The archetype is **viewport state** — scroll offset, zoom, pan — which persists and updates with every event and never "finishes":

```
scroll / wheel / pinch → render at the new offset, carry (offset, zoom) onward as c
```

Likewise: expanded/collapsed tree nodes, the active tab, a carousel's slide index, window geometry you drag around, hover/selection highlight, a counter or cart total, an undo/redo stack. The `c` is **durable** — pure "how this widget currently looks," never part of the business model.

**Rule of thumb:** if the interaction *begins, runs, and resolves to a value*, it's `Resolving` (the `c` dies at `Done`); if it's *state the widget simply has and keeps updating*, it's `Retaining` (the `c` lives on). Both are the same `c`-feedback in `UI m` — differing only in whether a `Done` ever short-circuits out (a gesture's `mouseup`) or the loop runs indefinitely (the viewport never completes).

This is why bambik wants them: today every bit of state must live in the business model and thread through every parent, so a counter's count or a panel's expanded-flag leaks into the domain types. `Retaining` keeps that state **local** to the widget; `Resolving` lets a drag / wizard / "add-another" widget **own its loop** instead of exposing each intermediate step. The instances that deliver this — `Resolving (UI m)` / `Retaining (UI m)` in [UI.purs](../src/UI.purs) — wire the `c` feedback through `UI`'s `toUser`/`fromUser`.

## The optics as a domain model (DDD reading)

The split that runs through this whole note — *spatial data navigation* (the diagonals) vs *behavior over time* (the mixed directions) — is exactly Domain-Driven Design's **Value Object / Entity** line. The four optics map onto DDD's tactical vocabulary, and the type-level facts above become the domain rules:

| optic | encoding | DDD role |
|---|---|---|
| `Lens` | has-a (product field) | **Value Object** accessor |
| `Prism` | is-a (sum case) | **Value Object** discriminator |
| `Reel` | fold a command into state (`+→×`) | **Entity / Aggregate** |
| `Shutter` | run a step that finishes or loops (`×→+`) | **Process / Use Case / Saga** |

- **Diagonals = Value Objects.** `Lens`/`Prism` navigate frozen ADT structure: timeless, by-value, equal-by-structure, and they have a `(->)` instance. That is a Value Object precisely — no identity, defined wholly by its attributes, immutable (`set` returns a *new* whole).
- **`Reel` = Entity / Aggregate.** Its product output is *total* — every step yields a complete next state, so it never finishes: a thing that **persists**. The residual `c` is the entity's state/identity carried forward (held by the carrier, not the caller — matching DDD's "no reaching past the root"), and `recon` is the **aggregate root**, the single point that re-establishes invariants on every transition. The `Right c` "resume" branch of `retain` is **rehydration** from a snapshot. `cartReel`/`ledgerReel` in [test/BusinessOptics.purs](../test/BusinessOptics.purs) are aggregates folding commands into carried state.
- **`Shutter` = Process / Saga.** Its sum output is a *choice* — `Done`/`Loop` — so it can **terminate**: a process driving toward completion. The residual is its in-flight context, escaped on `Loop` (a saga's persisted state between steps). `verifyKyc` in [test/BusinessOptics.purs](../test/BusinessOptics.purs) is exactly this: `Verified` (`Done`) or `Pending partial` (`Loop`, carrying accumulated state).
- **The missing `(->)` instance is the entity/value-object line drawn in types.** A stateless value can navigate data (Value Object) but cannot carry identity over time (Entity/Process) — see ["The mixed directions' own strength"](#the-mixed-directions-own-strength-resolve-and-retain).

**Composition is the aggregate tree.** Optic composition is function composition, so aggregates nest by composing optics:

- a `Reel` whose focus is itself a `Reel` is a **sub-entity** — its residual is *local identity* nested inside the outer residual, reachable only by composing *through* the root;
- a `Reel` bottoming out in a pure `(->)` focus has a **value object** at the leaf (the recursion stops).

So an **aggregate** is a composed *stack* of optics, the **aggregate root** is its outermost `Reel`, and DDD's "external access only through the root" is precisely "you reach an inner optic only by composing through the outer one." (The Cybercat Institute's *Optics for UI* reaches the same Mealy machine by *parametrising the diagonal `Lens` with state* — the Para construction — rather than by promoting the off-diagonal optic; their explicit state parameter is this note's residual `c`, but threaded openly rather than encapsulated in the carrier. Their work covers the entity half; the `Shutter`/saga half, a sum-output process that can finish or loop, has no Para counterpart.)

### Business logic as a closed algebra

The four optics are not the *whole* of an application's logic — they are its **structural skeleton**. Everything else fills in around them *inside the same profunctor algebra*:

| concern | profunctor operation |
|---|---|
| structural navigation, state, process | the four strengths (`Strong`/`Choice`/`Resolving`/`Retaining`) |
| computation / arithmetic | `dimap`'s `decon`/`recon` — every optic is `dimap pre post (strength g)` |
| flow / orchestration | composition (`>>>`/`Semigroupoid.do` between stages; `synced` within one) |
| effects (DB, API, async) | the **carrier** — instantiating the polymorphic `p := UI m` |

The algebra is therefore **closed** over business logic: every pure step has a home (structure in the strengths, computation in `dimap`, flow in composition), and effects ride in the carrier. In particular the arithmetic is *not* outside the optics — it **is** their `decon`/`recon` (a cart's `total + line.price` is literally the `recon` of its `Reel`). Two things sit at the edge by design: **opaque pure functions** (a pricing engine is *carried* as a `rmap`/focus but not *decomposed* by optics) and **effect execution** (in the carrier — exactly DDD's domain/infrastructure boundary, which keeps effects out of the domain). The payoff is that the optic-expressed logic is **carrier-independent**: one definition runs unchanged in a live `UI m`, a pure test stepper, or a batch/server job.

## Introduce vs eliminate: each isolates one row-discipline

A full `recordToRecord` does two things: **decompose** its input (`InclusiveRows`) and **assemble** its output (`ExclusiveRows`). Each single-field combinator isolates exactly one:

- **introduce** = the **output-assembly** half (grow one field/case; input passes through).
- **eliminate** = the **input-decomposition** half (split off one field/case; output passes through).

Concretely, `eliminateProperty` rides the input split — `lcmap \s -> Tuple (get l s) (delete l s)`; case elimination (`caseToVariant` with a `Void`-output sink, `rmap absurd`) rides the input dispatch — `lcmap (on l Left Right)`. The split-off field/case is handed to a **sink whose output is discarded** — `p prop Unit` for `eliminateProperty` (the `Unit` is dropped via `snd`) and `p case Void` for the case eliminator. Those two sink-output types are the recurring `Unit`(terminal)/`Void`(initial) split: the product side *chooses* `Unit` (any type would do — `snd` throws it away — so it's pinned to `Unit` to make the discard explicit), while the sum side has `Void` *forced* on it (`left` routes the handled branch into the `Left` slot of `Either Void [ | t ]`, and only an uninhabited slot lets `either absurd identity` collapse it back to `[ | t ]`). The same sink-pinning completes the eliminate family across the mixed directions: `× → +` elimination is `propertyToCase @l @w` at a `Void` sink (case `l ∷ Void` can never fire; the background escapes wrapped at `w`), and `+ → ×` elimination is `caseToProperty @l @w` at a `Unit` sink (field `l ∷ Unit` records only that the case was consumed) — the product side again *choosing* `Unit`, the sum side again *forced* to `Void`.

This is what "single-field combinator = degenerate merge with identity" means concretely: `recordToProperty l f ≡ recordToRecord identity (rmap (\r -> {l: r}) f)`, with the one-field operand a genuine record-reading sub-profunctor (the `p { | s } prop` shape — it may read the whole record).

### The introduce quartet: one schema, five roles

The four introduce members share a single generating schema, read in the photographic register the library already inhabits (`Shutter`, `Reel`):

| role | letter | meaning |
|---|---|---|
| label | `l` | where the focus attaches |
| focus | `f` | the single value the wrapped profunctor is wired to |
| background | `b` | the shot's complement at `l` |
| shot | `s` | the grown row: `Cons l f b s` |
| reality | `r` | the row the camera is *pointed at* — the entire opposite side, wired to the wrapped profunctor verbatim |

**The schema**: each member grafts `g` between the focus `f` and the whole of reality; the label side grows from `f` to the shot `s`; reality passes to/from `g` untouched. (One level down, the existential constructors `lensE`/`prismE`/`shutterE`/`reelE` speak the ecosystem's `∃c` language — at every row instantiation their residual `c` *is* the background.) Every signature repeats exactly one row exactly twice — once inside `g`, once in the result — and that row is reality:

```purescript
recordToProperty :: … Cons l f b s => Strong p     => p { | b } f -> p { | b } { | s }   -- reality = b
caseToVariant    :: … Cons l f b s => Choice p     => p f [ | b ] -> p [ | s ] [ | b ]   -- reality = b
recordToCase     :: … Cons l f b s => Profunctor p => p { | r } f -> p { | r } [ | s ]   -- reality free
caseToRecord     :: … Cons l f b s => Retaining p  => p f { | r } -> p [ | s ] { | r }   -- reality free
```

**Background conservation** makes the diagonal/mixed split a theorem rather than a notational accident. On the diagonals, reality *must* coincide with the background: `recordToProperty`'s output record needs its background fields filled and only the input can supply them; `caseToVariant`'s non-focus input cases must land somewhere and only the output can receive them. Across the mode boundary there is nothing to conserve — reality is consumed whole (`recordToCase`) or produced whole (`caseToRecord`) — so it stays a free row. In the idiom: **on the diagonal, what the camera sees becomes the shot's background; across the boundary, reality never enters the frame.** The constraint column is priced by reality's fate: read it (`Strong`, via duplication), pass it (`Choice`, via branching), consume it (`Profunctor`, nothing survives), produce it without input (`Retaining`, replay from state).

Note the direction of specialization: the diagonals are the `reality := background` *instances* of the general five-role shape — sound, because instantiating a free variable loses nothing. Pinning the other way (forcing the mixed members' reality to equal `b` for the sake of surface symmetry) would assert a data flow that doesn't exist and force dead cases on downstream consumers; the free `r` is the visible scar of the mode crossing, and it is information.

## When to use which

Both strategies build the same values; pick by the granularity of the pieces you start from.

### Single-field style

The leaf level works one label at a time. In the living code this is the label-indexed components and the closed-singleton `field`:

```purescript
RecordToRecord.do
  MDC.filledTextField @"shortId" { floatingLabel: "Short ID" }
  MDC.filledTextField @"orderId" { floatingLabel: "Unique ID" }
  field @"customer" customerSubForm
```

(The algebra's growth-by-one-field form of the same idea is the pruned `recordToProperty @l … >>> recordToProperty @l' …` chain — one new field per step; it survives in this note as the derivation of the merge/single-field correspondence.)

This reads as "this record has these fields, one per line." It is the right style at the **leaf level** — when you start from atomic value-sources.

### Merge style — `src/Data/Profunctor/Row/Example.purs`

```purescript
-- src/Data/Profunctor/Row/Example.purs:102-108
recordToRecordExample :: MyRowToRowProfunctor
  { in1 :: MyData, in2 :: MyData, in3 :: MyData }
  { out1 :: MyData, out2 :: MyData, out3 :: MyData }
recordToRecordExample = RecordToRecord.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor { "in1" :: MyData } { "out1" :: MyData })
  (MyRowToRowProfunctor :: MyRowToRowProfunctor { "in1" :: MyData, "in2" :: MyData } { "out2" :: MyData })
  (MyRowToRowProfunctor :: MyRowToRowProfunctor { "in3" :: MyData } { "out3" :: MyData })
```

Each line is a *complete sub-profunctor with its own multi-field input and output row*. `RecordToRecord.do` merges them, solving `InclusiveRows` on inputs and `ExclusiveRows` on outputs.

This reads as "this record is the side-by-side combination of these pre-built sub-records." It is the right style at the **mid level** — when you have already-assembled row-shaped pieces and want to combine them.

### Mixing them

Idiomatic code uses **both** at different scales:

- Single-field combinators to build small records and variants from atomic value-sources.
- Merge to combine those pre-built sub-shapes into a larger composite.

The merge style shines once you have separate sub-records (say a customer block, a payment block, an address block) already built and want to glue them. Pick the granularity that matches the sentence you want to write.

## One-line summary

> Merge and single-field combinators are dual construction strategies for the same family of `Record`/`Variant`-shaped profunctors. Merge builds by **binary combination of complete sub-shapes**; single-field combinators build by **linear chaining of single-field atoms**. Both rest on the row-typed `Strong`/`Choice` focus classes (`focusRecord`/`focusVariant`): product focus carries its complement **unconditionally** (`first`), variant focus carries it **gated** (`left`/`right`), and the mixed kinds get no *focus* (no same-kind complement to thread) — instead each gains a mode-crossing strength, `resolve` (×→+, a loop step) and `retain` (+→×, a Mealy step), with single-field forms `resolveProperty`/`retainCase`. Neither family subsumes the other at the typeclass level, but their value-level denotations coincide, and a single-field combinator is an exact `identity`-pinned merge.

## Materialized in code

The repository implements this in `Data.Profunctor.Row.*` (pruned to demo-reachability — see the code-status note; pruned combinators below are marked):

- **`focusRecord`** (in [Row/RecordToRecord.purs](../src/Data/Profunctor/Row/RecordToRecord.purs), alongside `RecordToRecord`) — `Strong p => p { | f } { | f' } -> p { | s } { | s' }` (`ExclusiveRows f b s`, `ExclusiveRows f' b s'`), the row-typed `first`/`second`: splits the shot `s` into focus `f` and background `b`, runs the argument on `f` via `first`, and re-merges. Its variant dual `focusVariant` is pruned; the dispatch it used, `splitVariant`, survives in [Row/VariantToVariant.purs](../src/Data/Profunctor/Row/VariantToVariant.purs) (`reelWrap` shares it).
- **`Resolving`** (in [Row/RecordToVariant.purs](../src/Data/Profunctor/Row/RecordToVariant.purs), alongside `RecordToVariant`) — `class Profunctor p <= Resolving p` with `resolve :: p a b -> p (Tuple a c) (Either b c)`, the bare product→sum (×→+) strength. No `(->)` instance. The row focus function `shutterWrap` sits atop it.
- **`Retaining`** (in [Row/VariantToRecord.purs](../src/Data/Profunctor/Row/VariantToRecord.purs), alongside `VariantToRecord`) — `class Profunctor p <= Retaining p` with `retain :: p a b -> p (Either a c) (Tuple b c)`, the bare sum→product (+→×) strength. No `(->)` instance. The row focus function `reelWrap` sits atop it.
- **Single-field/case combinators** — living: `property` (on `Strong`; the value-level field lens) and `field` (its closed-singleton merge-operand form — on bare `Profunctor`: with an empty background `dimap` suffices, and its emissions are runtime-exact singletons, which the gates' left-biased `Record.union` requires of merge operands), `case_` via `prismE` (on `Choice`; the value-level case prism), `resolveProperty`/`propertyToCase` (on `Resolving`), `retainCase`/`caseToProperty`/`caseToRecord` (on `Retaining`; `caseToRecord` is the Mealy reducer — case `l` updates the record, other cases replay it), `recordToCase` (×→+ introduce, mere `Profunctor` — the ungated emission). Pruned: `recordToProperty`/`eliminateProperty` (×→× grow/drop), `caseToVariant` (+→+ absorb), `lensE`, `withRecordDefault(s)`. All row combinators are plain functions over the strengths; the mixed strengths' instances live on a genuinely stateful carrier — `Resolving (UI m)` / `Retaining (UI m)` (there is still no `(->)` instance: a pure function can't loop or retain state).
- **Case-introduction** — injecting a *fresh* variant case (the one operation outside `Choice`, see the rationale above) exists only where nothing gates it: `recordToCase` on the ×→+ direction.
- **Merge classes** — `RecordToRecord`/`RecordToVariant`/`VariantToRecord`/`VariantToVariant`, each with its nullary unit `pempty` and qualified-do sugar; the mixed direction modules additionally host their unary strength classes.
- **Tests**: [test/Main.purs](../test/Main.purs) exercises the diagonals on `(->)` — `focusRecord`/`property`/`recordToCase` — plus the merge unit laws and knowledge-gating on the `UI` carrier via a probe harness; [test/BusinessOptics.purs](../test/BusinessOptics.purs), [test/RestaurantReel.purs](../test/RestaurantReel.purs), [test/EntityEventExample.purs](../test/EntityEventExample.purs) and [test/HelloShutterReel.purs](../test/HelloShutterReel.purs) exercise `Shutter`/`Reel`.

## References

Source locations cited in this document:

- Merge classes (each with `pempty` and qualified-do):
  - `src/Data/Profunctor/Row/RecordToRecord.purs`
  - `src/Data/Profunctor/Row/RecordToVariant.purs`
  - `src/Data/Profunctor/Row/VariantToRecord.purs`
  - `src/Data/Profunctor/Row/VariantToVariant.purs`
- Merge examples: `src/Data/Profunctor/Row/Example.purs` (phantom carrier) + `showcase/App.purs` (four-direction pipeline)
- Row strengths and their combinators (each beside its merge):
  - `RecordToRecord.purs` — `focusRecord` (on `Strong`); `property` (on `Strong`) / `field` (on bare `Profunctor`)
  - `VariantToVariant.purs` — `case_` (on `Choice`, via `prismE`); `splitVariant`
  - `RecordToVariant.purs` — bare `class Resolving`/`resolve` + row focus `shutterWrap`; `resolveProperty`/`propertyToCase`; `recordToCase`; induced optic `Shutter`/`shutter`; existential constructor `shutterE`
  - `VariantToRecord.purs` — bare `class Retaining`/`retain` + row focus `reelWrap`; `retainCase`/`caseToProperty`/`caseToRecord`; induced optic `Reel`/`reel`; existential constructor `reelE`
- Unary row reshapings (the two widenings): `Data.Profunctor.Row` — see "Reshape vs focus"
- Base product↔sum binary counterpart of `resolve`: `p a b -> p c d -> p (Tuple a c) (Either b d)` — cited in `resolve`'s docstring; no longer a module of its own
- Row constraints (`InclusiveRows`/`ExclusiveRows`/`DispatchableVariants`): `src/Data/Profunctor/Row.purs`
