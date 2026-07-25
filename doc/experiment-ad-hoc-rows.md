# Experiment: demos without nominal types — ad-hoc rows only

Every `type` synonym and (almost) every `data` declaration was removed from all
demos (demo/1, demo/7guis, demo/nguis). Synonyms were inlined at every use
site, transitively; sum types became anonymous variant rows via the fork's
sugar (`[ oneWay :: Unit, return :: Unit ]`, `.oneWay unit`,
`Data.Variant.match`). Behavior is unchanged: full build, `spago test`, all
bundles, and the whole smoke suite pass.

## What survived

Exactly one declaration in ~3,000 demo lines:

```purescript
data Expr                    -- cells' formula AST
  = Lit Number | Ref String | Range String String | App String (Array Expr) | Bin Char Expr Expr
```

`Expr` is directly recursive, and rows cannot express μ — a hard structural
boundary, not a style choice. Everything else went, including
markdown-previewer's `Block`/`Inline` (they looked recursive but aren't:
`Block` → `Inline` → nothing, no cycle).

## Source size

| metric | before | after |
|---|---|---|
| total demo LOC | 3,053 | 2,913 (−4.6%) |
| total chars | 118,984 | 142,860 (+20.1%) |
| non-whitespace chars | 92,108 | 110,769 (+20.3%) |
| diff | — | 30 files, +444/−583 |
| longest line | 302 ch | 735 ch |
| p99 line width | 160 ch | 282 ch |
| p95 line width | 103 ch | 133 ch |
| lines > 120 ch | 89 | 196 |

Line count *drops* (every declaration block vanished) while the code *grows a
fifth in characters*: the type text didn't disappear, it moved from one
declaration into every signature that used the name, multiplied by the
use-site count. Chars, not lines, are the honest size metric here. The distribution per file is bimodal:

- shrinks when the type had few consumers: movie-browser −28, weather −19,
  quiz/checkout −15, todomvc −14
- grows when a big row has many consumers: **demo/1 +57** — the 22-line
  `Order` row is now pasted verbatim into 4 signatures, ~88 lines of repeated
  type; stopwatch +8, markdown-previewer +8, reorder +4

Rule of thumb that fell out of the data: cost ≈ row size × use-site count.
Below roughly 3 uses × 3 fields the synonym was pure ceremony; above it, it
was earning its keep.

## Type inference

**Zero annotations had to be added in the entire conversion, and zero
inference failures occurred.** Every file compiled on the first attempt after
mechanical substitution. PureScript synonyms are transparent, so removal is
semantically free; and the demos' business functions were already fully
annotated at top level, so inference never had to reconstruct a row from
scratch. Variant construction (`.all unit` open injections) closed against
`mvu` seeds' concrete signatures without help; `select`/`segmentedButton`'s
`Eq` constraints resolved through Variant's structural `Eq` over `Unit`
payloads, so every `derive instance Eq` was simply deleted.

The two *pre-existing* annotations that inference genuinely requires are where
the style hurts most, because they live **inside expressions**, where a long
row can't be formatted away:

- circle-drawer's `foreach` lambda: `\(m :: Canvas) ->` became a ~280-char
  in-expression annotation on the densest line of the file
- inbox's mid-pipeline echo wire: `(identity :: PUI Web Mailbox Mailbox)`
  became ~330 chars, the row spelled twice

Signatures can go multi-line; in-expression annotations can't, and they are
exactly the positions the row layer sometimes needs pinned.

## Type signatures

Representative growth (checkout):

```purescript
atCart :: Step -> Maybe Step
-- became
atCart :: { item :: String, address :: String, card :: String, confirmation :: String, step :: String }
       -> Maybe { item :: String, address :: String, card :: String, confirmation :: String, step :: String }
```

repeated near-identically for 11 helpers. Nesting compounds transitively:
shopping-cart's `addUnit :: Product -> Cart -> Cart` (28 ch) became 250 ch
with `Product` inlined twice inside `Cart`; markdown-previewer's
`blocks :: Array String -> Array Block` became a 6-line type in which the
`Inline` row appears four times. The update-helper idiom `X -> Model -> Model`
— the backbone of every MVU demo — is the worst-hit shape, since `Model`
appears twice per signature and the helpers come in packs of 5–11.

Two genuine signature *improvements*: single-field models
(`Queue = { serving :: Int }`, `Auction = { bid :: Number }`) read better
inline — ticket-dispenser's `unfolding` signature now shows both trace
channels concretely; and quiz's `{ question :: Int, correct :: Int }` says
more than `QuizRun` did.

## Readability

Better or neutral (small/flat models): auction, ticket-dispenser,
tic-tac-toe, photo-gallery, tip-calculator, color-mixer, payment,
restaurant-menu (its synonyms were pure documentation — the widget was
already structurally typed).

Clearly worse (large or nested or many-consumer models): demo/1, inbox,
checkout, crud, stopwatch, calculator, password-generator, shopping-cart,
weather, markdown-previewer's parser half. Three distinct losses:

1. **Documentation-of-role.** `Canvas`, `PeopleCatalogue`, `Bill` named what a
   row *is for*; checkout's `Order` vs `Step` distinguished model from
   model+loop-state in the `folding @"next"` wizard — now you diff two 4- vs
   5-field literals to find the looped field. Aff actions no longer say what
   they load (`loadOrder :: {22 lines} -> Aff ...`).
2. **Single point of change.** Adding a field to a model now means editing
   every signature in lockstep — ~10 sites per file in crud/stopwatch/inbox,
   7 in password-generator, 4×22 lines in demo/1.
3. **Sameness.** Structurally equal rows with different roles become
   indistinguishable (payment's model vs its retry row).

Genuine wins on the variant side: enum ADTs convert beautifully.
`data Visibility = All | Active | Completed` + `derive instance Eq` became a
row that needs no derive, whose labels feed `match` records
(`display = match { numV: formatNum, textV: \t -> t, errV: \e -> e }` is
tighter than three pattern equations), and whose construction `.all unit`
reads fine (though there's no niladic sugar — the `unit` payload is
mandatory). Multi-argument constructors force named payload records
(`Heading Int (Array Inline)` → `.heading { level, inlines }`) — bulk in the
type, but a readability *gain* at construction sites.

One semantic regression: the fork's variant *pattern* sugar requires a `_`
catch-all, so cells' `numAt` — previously compiler-checked exhaustive over 4
constructors — gained a dead `_ -> Left "#REF!"` branch that would silently
absorb a future case. `Data.Variant.match` stays total and avoids this, but
doesn't do multi-scrutinee or literal-payload patterns.

## Developer experience

- Conversion itself is risk-free: zero compile errors across 30 files, no
  row-layer or variant-sugar friction. The library's structural spine
  (merges, `mvu`, `edits`, `feedback`, `foreach`) never referenced the names
  anyway — **the UI-pipeline code was untouched in every file**; nominal types
  lived exclusively in the business layer.
- Error messages and IDE hovers now print full rows instead of names — fine at
  3 fields, walls at demo/1 scale.
- Refactoring cost inverts: renaming a *type* becomes free (there is none),
  renaming a *field* touches every pasted copy.

## Verdict

Nominal types in bambik demos are pure business-layer documentation — the
profunctor/row machinery is already 100% structural and doesn't miss them.
Removing them costs nothing in inference or compile-time and −4.6% in line
count, but pays for it in signature width (p99 +76%), duplicated
single-point-of-change, and lost role names. The data supports a split
convention rather than either extreme, which is what CLAUDE.md already
prescribes: **structural as far as readable — anonymous rows for small/
single-use shapes, a named alias only for the top aggregate** (and `data`
where there's recursion). The experiment confirms the existing convention is
at the optimum; the all-ad-hoc extreme is viable but strictly worse for the
larger demos.

## Follow-up audit: unnecessary row members after inlining

A second pass checked every inlined signature for row members the function
body never needs. Headline: **inlining created no new slack — but it exposed
a lot of pre-existing slack.** At the parent commit every flagged helper was
typed `Model -> Model` / `Model -> String` behind a synonym naming the full
model; demanding the whole model cost one cheap word, so nobody noticed.
Ad-hoc rows re-price that habit per field per paste site: a large share of
the signature-width explosion measured above is *over-demand*, not necessary
type text.

### Where the exact row is genuinely forced

Consistent across all 30 files: `mvu`/`with` seed literals, `announce`/
`seeded` primers, constructed record literals (loaders like demo/1's
`loadOrder`, state-bracket constructors like `fulfillmentState`), merge
operands and `forCase` positions, and inbox's mid-pipeline
`identity :: PUI Web {model} {model}` type anchor. Everything else —
projections under `lcmap`/`projection`, `updates`/`match` handlers,
`provided` panes, `every` steps — only has to *unify* with the
pipeline-pinned model, and open rows (`forall r. { used :: T | r } -> ...`)
unify fine. The pipeline pins the concrete model at exactly one place (the
merge or the seed); helpers never need to repeat it.

### Magnitude of the slack

Dead weight concentrates in the "pack of small helpers over one model" files:

| file | share of helper-signature row text that is carried, never read |
|---|---|
| checkout | ~70% (9 of 11 helpers read only `step` + at most one field) |
| inbox, weather | ~two-thirds (most helpers read 1–2 of 4–6 leaf fields) |
| calculator | 60–80% in the read helpers (`pressKey` alone earns the full row) |
| stopwatch | ~50% |
| demo/1 | of 4 full-Order pastes only `loadOrder` earns it; `printReceipt` needs 2 of 7 top fields, `submitOrder` 3, `summarize` 5 |
| crud | ~17 of 45 member occurrences carried; plus a dead `surname` in the `Entry` row that flows into `listOf` unread |
| signup-form | ~40% — `plan`/`country` are edited and seeded but never consulted by any business function |
| tip-calculator | 6 of 8 row-typed values over-demand (only `perPersonLine` and the seed need the whole `Bill`) |

Clean (no or trivial slack): temperature-converter, shopping-cart, reorder,
tic-tac-toe, markdown-previewer, photo-gallery, restaurant-menu, and
near-clean auction, ticket-dispenser, payment, color-mixer, cells.
**No unused variant cases exist anywhere** — every case of every converted
variant is both constructed and consumed.

Extreme cases: quiz's `restart` and inbox's `emptiedNote` declare the full
model and ignore their argument entirely (`forall a. a -> ...` would do).
Incidental finds: `demo/7guis/flight-booker/Business.purs` is imported by
nothing (orphaned module); circle-drawer's `undo`/`redo` don't restore
`diameter` — visible in the audit precisely because the field sits unused in
their signatures.

### What this does to the verdict

The width explosion blamed on ad-hoc rows above is really two costs
superimposed: necessary row text (the model spelled where it's pinned) and
over-demand (full model where 1–3 fields suffice). The second dominates in
the worst files. The synonym style *hid* the over-demand; the ad-hoc style
*charges* for it — and, unlike the synonym style, makes the honest narrow
signature expressible per helper. A disciplined ad-hoc variant of the demos
(open rows for helpers, exact rows only at seeds/literals/anchors) would be
substantially less verbose than this branch, at the cost of `forall r.`
noise and weaker "this is the model" documentation. The optimum remains the
existing convention, but with one refinement the audit argues for: helper
signatures need not name the full model even when a synonym exists — the
full-model `Model -> Model` habit is a documentation choice, not a
requirement of the row machinery.

## Third state: unused row members deleted (narrowed ad-hoc)

The audit's narrowings were then applied: every over-demanding helper was
retyped row-polymorphically (`forall r. { used :: T | r } -> ...`), with
exact rows kept at seeds, primers, constructed literals, and merge operands.
Verified: full build, `spago test`, demo/1, all bundles, smoke suite green.

| metric | nominal (main) | full-row ad-hoc | narrowed ad-hoc |
|---|---|---|---|
| lines | 3,053 | 2,913 | 2,915 |
| chars | 118,984 | 142,860 (+20.1%) | 136,162 (+14.4%) |
| non-whitespace chars | 92,108 | 110,769 | 105,011 |
| longest line | 302 | 735 | 591 |
| p99 line width | 160 | 282 | 207 |
| lines > 120 ch | 89 | 196 | 167 |

Narrowing clawed back only ~28% of the char growth (−6,698 of +23,876).
Two reasons: `forall r rs.` row-variable ceremony costs chars back, and —
the substantive finding — **14 of ~64 attempted narrowings would not
compile** and had to stay full-row.

### The pinning perimeter (found empirically, consistent across all files)

Open rows are accepted wherever the model flows through `dimap`-plumbing:
`lcmap`/`projection` as pipeline stages, `tapped`, `updates`/`match`
folders, `provided` panes, `every` steps, `action` mid-pipeline, even a
pass-through feeding a dialog (upstream of a closed anchor, open is fine).

Open rows are rejected — `No type class instance ... Prim.Row.Union t0 t2
(...) / instance head contains unknown type variables` — wherever the
function's row is a **row-merge's evidence input**:

- an operand of a `.do` merge (`projection f` inside `RecordToRecord.do`,
  a status line between buttons in `RecordToVariant.do`) — the merge's
  `SharedRecordInputs` union must solve locally over the operands' rows,
  and one open tail leaves it groundless, *even though* the enclosing
  pipeline (`mvu`/`folding`) fully determines the row;
- anything reaching a merge transitively through `asCase` payload replay
  (demo/1's `submitOrder`/`printReceipt` — the button replays the record as
  case payload, and the `+→+` dispatch behind it pins the row);
- a feed of `completed` (its `Union n nx i`/`Nub` evidence needs the ground
  input row).

So the rule the compiler enforces: *the function typed at a merge operand's
own input must be closed; upstream of a closed anchor, open is fine.* The
boundary is invisible in the source — whether `| r` compiles encodes a
combinator fact (merge operand vs pipeline stage) the reader must know.

### Readability of the third state

Unanimous across the sweep: narrowed beats full-row ad-hoc everywhere it
lands — `keepMessages :: forall r. { confirming :: Boolean | r } -> ...`
states the whole contract where the old signature was a 230-char wall, and
the dead-field discoveries (`password` unread by `samplePassword`, `faulty`
by `settle`, undo/redo not restoring `diameter`) are visible in the types.
Against nominal it splits: for flat models and small footprints the open row
is *more* informative than an alias (`{ amount :: Number | r }` says more
than `Order -> String`); where a nested entity row recurs (inbox's message
row) or a helper touches nearly the whole model, the alias still reads best
— and the pinned survivors keep the full anonymous row spelled out, so each
file ends up with two signature styles whose difference encodes the merge
perimeter.

### Amended verdict

"Unused rows before vs after" has a three-part answer: nominal style hides
over-demand (zero cost, zero visibility), full-row ad-hoc charges maximally
for it, narrowed ad-hoc eliminates what the merge perimeter allows (~28% of
the growth) and turns the rest into an explicit, compiler-enforced map of
which functions sit on merge evidence. The narrowed state is the best
ad-hoc variant but still +14% chars over nominal; the synonym's remaining,
irreducible value is naming the model at the pinned positions the row
machinery genuinely forces to stay ground.

## Fourth state: closed narrow rows (library taught to subsume)

The third state's `forall r.` ceremony was then attacked at the source: can the
*library* absorb the widening so business functions can be closed narrow rows
with no row variables at all? Mostly yes. Two combinators were added to
`PUI.purs` (36 lines):

- `updatesOn` — `updates` whose handler is `e -> {|small} -> {|small}`, the
  result merged back over the retained model (`Union small big u, Nub u big`).
- `everyOn` — the same for `every`'s tick step.

Everything else needed no new library code: `widenRecordInput` (already
exported) covers read-stages, and `completed` and the merges already widen.

Result: **9 `forall`s left in all demos**, down from 89, and every survivor is
principled — 5 true pass-throughs whose row variable appears in the *result*
(`atCart`-style `{ step :: String | r } -> Maybe { step :: String | r }`,
stopwatch's `whenHalted`/`whenRunning`, inbox's `confirmingDelete`) and 4
genuinely parametric ignored arguments (`forall a. a -> ...`). Verified: build,
`spago test`, demo/1, all bundles, smoke suite green.

| metric | nominal | full-row ad-hoc | narrowed (open rows) | closed narrow |
|---|---|---|---|---|
| lines | 3,053 | 2,913 | 2,915 | 2,881 |
| chars | 118,984 | 142,860 | 136,162 | 138,106 |
| p99 line width | 160 | 282 | 207 | 242 |
| lines > 120 ch | 89 | 196 | 167 | 173 |
| `forall` in demos | 0 | 0 | 89 | 9 |

Closed rows are *fewer lines* than every other state but slightly **more
chars** than the open-row state: dropping `forall r.` binders is cheaper than
what closing costs — nested rows must be spelled out in full (widening is
top-row only, so weather's report row and movie-browser's movie row appear
verbatim in every signature), plus the `# widenRecordInput` stages and the
rule-6 call-site literals.

### The position taxonomy (the real deliverable)

Whether a closed narrow row compiles depends entirely on *which combinator
consumes it* — five distinct behaviors, established by construction:

| position | closed narrow row |
|---|---|
| `# completed` | **works directly** — and adding `widenRecordInput` *breaks* it (two coercions ⇒ ambiguous row) |
| `.do`-merge operand | works **iff** the operands' union already equals the flowing row (typically a sibling pins it); otherwise ONE `# widenRecordInput` on the whole merge |
| `# tapped`/`# displayed`/`lcmap` feed | needs one `# widenRecordInput` between the narrow stage and the consumer |
| `updates`/`every` handler | needs `updatesOn`/`everyOn` — *unless* the handler is the only thing pinning its sibling's replayed payload |
| plain application (`attrWith`, `action`, helper→helper) | **no subsumption ever** — caller passes an explicit literal |

Two traps worth recording. First, `SharedRecordInputs` fixes a merge's input
to `Nub (i1 ∪ i2 ∪ …)`, so narrowing *all* operands of a merge shrinks its
input below the model row (crud's three Aff actions, checkout's five button
feeds, inbox's three display lines all hit this) — the fix is one widen
outside the merge, never per-operand. Second, the `const <<< f` idiom in a
`match` handler is often the *only* thing pinning the sibling widgets'
replayed payload: rewriting it to `\_ -> f` (needed when `f` is narrow)
un-pins the merge and fails, so stopwatch's and inbox's update packs stay at
the full model row — closed, but not narrow.

### Costs that are not type text

Narrowing forced two genuine code changes, not just signature changes:
circle-drawer's `pushUndo` now returns only `{ undoStack, redoStack }` with
callers splicing the stacks back into their record updates, and
`updatesOn`-style handlers under one `match` must share a footprint — so
circle-drawer's `undo`/`redo` widened back to the whole model because their
sibling reads `diameter`. Neither is a readability win.

### Amended verdict

The library *can* support fully closed, `forall`-free narrow signatures — it
took 36 lines and no changes to the row layer. The result is the shortest
state by lines, states each function's exact dependency footprint, and needs
no row-polymorphism knowledge from the reader. What it costs is a taxonomy:
five consumer positions with five different widening rules, invisible in the
source, plus explicit literals wherever plain application is involved. So the
end state of the whole experiment is not "ad-hoc rows are worse" but a sharper
trade: nominal aliases buy *one name per model and no positional rules*;
closed ad-hoc rows buy *per-function honesty* at the price of knowing where
the library widens. Both beat the middle state (full-row ad-hoc), which pays
the width and hides the over-demand.

## Fifth state: subsumption baked into the stages (`widenRecordInput` withdrawn)

The fourth state's taxonomy was the complaint: five consumer positions with
five different widening rules, all invisible in the source, and 39 hand-written
`# widenRecordInput` stages carrying them. The fifth state asks the obvious
follow-up — can the *wrapping combinators* absorb the widening, so that
`widenRecordInput` disappears from the vocabulary demos can reach?

Almost entirely. Six combinators now carry `Union narrow extra big` and widen
their argument internally, and the two `…On` siblings are gone (folded into the
names they were siblings of, so there is **one name per concept** again):

| combinator | what subsumes |
|---|---|
| `tapped`, `displayed` | the display reads a sub-row of the stage's row |
| `updates` | *both* the handler's sub-row **and** the wrapped event widget's row (this absorbed `updatesOn`) |
| `every` | the tick step's sub-row (absorbed `everyOn`) |
| `edits`, `acted` | the element widget reads a sub-row of the array's element row |

`updates`/`every`'s type-agnostic implementations survive as private `mealy`
and `heartbeat` — the public stages are row-typed, because that is where the
subsumption is stated. `widenRecordInput` is no longer re-exported from `PUI`:
it stays exported from `Data.Profunctor.Row` as the merge instances' plumbing,
so reaching it from a demo now takes a deliberate second import.

### Where subsumption must NOT be baked in (measured, not assumed)

Baking it into three further combinators was tried and reverted, each for a
distinct structural reason:

- **`foreach`** — an element widget's row is *inferred from the array*, not
  stated (elements are built from accessors: `text # projection _.key`).
  Subsumption decouples the two and leaves the row unknown: calculator, cells
  and color-mixer all failed. Element rows stay exact.
- **`provided`** — the pane is *fed* the payload, so the pane row and the
  `Maybe`-projection's result row are the same thing by construction. Widening
  it broke quiz's inner `completed` (two chained subsumptions ⇒ ambiguous row).
  Reverted; instead each pane projection now returns exactly what the pane
  reads, which **closed three `forall r.`s** in checkout.
- **`onCase`** — a case *payload* is pinned by the action that consumes it as
  often as by the widget that emits it. Widening it broke five sites (crud,
  inbox, password-generator, reorder, weather).

The rule that falls out: **subsumption belongs where a row is only ever
*read* against a row the stage already carries** (the ×-diagonal stages, the
container element). It must not go where the row is *inferred through* the
combinator — payload boundaries (`provided`, `onCase`, case payloads) and
containers whose element row comes from the array.

### The residue

`widenRecordInput` now appears **once** in all of demo/ — demo/1's `×→+`
button merge, whose consumer is a `+→+` dispatch stage rather than `updates`,
so no subsuming stage sits at that boundary. The alternative (spelling the
whole order row into `submitOrder`/`printReceipt`, or into a new projection)
costs ~17 lines of row text to save one import, so the widen stays as the
honest marker of a structurally uncovered position.

Payload exactness also charges four signatures for fields they never read:
weather's `fetchReport` (`shown`), quiz's `questionChoices` (`prompt`), and
signup-form's `register`/`validate`/`validationSummary` (`plan`, `country` —
the `button` replays its whole fed row and ×→+ has no subsuming adopter).

### What the sites became

Every failure the bake introduced had a *better* spelling available, which is
the strongest evidence that this is the right factoring:

| before | after |
|---|---|
| `text # projection _.field # tapped` | `text # forValue # forField @"field" # tapped` |
| `text # lcmap (\m -> { value: readout { faulty: m.faulty, entry: m.entry } })` | `text # projection readout` (calculator — the phase-4 literal splice dissolved) |
| `action (\r -> samplePassword { length: r.length, … })` | `action samplePassword` (same, in password-generator) |
| `updates (match { clicked: \_ -> addTodo })` | `updates (match { clicked: const <<< addTodo })` — pins the event widget's replayed payload to the handler's row |
| `(identity :: PUI Web { messages :: …, opened :: …, confirming :: …, nextId :: … } { … })` | `identity` (inbox — a ~200-char annotation that is simply no longer needed) |
| `lcmap (\(m :: { circles, selected, diameter, adjusting, undoStack, redoStack }) -> …)` | `lcmap canvasCircles` with `canvasCircles :: { circles, selected } -> Array …` (circle-drawer — ~370 chars of annotation stating six fields to read two) |

Two demos changed shape rather than types: potluck's model became
`{ guests :: Array … }` lifted with `# field @"guests"` (a bare-`Array` carrier
can host no `tapped`/`displayed` stage now that those are record-typed), and
shopping-cart's leading `constantly productCatalogue` became
`lcmap productCatalogue` with `productCatalogue :: {} -> Array …`, because a
*first* pipeline stage has no neighbour to pin `constantly`'s free input.

| metric | nominal | full-row ad-hoc | narrowed (open rows) | closed narrow | stages subsume |
|---|---|---|---|---|---|
| lines | 2,990 | 2,856 | 2,858 | 2,824 | 2,839 |
| chars | 116,935 | 140,637 | 133,939 | 135,883 | 135,537 |
| p99 line width | 161 | 282 | 211 | 242 | 241 |
| lines > 120 ch | 89 | 195 | 166 | 172 | 172 |
| `forall` in demos | 0 | 0 | 89 | 9 | **2** |
| `widenRecordInput` lines | 7 | 7 | 7 | 58 | **2** |

(All states measured over the same file set, excluding the orphaned
`flight-booker/Business.purs`, which this state deleted — it was dead code
fully superseded by `FlightBooker.purs`. The earlier tables in this document
included it, hence the small differences.)

The two surviving `forall`s are genuinely parametric ignored arguments
(`restart :: forall a. a -> …`, `nextTicket :: forall a. Tuple a … -> …`) —
**no row polymorphism is left in any demo**, and no demo names a type.

### Verdict

The taxonomy was not inherent to ad-hoc rows; four of its five rows were a
missing library feature. With subsumption in the stages, a business function
states exactly the fields it reads, as a closed anonymous row, and the
pipeline accepts it — no row variables, no coercion stages, no positional
rules to remember for the ×-diagonal. What remains is a much smaller and more
teachable rule: **rows are read narrow, payloads are exact.** The cost of
ad-hoc rows over nominal aliases is now purely the width of repeated nested
rows (+16% chars, p99 242 vs 161), not ceremony.

Verified at this state: `spago build`, `spago test`, demo/1, all bundles, and
the full `npm run smoke` suite green.
