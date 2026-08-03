# Bambik vs other frameworks on the 7GUIs benchmark

A measured comparison of code size across implementations of the seven
[7GUIs](https://eugenkiss.github.io/7guis/) tasks — counter, temperature
converter, flight booker, timer, CRUD, circle drawer, cells — in bambik and
four reference frameworks. All numbers are measured from real corpora, not
estimated.

## Corpora and methodology

| Corpus | Source | Notes |
|---|---|---|
| **Bambik** | `demo/7guis/*-mdc2/` in this repo | PureScript, MDC2 components, deployed at erykciepiela.xyz |
| **Elm** | [dwayne/elm-7guis](https://github.com/dwayne/elm-7guis) | The most thorough community Elm version |
| **React + MobX + TypeScript** | [eugenkiss/7guis-React-TypeScript-MobX](https://github.com/eugenkiss/7guis-React-TypeScript-MobX) | By the 7GUIs author himself |
| **Vue 3** | Official Vue docs examples (Composition API: `composition.js` + `template.html`) | Written by the Vue team |
| **Svelte** | [joysofcode/svelte-7GUIs](https://github.com/joysofcode/svelte-7GUIs) | |

Counting rules, applied identically to every corpus:

- blank and comment lines stripped;
- **every** per-task source file counted, business modules included — shared
  framework/runtime code excluded on all sides;
- CSS excluded (Vue's stylesheet files, Svelte's `<style>` blocks);
- tokens counted with the cl100k BPE (gpt-tokenizer) — a proxy for LLM
  generation/reading cost;
- characters counted on the same stripped text — a proxy for raw
  typing/reading volume.

The bambik side is reproducible: `scripts/count-7guis.mjs` applies these
rules and prints the per-file and total figures in the tables below.

```sh
npm i --no-save gpt-tokenizer
node scripts/count-7guis.mjs demo/7guis/*-mdc2/*.purs demo/7guis/*/*Logic.purs
```

The July measurement left no script behind, which is why its bambik numbers
could not be reproduced exactly (see below) — this one can.

**On the previous revision's bambik numbers.** This table supersedes a July
2026 measurement that reported 7 040 tokens. Two things changed. The
smaller: that count took flight-booker's `FlightBooker.purs` and dropped the
`Business.purs` beside it, understating the task by 565 tokens against its
own stated rule that task-specific helpers are included; re-measured on a
consistent all-files basis, the July tree was **7 592**, not 7 040. The
larger: the demos themselves grew. Comparing like with like, bambik's 7GUIs
corpus went 7 592 → 9 856 tokens (+30%) over ~60 commits. That growth is
itemized below, because most of it is *scope*, not verbosity — and one
task's growth is the library getting stricter about honesty, which is a cost
worth seeing rather than hiding.

One honest limit on the July column: re-running the rules above over the
July tree reproduces its per-task figures only to within a few percent,
scattering in both directions (temperature −46, timer +60), so the original
tokenizer invocation differed in some detail no longer recoverable. The
July column here is therefore *this* script's reading of that tree — the
right basis for a like-for-like delta, but not a verbatim reprint of what
was published. The +2 264 growth is measured consistently; the individual
July cells may differ slightly from the previous revision's.

**On this revision's numbers.** Measured after the view/logic module
split (each task is a view module over a logic module the MDC2/MDC3
twins share verbatim; writing.md states the pattern), so the invocation
above includes the shared logic modules, per the all-files rule. The
split costs +235 tokens / +1 109 characters over the previous revision's
9 856 / 35 533 — module headers, export lists and the import lines both
halves now carry — and changes no program.

Third-party corpus numbers are carried forward from the July measurement;
those repositories were not re-fetched, so their figures date from then.

Two scope caveats that matter throughout:

1. **Vue's and Svelte's *cells* are not the same program.** Vue evaluates
   formulas with `new Function('get', 'return ' + exp)` — JavaScript's own
   `eval` *is* its parser, evaluator, and expression grammar, with no cycle
   detection. Svelte's cells does `SUM` over a cell list only. Bambik (like
   any typed implementation) writes a real formula parser/evaluator.
2. **Bambik's flight booker and CRUD exceed the spec.** Flight booker parses
   DD.MM.YYYY, range-checks, and enforces the ordering invariant, returning a
   variant itinerary rather than a boolean. CRUD now simulates an async
   backend (`Aff` create/update/delete with latency, a progress indicator,
   dispatch through `action`/`onCase`) that the reference implementations do
   not have.

## Tokens per task (cl100k BPE)

| Task | **Bambik** | July 2026 | Vue (official) | Svelte | React+MobX | Elm |
|---|---|---|---|---|---|---|
| Counter | 239 | 204 | 48 | 50 | 109 | 192 |
| Temperature | 462 | 377 | 165 | 165 | 898 | 1 219 |
| Flight booker | 2 057¹ | 1 704 | 411 | 333 | 825 | 2 162 |
| Timer | 644 | 495 | 282 | 236 | 673 | 1 028 |
| CRUD | 1 573² | 913 | 442 | 526 | 849 | 3 182 |
| Circle drawer | 2 079³ | 1 192 | 599 | 569 | 3 075 | 4 484 |
| Cells | 3 037 | 2 707 | 590⁴ | 859⁴ | 3 347 | 7 892 |
| **Total** | **10 091** | **7 592** | **2 537** | **2 738** | **9 776** | **20 159** |

¹ includes the beyond-spec itinerary validation, now in one module.
² now simulates an async backend — see caveat 2.
³ the canvas moved from a raw HTML string into typed SVG — see below.
⁴ reduced scope — see caveat 1. Excluding cells from both sides, bambik
7 054 vs Vue 1 947, a ratio of 3.6× instead of 4.0×.

### Where the +2 264 tokens went

Three tasks account for 79% of the growth, and the reasons differ:

- **Circle drawer +839.** Honesty. The July version drew its canvas by
  shipping a raw `<svg>` string to a `view` primitive with a hand-written
  render callback — markup the tokenizer barely charged for and the type
  system never saw. It is now built from SVG oculars (`svg`/`circle` with
  `attrWith` per attribute) through the retaining `foreach`, so every
  attribute is typed and channel-fed. The program got no bigger; the part
  that was hiding in a string became code.
- **CRUD +590.** Scope. It gained a simulated async backend: `Aff` actions
  with latency, an indeterminate progress bar, and `+→+` dispatch through
  `action`/`onCase`. The reference implementations mutate an array
  synchronously. This is a different, larger program.
- **Flight booker +364.** Consolidation plus the A12/A13 refactors: one
  module instead of two, single-record business functions dispatched with
  `informed`, and a variant-returning itinerary classifier replacing boolean
  predicates.

The remaining four tasks account for the other 471 tokens (counter +13,
temperature +50, timer +100, cells +308) — the ordinary drift of
`mvu`/`updated` renames, explicit-import churn, and the MDC2 vocabulary's
config records.

## Characters per task

| Task | **Bambik** | July 2026 | Vue (official) | Svelte | React+MobX | Elm |
|---|---|---|---|---|---|---|
| Counter | 888 | 758 | 167 | 182 | 392 | 814 |
| Temperature | 1 704 | 1 389 | 483 | 455 | 3 457 | 4 404 |
| Flight booker | 7 285 | 6 371 | 1 601 | 1 180 | 3 469 | 10 006 |
| Timer | 2 502 | 1 860 | 1 059 | 772 | 2 516 | 4 139 |
| CRUD | 6 614 | 3 342 | 1 795 | 1 824 | 3 276 | 15 433 |
| Circle drawer | 7 658 | 4 027 | 2 262 | 1 935 | 12 012 | 24 256 |
| Cells | 9 991 | 8 734 | 1 825 | 2 851 | 12 929 | 34 817 |
| **Total** | **36 642** | **26 481** | **9 192** | **9 199** | **38 051** | **93 869** |

## Code density (characters per token)

| | Bambik | Svelte | Vue | React | Elm |
|---|---|---|---|---|---|
| chars/token | 3.63 | 3.36 | 3.62 | 3.89 | 4.66 |

Elm is the outlier: its house style is whitespace-lavish (indentation
ladders, aligned `case` arms, one-field-per-line records), which characters
bill at full price while the BPE compresses. Bambik has moved from the dense
end (3.42) to the middle, now level with Vue — the SVG and config-record
work added attribute strings and labels, which are character-heavy and
token-cheap, diluting the operator-dense style that produced the old figure.
Bambik packs ~17 tokens per line against 6–8 for the others, so line counts
flatter it more than ever and are the least honest metric here.

## Ratios, by metric

| Metric | vs Elm | vs React+MobX | vs Vue/Svelte |
|---|---|---|---|
| Tokens | 2.0× smaller | ~parity (1.03×) | 3.7–4.0× larger |
| Characters | 2.6× smaller | 1.04× smaller | 4.0× larger |

**This is the headline change since July.** Bambik was 1.4× smaller than
React+MobX in tokens; it is now level with it. Against Elm the advantage
halved (2.9× → 2.0×). Against Vue/Svelte the gap widened from 2.8× to 3.9×.

How much of that is real depends on what you are asking. Against React+MobX
the comparison is now unfair in bambik's *disfavour*: its CRUD has an async
backend theirs lacks, and its flight booker validates dates theirs does not.
Normalizing for the two scope changes (subtracting CRUD's +590 and treating
circle-drawer's string-to-SVG move as neutral, since the July count charged
nothing for the markup) puts bambik near 8 700 tokens — still ~1.2× larger
than July's honest 7 592 baseline, and still a materially smaller advantage
over the typed competition than the previous revision claimed.

The line metric is dropped from this table. At 17 tokens per line, bambik's
line counts say more about its formatting rules — trailing chains cascading
onto one line, closers never starting a line — than about program size.

## Bundle size (bambik only)

Least-squares fit over the seven (minified source, minified bundle) pairs of
the `-mdc2` demos:

**`bundleSize(s) ≈ 516 kB + 3.6 × s`**, with **r² = 0.87**.

| Demo | source | bundle |
|---|---|---|
| counter | 785 B | 518.3 kB |
| temperature-converter | 1.5 kB | 516.3 kB |
| timer | 2.3 kB | 528.3 kB |
| crud | 6.3 kB | 547.7 kB |
| flight-booker | 7.4 kB | 544.5 kB |
| circle-drawer | 7.5 kB | 540.6 kB |
| cells | 9.9 kB | 547.6 kB |

The intercept still dominates: ~516 kB is the fixed floor (MDC components,
the PureScript runtime, the library core), and the whole counter-to-cells
source range moves the bundle by ~31 kB (~6%).

The fit itself changed character. July's r² was 0.33 — dependency *reach*
dominated, not source length. It is now 0.87: as the demos grew, they
converged on the same broad slice of the library, so what varies is mostly
volume. This is a fit over seven points with a 516 kB intercept and a 31 kB
spread, so the slope is a weak estimate either way; the honest summary is
unchanged — application code is nearly free after the floor.

## Developer experience (expert steady-state)

Size is only half the story. This section compares the day-to-day
experience of *fluent* practitioners — the bambik programmer is assumed to
know profunctors, strengths, optics, and categories and to be at home in
PureScript/Haskell; the others are assumed equally expert in their own
stacks. Learning curve is deliberately out of scope.

### Writing new UI

For the expert, bambik is closest to *specification transcription*: a form
is a `RecordToRecord.do` block whose lines are the fields, and there is no
separate design step of "what messages exist, where does state live, how do
components communicate" — the row types are derived from the components
composed, mostly inferred. The wiring layer has nearly zero degrees of
freedom, which is the expert's gain: there is usually exactly one way to
write it, and effort goes to the actual domain (the formula parser, the
itinerary invariant).

Elm cannot buy back its ceremony with expertise — every interaction costs
the four-place ritual (Msg constructor, update branch, view code, sometimes
a lens); fluency makes it fast and error-free but not shorter. Vue and
Svelte experts write at thought-speed, but the degrees of freedom that make
them fast persist as design decisions (where state lives, watch vs
computed): two Vue experts write visibly different code for the same task,
two bambik experts write nearly the same code. The React+MobX expert *is*
the architecture — the framework doesn't enforce the
actions/observables/components discipline; the developer does.

### The type system: collaborator vs gatekeeper

Fluency changes the character of bambik's row errors. The expert reads a
`Nub` failure as "open row", an ambiguity at a sibling stage as "unpinned
payload" — a diagnostic pointing at a specific known cause, the way a
Haskell veteran reads a missing-instance error (`doc/type-errors.md` is
this pattern table, written down). One honest asymmetry remains: bambik
errors report the symptom at the wrong location — the merge, not the
offending operand — so even the expert does one mental dereference Elm
never asks of anyone. Elm's errors are simply never a cost; TypeScript's
are middling; in Vue/Svelte the type system barely participates and the
expert substitutes runtime discipline and tests.

### Feedback loop

Expertise does not change the tooling gap — this is bambik's main remaining
deficit, though it narrowed since July. `npm run dev` now watches, rebuilds
incrementally and reloads the browser automatically (~2s per edit), so the
manual rebuild–refresh step is gone. What remains is the difference that
matters: the reload is a *reload*, not state-preserving HMR, so the expert
re-establishes UI state on every edit where a Vue/Svelte expert keeps it
(`tapped` probes and the emission trace partially compensate as in-pipeline
inspection). What changes with fluency is how often the loop is *needed*:
the bambik and Elm experts open the browser to check aesthetics and event
feel — if it compiles, the wiring is right — while the Vue/Svelte expert
opens it to check correctness, continuously. Fewer slow iterations vs many
instant ones: roughly a wash at 7GUIs scale, tilting typed as logic grows
(the cells evaluator was developed almost entirely against the compiler).

### Reading and modifying existing code

Arguably bambik's strongest axis. The density that inflates its token
counts is exactly what an expert wants when reading:
`slider { min: minVolume, max: maxVolume } # asField @"volume"` is the
whole story of that widget — model binding, direction, adoption — in one
line, locally, with nothing to cross-reference. Elm requires mentally
joining view/Msg/update even when fluent; MobX requires tracing implicit
reactive graphs; Vue templates hide bindings in string attributes invisible
to find-references. For modifications, bambik and Elm are compiler-guided
and total, and the bambik change is also smaller — one pipeline line versus
Elm's four sites. Vue/Svelte experts do textual archaeology and lean on
tests.

### Debugging runtime behavior

The one axis where the comparison flips against bambik. When something is
wrong at runtime despite compiling — an emission not propagating, a merge
gate retaining stale state, a `looped` re-entrancy surprise — the expert
has `tapped` and the console, versus Elm's time-travel debugger and
Vue/React's mature devtools showing the live state tree. Bambik's runtime
semantics (gates, retention, propagation order) live in module-header prose
and the expert's head, not in a tool. Rare events, but the costliest
minutes in the bambik expert's week.

### Steady-state summary

| Axis | Bambik | Elm | React+MobX | Vue | Svelte |
|---|---|---|---|---|---|
| Writing (expert-speed) | high — near-spec transcription | medium — ceremony floor | medium | highest | highest |
| Wiring correctness by construction | strongest | strong | weak | weak | weak |
| Error diagnostics | good for the fluent (wrong-location tax) | best | fair | minimal | minimal |
| Iteration loop | weakest (auto-reload, no state-preserving HMR) | good | good | best | best |
| Reading/modifying | best (local, dense, total) | good (4-site joins) | fair | fair | fair |
| Runtime debugging tools | weakest | best | good | good | good |
| Code uniformity across experts | near-canonical | high | low | low | medium |

With fluency assumed, bambik's profile is **strong where the compiler
participates** (writing, reading, refactoring, correctness — arguably the
best of the five at reading and change-safety) and **weak where tooling
participates** (iteration loop, runtime inspection). Vue/Svelte keep the
throughput crown but lose their biggest relative advantage — expert bambik
writing is much closer to their speed than the token counts suggest, since
most of bambik's extra tokens are inferred-checkable structure typed
without thought — though the gap those counts describe has widened to
~3.9×, so this claim carries more weight than it did in July. Elm remains the safest, most tool-supported typed option
but is the only one where expertise cannot reduce the ceremony. Bambik's
remaining gap is infrastructural, not conceptual: state-preserving HMR, a
devtools story for the propagation graph, and errors that point at the
offending operand instead of the merge — all buildable, none
paradigm-inherent.

## Conclusion

In every size metric the landscape splits into three tiers — but bambik has
moved between them since July.

1. **Untyped reactive-template frameworks** (Vue, Svelte): ~2 500–2 700
   tokens total. Template syntax and `ref`-style reactivity are extremely
   token-cheap — and part of the terseness is doing less: no static types,
   minimal validation, `eval` where a typed language writes a parser.
2. **Bambik and React+MobX**: ~9 800–10 100 tokens each. Bambik no longer sits alone
   in the middle; it has converged with the typed-imperative option it was
   1.4× smaller than. Its 7GUIs corpus does more than theirs (async CRUD,
   real date validation, typed SVG), so the comparison flatters React, but
   the previous revision's "30–65% cheaper than the typed competition" no
   longer holds against React at all.
3. **Thorough Elm** (~20 000). Elm's cost remains structural: a `Msg`
   constructor per interaction, an `update` case per constructor,
   `subscriptions`, separate view functions. In bambik the component *is* the
   message — the round trip Elm smears across four places is written once as
   a pipeline stage. This is the one tier boundary that has not moved, though
   the margin halved from 2.9× to 2.0×.

The trend is worth naming plainly: **bambik's 7GUIs corpus grew 30% in six
weeks while the library was being simplified.** Some of that bought honesty
(circle-drawer's canvas is now typed rather than a string the compiler never
read) and some bought scope (CRUD's backend). But the drift also includes
~470 tokens of ordinary churn across the four untouched tasks, and the
config-record style pushed character density up. A benchmark corpus that
grows while its library shrinks is a signal to watch, not to explain away —
if the next measurement shows the same, the demos are accreting rather than
demonstrating. (The August re-measure adds a further +235 tokens, +2.4%,
all of it the view/logic module split's headers, export lists and import
lines — structure, not program, and the price of the twins now sharing
their logic modules verbatim.)

Bundle-wise, bambik pays a ~516 kB fixed floor (mostly MDC + runtime) after
which application code is nearly free.
