# Bambik vs other frameworks on the 7GUIs benchmark

A measured comparison of code size across implementations of the seven
[7GUIs](https://eugenkiss.github.io/7guis/) tasks — counter, temperature
converter, flight booker, timer, CRUD, circle drawer, cells — in bambik and
four reference frameworks. All numbers are measured from real corpora, not
estimated.

## Corpora and methodology

| Corpus | Source | Notes |
|---|---|---|
| **Bambik** | `demo/7guis/` in this repo | PureScript, MDC components, deployed at erykciepiela.xyz |
| **Elm** | [dwayne/elm-7guis](https://github.com/dwayne/elm-7guis) | The most thorough community Elm version |
| **React + MobX + TypeScript** | [eugenkiss/7guis-React-TypeScript-MobX](https://github.com/eugenkiss/7guis-React-TypeScript-MobX) | By the 7GUIs author himself |
| **Vue 3** | Official Vue docs examples (Composition API: `composition.js` + `template.html`) | Written by the Vue team |
| **Svelte** | [joysofcode/svelte-7GUIs](https://github.com/joysofcode/svelte-7GUIs) | |

Counting rules, applied identically to every corpus:

- blank and comment lines stripped;
- per-task files only — shared framework/runtime code excluded on all sides;
  task-specific helpers (undo stacks, formula parsers) included;
- CSS excluded (Vue's stylesheet files, Svelte's `<style>` blocks);
- tokens counted with the cl100k BPE (gpt-tokenizer) — a proxy for LLM
  generation/reading cost;
- characters counted on the same stripped text — a proxy for raw
  typing/reading volume.

Two scope caveats that matter throughout:

1. **Vue's and Svelte's *cells* are not the same program.** Vue evaluates
   formulas with `new Function('get', 'return ' + exp)` — JavaScript's own
   `eval` *is* its parser, evaluator, and expression grammar, with no cycle
   detection. Svelte's cells does `SUM` over a cell list only. Bambik (like
   any typed implementation) writes a real formula parser/evaluator
   (~1 800 tokens).
2. **Bambik's flight booker exceeds the spec**: it includes an `Itinerary`
   correct-at-construction domain type with DD.MM.YYYY parsing, range
   checks, and the ordering invariant (~25 lines beyond the minimal
   version). Vue's accepts any `new Date()`-parseable string.

## Tokens per task (cl100k BPE)

| Task | **Bambik** | Vue (official) | Svelte | React+MobX | Elm |
|---|---|---|---|---|---|
| Counter | 194 | 48 | 50 | 109 | 192 |
| Temperature | 423 | 165 | 165 | 898 | 1 219 |
| Flight booker | 1 134¹ | 411 | 333 | 825 | 2 162 |
| Timer | 435 | 282 | 236 | 673 | 1 028 |
| CRUD | 928 | 442 | 526 | 849 | 3 182 |
| Circle drawer | 1 173 | 599 | 569 | 3 075 | 4 484 |
| Cells | 2 753 | 590² | 859² | 3 347 | 7 892 |
| **Total** | **7 040** | **2 537** | **2 738** | **9 776** | **20 159** |

¹ includes the beyond-spec `Itinerary` domain type.
² reduced scope — see caveat 1 above. Excluding cells from both sides,
bambik 4 287 vs Vue 1 947, a ratio of 2.2× instead of 2.8×.

## Characters per task

| Task | **Bambik** | Vue (official) | Svelte | React+MobX | Elm |
|---|---|---|---|---|---|
| Counter | 706 | 167 | 182 | 392 | 814 |
| Temperature | 1 467 | 483 | 455 | 3 457 | 4 404 |
| Flight booker | 4 096 | 1 601 | 1 180 | 3 469 | 10 006 |
| Timer | 1 569 | 1 059 | 772 | 2 516 | 4 139 |
| CRUD | 3 360 | 1 795 | 1 824 | 3 276 | 15 433 |
| Circle drawer | 3 955 | 2 262 | 1 935 | 12 012 | 24 256 |
| Cells | 8 914 | 1 825² | 2 851² | 12 929 | 34 817 |
| **Total** | **24 067** | **9 192** | **9 199** | **38 051** | **93 869** |

## Code density (characters per token)

| | Bambik | Svelte | Vue | React | Elm |
|---|---|---|---|---|---|
| chars/token | 3.42 | 3.36 | 3.62 | 3.89 | 4.66 |

Elm is the outlier: its house style is whitespace-lavish (indentation
ladders, aligned `case` arms, one-field-per-line records), which characters
bill at full price while the BPE compresses. Bambik sits at the dense end —
heavy on single-character operators (`#`, `$`, `@`, `>>>`) and braces, which
are token-expensive. Bambik also packs ~12 tokens per line versus 6–8 for
the others, which is why line counts alone flatter it.

## Ratios, by metric

| Metric | vs Elm | vs React+MobX | vs Vue/Svelte |
|---|---|---|---|
| Lines | ~2× smaller | ~1.4× smaller | ~parity |
| Tokens | 2.9× smaller | 1.4× smaller | ~2.8× larger |
| Characters | 3.9× smaller | 1.6× smaller | ~2.6× larger |

The three metrics tell one consistent story with different emphasis. Lines
is the flattest metric (and the one that flatters bambik most, because its
lines are dense); tokens best proxies LLM generation cost; characters best
proxies raw reading/typing volume.

## Bundle size (bambik only)

Least-squares fit over the seven (minified source, minified bundle) pairs:

**`bundleSize(s) ≈ 522 kB + 3.0 × s`**, with **r² = 0.33**.

| Demo | source | bundle |
|---|---|---|
| counter | 764 B | 513.2 kB |
| temperature | 1.4 kB | 522.8 kB |
| timer | 1.9 kB | 524.5 kB |
| flight-booker | 2.1 kB | 552.1 kB |
| crud | 3.4 kB | 537.1 kB |
| circle-drawer | 4.0 kB | 526.7 kB |
| cells | 8.8 kB | 546.8 kB |

The intercept dominates: ~522 kB is the fixed floor (MDC components, the
PureScript runtime, the library core). The entire counter-to-cells source
range (764 B → 8.8 kB) moves the bundle by only ~34 kB (~6%). The low r²
means dependency *reach* — which library modules a demo pulls in — drives
the variance, not source length; flight-booker's +4.2% residual comes from
its Aff/action/select reach, not its line count.

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
`Itinerary` invariant).

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
deficit. Vue/Svelte experts live in state-preserving HMR; the bambik expert
waits seconds per edit–spago–esbuild–refresh cycle and re-establishes UI
state each time (`tapped` probes partially compensate as in-pipeline
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
| Iteration loop | worst (no HMR) | good | good | best | best |
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
without thought. Elm remains the safest, most tool-supported typed option
but is the only one where expertise cannot reduce the ceremony. Bambik's
remaining gap is infrastructural, not conceptual: HMR, a devtools story for
the propagation graph, and errors that point at the offending operand
instead of the merge — all buildable, none paradigm-inherent.

## Conclusion

In every size metric the landscape splits into three tiers:

1. **Untyped reactive-template frameworks** (Vue, Svelte): ~2 500–2 700
   tokens total. Template syntax and `ref`-style reactivity are extremely
   token-cheap — and part of the terseness is doing less: no static types,
   minimal validation, `eval` where a typed language writes a parser.
2. **Bambik**: ~7 000 tokens, alone in the middle. 30–65% cheaper than the
   typed competition. The gap versus Vue/Svelte is the measured price of
   static row types, 100% explicit imports, real domain validation, and a
   real expression parser — a defensible trade, but a real price, not a
   wash.
3. **Typed message-passing architectures** (React+MobX ~9 800, thorough Elm
   ~20 000). Elm's cost is structural: a `Msg` constructor per interaction,
   an `update` case per constructor, `subscriptions`, separate view
   functions. In bambik the component *is* the message — the round trip
   that Elm smears across four places is written once as a pipeline stage.

Bundle-wise, bambik pays a ~522 kB fixed floor (mostly MDC + runtime) after
which application code is nearly free.
