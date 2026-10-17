# Bambik and other frameworks on 7GUIs

Measured 2026-10-17, from scratch: every corpus re-fetched at a pinned
commit, every number re-measured by `scripts/count-7guis.mjs`, every
implementation read against the verbatim task statements at
<https://eugenkiss.github.io/7guis/tasks/>. Nothing is carried over from
the previous comparison (deleted 2026-10 as rotted; its history is in git).

## The corpora

| Corpus | Source, commit | Notes |
|---|---|---|
| **bambik (MDC3)** | this repo, `demo/7guis/*/MDC3.purs` + each app's view model module | all seven tasks; the MDC3 twin is the one every app has |
| **bambik (HTML)** | `demo/7guis/*/HTML.purs` + the same view model modules | five tasks; crud and cells have no plain-HTML twin |
| **Elm** | [dwayne/elm-7guis](https://github.com/dwayne/elm-7guis) `16fd562` (2026-02) | the most thorough community Elm version; tests, elm-review in CI |
| **React+MobX** | [eugenkiss/7guis-React-TypeScript-MobX](https://github.com/eugenkiss/7guis-React-TypeScript-MobX) `e121076` (2018) | by the 7GUIs author; TypeScript; `drawer-traditional` is the circle drawer (the MST one is a stub) |
| **Vue 3** | [vuejs/docs](https://github.com/vuejs/docs) `40aa88a`, `src/examples/src/*` | the official examples, Composition API |
| **Svelte 5 (official)** | [sveltejs/svelte.dev](https://github.com/sveltejs/svelte.dev) `89f941c`, `content/examples/20-7guis` | runes; six tasks, no cells |
| **Svelte (joysofcode)** | [joysofcode/svelte-7GUIs](https://github.com/joysofcode/svelte-7GUIs) `c794bdd` | runes, TypeScript; all seven tasks |

## In one paragraph

Bambik's 7GUIs is **9 329 tokens** — level with React+MobX (9 510), 2.2×
smaller than Elm (20 672), 3.7× larger than Vue (2 537), and 3.5× larger
than official Svelte on the six tasks both have (5 849 against 1 654). It
meets **22 of 23** spec requirements, ahead of Elm (21.5), React+MobX (17), Vue (14) and both
Svelte corpora (12.5 each). Its wiring is the smallest
part of it: the seven views' bodies are **1 347 tokens**, less than the
official Svelte corpus whole; 76% of the corpus is business logic in view
model modules (cells' alone is 2 990) and 17% is module
headers and explicit imports. Its plain-HTML bundle floor (13.3 kB gz) is
the smallest measured, level with Elm. What no other corpus has is that
the same view model runs under six design systems unchanged, and that
each view is mechanically checked to determine its view model's
signatures. Its two remaining partial marks are one design position
(absent rather than disabled) and one layout choice (below).

## Spec fidelity

Each implementation read against the task statements; ✓ met, ◐ partial or
deviating, ✗ missing. The per-corpus findings behind each mark are
summarised under the table.

| Requirement (spec wording, abridged) | bambik | Elm | React+MobX | Vue | Svelte off. | Svelte joy |
|---|:-:|:-:|:-:|:-:|:-:|:-:|
| **Temperature** both fields initially empty | ✓ | ✓ | ✓ | ✗ | ✗ | ✓ |
| … the other updated as the user types | ✓ | ✓ | ✓ | ◐ | ✓ | ✓ |
| … a non-numerical string leaves the other unchanged | ✓ | ✓ | ✓ | ◐ | ✗ | ✗ |
| **Flight** T2 enabled iff "return flight" | ◐ | ✓ | ✓ | ✓ | ✓ | ✓ |
| … an ill-formatted date colours its field red | ✓ | ✓ | ✓ | ◐ | ◐ | ◐ |
| … Book disabled for a bad date or T2 strictly before T1 | ✓ | ✓ | ◐ | ◐ | ◐ | ✓ |
| … a message informing of the selection | ✓ | ✓ | ✓ | ✓ | ✓ | ◐ |
| **Timer** slider reflects immediately | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| … stops at e ≥ d, restarts when d is raised | ✓ | ✓ | ◐ | ✗ | ✓ | ✓ |
| … reset sets e to zero | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| **CRUD** surname-prefix filter, immediately | ✓ | ✓ | ◐ | ✓ | ✓ | ◐ |
| … Update and Delete enabled iff an entry is selected | ✓ | ◐ | ✗ | ◐ | ✓ | ✗ |
| … create appends, update replaces, delete removes the selected | ✓ | ◐ | ✓ | ◐ | ◐ | ✗ |
| **Circle drawer** left-click on empty area creates an unfilled circle | ✓ | ✓ | ✓ | ✓ | ◐ | ✓ |
| … the circle nearest the pointer is grey (hover) | ✓ | ✓ | ✓ | ◐ | ◐ | ◐ |
| … right-click → "Adjust diameter.." → dialog with a slider | ◐ | ✓ | ◐ | ◐ | ◐ | ◐ |
| … one adjustment is one undo step | ✓ | ◐ | ◐ | ✓ | ◐ | ◐ |
| … undo/redo, redo dropped on a new change | ✓ | ✓ | ✓ | ✓ | ✓ | ✗ |
| **Cells** scrollable, A–Z × 0–99 | ✓ | ✓ | ✗ | ✗ | — | ✗ |
| … double-click edits the formula | ✓ | ✓ | ◐ | ◐ | — | ✓ |
| … a formula language: references, ranges, arithmetic | ✓ | ✓ | ◐ | ◐ | — | ✗ |
| … only the cells depending on a change recomputed | ✓ | ✓ | ✓ | ◐ | — | ◐ |
| … cycles handled | ✓ | ✓ | ◐ | ◐ | — | ✗ |
| **Score** (✓ = 1, ◐ = ½) | **22 / 23** | **21.5 / 23** | **17 / 23** | **14 / 23** | **12.5 / 18** | **12.5 / 23** |

Counter is met by all six and left out (official Svelte's count field is
editable, a quibble). Where a native `<input type="date">` makes a
malformed date untypeable, the red-field row is ◐: the rule is sidestepped,
not implemented.

**bambik.**

- **Temperature converter:** both fields start empty. Each field's
  `settled` rewrites the other only when the two disagree, so typing
  0.1 °F does not come back as 0.10000000000000142.
- **Flight booker:**
  - The return date *exists* only for a return flight rather than being
    disabled; bambik has panes, not disabled editors (the one ◐).
  - A date the calendar does not have (31.02.2026 included) sets the
    field's `aria-invalid`, which the page colours red.
  - Book exists only for a bookable trip, which meets the rule by
    construction.
  - Book opens a dialog stating the selection, "You are going to book a
    one-way flight on 27.03.2026.", confirmed by its own Book button.
- **CRUD:**
  - Update and Delete exist only while an entry is selected rather than
    being disabled, which meets the rule by construction.
  - A filter change that hides the selected entry drops the selection.
- **Circle drawer:**
  - The circle nearest the pointer, among those containing it, is grey,
    through a pointer-move source (`onHoveredXY`).
  - Right-clicking it opens a popup at the pointer with "Adjust
    diameter.." (`onContextMenuXY`). The adjustment opens a frame with
    the live slider, but as a panel above the canvas with its Close button
    beneath it, not as a separate window (the ◐).
  - One adjustment is one undo step, closed by Close, the next canvas
    click, undo or redo — closer to the spec than the mouse-up commits of
    Elm and React.
- **Cells:**
  - 100 rows; double-clicking a cell edits its formula in place, and
    Enter, Tab or a click elsewhere commits it.
  - It has a real parser, `SUM` over ranges, arithmetic and `#CYCLE`.
  - Values are stored; a commit re-evaluates only the edited cell and the
    cells depending on it, each after the cells it reads.

**Elm** is the most faithful corpus. It has:
- a strict date parser with calendar validation;
- a hover-nearest selection and a right-click menu with a dialog, run as a
  small state machine;
- in cells, an explicit dependency graph with topological re-evaluation
  and cycle rejection;
- tests for CRUD's data layer and the whole cells engine.

Its slips are small:
- CRUD prepends instead of appending, and a selection hidden by the
  filter still acts;
- an adjustment commits on the slider's mouse-up, not when the dialog
  closes;
- cells re-renders all 2 600 cells through the virtual DOM on every edit.

**React+MobX** gets cells' "recompute only dependents" for free from MobX
`computed`, in about six lines. Its gaps:
- CRUD never fills the fields on selection and never disables
  Update/Delete;
- the timer counts wall-clock time since Reset, so it jumps rather than
  resumes;
- the circle drawer has no right-click (a left click opens the menu);
- cells is cut to 5×10, and its parser drops the arguments after a
  nested call;
- the temperature converter rounds to whole degrees.

**Vue** keeps every task under 100 lines by leaning on the platform:
- Native number and date inputs stand in for validation, so there is no
  red field. An empty number input becomes 0 and overwrites the other
  temperature.
- The timer cannot resume once stopped.
- Cells is 5×20, with no ranges. **Formulas are JavaScript run through
  `new Function`**: JavaScript itself is the parser, which means
  arbitrary script per cell and a hang on `for(;;)`, with cycles caught
  only as a stack overflow.

**Svelte (official)** is the shortest corpus and below spec in places:
- non-numeric input zeroes the other temperature;
- a same-day return is rejected while a cleared date stays bookable;
- CRUD's selection is an index into the filtered list, so it re-targets
  when the filter changes;
- the circle drawer has neither hover selection nor a menu.

It has no cells.

**Svelte (joysofcode)** has the most outright bugs:
- CRUD's Delete removes by the edit fields' values, and Update without a
  selection writes index −1;
- the circle drawer's undo never truncates redo, and can crash after
  undoing the first circle;
- cells is 10×10, with two functions, no ranges, no arithmetic and no
  cycle handling.

## Size

`node scripts/count-7guis.mjs <corpora-dir>`. Comments and blank lines
stripped by each language's own syntax, CSS and Svelte `<style>` blocks
excluded, tests excluded, code shared by all tasks excluded (Elm's
`Support/**`, React's `basic.tsx`/`utils.ts`/`css.ts`, the libraries and
frameworks themselves); every per-task file counted, business logic
included. Tokens are cl100k BPE (gpt-tokenizer), a proxy for reading and
generation cost.

| Task | bambik (MDC3) | bambik (HTML) | Elm | React+MobX | Vue 3 | Svelte off. | Svelte joy |
|---|---:|---:|---:|---:|---:|---:|---:|
| Counter | 202 | 202 | 250 | 109 | 48 | 38 | 50 |
| Temperature | 464 | 514 | 1 299 | 786 | 165 | 148 | 165 |
| Flight booker | 1 018 | 1 054 | 2 233 | 695 | 411 | 333 | 333 |
| Timer | 585 | 593 | 1 110 | 673 | 282 | 185 | 236 |
| CRUD | 1 127 | — | 3 258 | 849 | 442 | 440 | 526 |
| Circle drawer | 2 453 | 2 474 | 4 558 | 3 075 | 599 | 510 | 541 |
| Cells | 3 480 | — | 7 964 | 3 323 | 590 | — | 846 |
| **Total** | **9 329** | — | **20 672** | **9 510** | **2 537** | **1 654** | **2 697** |
| Total without cells | 5 849 | 4 837¹ | 12 708 | 6 187 | 1 947 | 1 654 | 1 851 |
| Characters, total | 31 990 | — | 98 224 | 36 785 | 9 192 | 5 733 | 8 919 |

¹ five tasks: crud has no plain-HTML twin either.

The third-party figures match the previous comparison's July measurement
to within a few percent (Vue identically, 2 537), so the counting rules
carry over. Bambik's corpus was 10 007 tokens in August (the MDC2
twins, the primary then). Consolidating flight-booker and the view
model modules and removing crud's simulated server, which the spec never
asked for, brought it to 7 922; closing the spec gaps — circle-drawer's
hover, popup and adjustment frame, cells' in-place editing, 100 rows and
dependency-driven evaluation, flight-booker's calendar check and
confirmation — brought it to 9 329.

**Where bambik's tokens are.**

| Part | Tokens | Share |
|---|---:|---:|
| view bodies — every stage, layout and wiring line of the seven apps | 1 347 | 14% |
| view headers and imports | 897 | 10% |
| view model modules (seeds, copy functions, handlers, parsers) | 7 080 | 76% |
| of which module headers and imports, both kinds of module | 1 570 | 17% of all |

The view of a whole task is 43 tokens of body for counter and 336 for
cells. The rest is the domain: cells' parser and dependency-driven
evaluator (2 781 tokens), circle-drawer's hover, menu, adjustment and
undo stacks (1 611), flight-booker's calendar check and copy (638).
Module and import ceremony is 17% of bambik's corpus against 11% for Elm, 7% for React and under 5% for
Vue and Svelte — the price of explicit import lists in two modules per
app, set by the code style.

**Reading the ratios.** Vue and Svelte are smaller because templates and
mutable reactive cells are token-cheap, and because, as the fidelity table
shows, part of their brevity is doing less: native inputs instead of
validation, `new Function` instead of a parser, cells at 5×20 or absent.
Against the typed corpora bambik is level with React+MobX, and 2.2× smaller than Elm, whose
cost is structural: a `Msg` constructor, an `update` branch and view
plumbing per interaction. In bambik the leaf is the message: `button
@"Count"` is the event, its case and its caption, and `fold @"Count"
increment` is its whole update.

## Bundle floor

A counter, minified and gzipped (`gzip -9`), bundled by esbuild for each
framework from a minimal entry written for the measurement. These are
fixed costs before any application code; for bambik the counter *is* the
demo's bundle.

| Counter | gz | raw |
|---|---:|---:|
| bambik, plain HTML | 13.3 kB | 48.5 kB |
| Elm 0.19.3 (`--optimize`) | 13.7 kB | 39.3 kB |
| Svelte 5.57 | 16.8 kB | 45.0 kB |
| Vue 3.5 (render function, no template compiler) | 26.0 kB | 65.9 kB |
| React 19.3 | 68.9 kB | 222.9 kB |
| React 19.3 + MobX 7 | 82.3 kB | 270.4 kB |
| bambik, MDC3 (Material web components included) | 88.8 kB | 473.1 kB |

Bambik's application code is nearly free after the floor: the plain-HTML
twins go from 9.7 kB (temperature-converter) to 20.8 kB (circle-drawer),
the MDC3 twins from 84 kB to 97 kB across all seven. The MDC3 figure buys a design
system no other corpus here has; the plain-HTML one is the like-for-like
number.

## Where each puts the work

**State and propagation.**
- **Vue, Svelte and MobX** propagate by tracking what each render or
  `computed` reads. Derived values are expressions that stay current by
  themselves, which is why their cells get dependency-limited
  recomputation without a graph.
- **Elm** propagates by re-running `view` over one model after each
  `update`.
- **Bambik** propagates by data flow along a pipeline. The model is one
  record, re-entering whole through `looped`. A derived value is a copy
  function on the line that shows it (`text personLine`), and an
  invariant between fields is `settled` on the editor that owns them
  (`# settled fromCelsius`).

  Nothing tracks reads, so nothing makes "recompute only dependents" free
  either: bambik's cells pays for it in its evaluator, which stores the
  values and re-evaluates an edited cell's dependents in dependency
  order.

**Where invariants live.**
- **Vue, Svelte and MobX:** in handlers, watchers and template
  expressions. That is where most of their spec gaps sit: buttons never
  disabled, selections re-targeted by an index, a cleared field read as 0.
- **Elm:** in types. `Initial | Valid | Invalid String` keeps the raw text
  of a bad temperature, and a zipper makes "at most one selected" true by
  construction.
- **Bambik:** in the same place as Elm, and its rows forbid the two usual
  escapes, `Maybe` and `Boolean` flags (`npm run check-view-model`).
  - A selection is `[ none, picked { index } ]`, the circle drawer's
    mode `[ drawing, menu, adjusting { before } ]`.
  - A control that should not be usable is absent rather than disabled:
    Book is `# provided @"bookable" bookingState`, the return date
    `# inCaseAt @"return flight" @"Flight type"`.

  The cost is visible in the fidelity table: where the spec says
  *disabled*, bambik says *absent*, a deliberate deviation.

**Wiring.**
- **Elm** spreads one interaction over four places: `Msg`, `update`, a
  decoder and `view`.
- **Vue and Svelte** inline it in the template (`@click="count++"`).
- **Bambik** writes it once, as a view line naming its anchor: a field
  (`filledTextField @"Name"`), a case (`button @"Create"`), a copy
  function (`text personLine`) or nothing. Code order is screen order and
  data order. Two buttons are two cases, each with its own
  `fold @l handler`, and an action's outcomes are named by its `Aff`'s
  type and told by statuses that own exactly those cases
  (order-form's `"Order submitted"` / `"Total missing"`).

  The compiler checks the whole round trip: an emitter with no fold, a
  fold for a case no one emits, or a missing seed is a type error at the
  line.

**What only bambik has.**
- **One view model, six design systems.** Counter's six views differ in
  the vocabulary import and the heading word (`headline4`,
  `headlineLarge`, `title3`, `h4`), and the plain-HTML one adds a
  container `div`. The view
  model module is shared verbatim, so a design-system twin is a view-only
  diff by construction.
- **The view determines its view model.** `npm run check-determined`
  compiles every view with a typed hole for each imported value and
  checks that every exported signature is that hole's type with its open
  row tails filled. The same view compiled over bare runtime `hole`s
  mounts and runs (`npm run check-holes`), so the screen exists before the
  logic.
- **Laws instead of conventions.** The four row merges, the knot and the
  container action have law tests: an exhaustive check over every event
  script up to length 8, plus a headless-Chrome bench running every published
  leaf against the real DOM. What they buy the app:
  - a change to several fields arrives once, never torn;
  - showing state never fires an event;
  - a merge's lines can be reordered without changing behaviour.
- **Accessibility from the label.** A label is the caption, the model
  field and the host element's accessible name at once
  (`filledTextField @"Start date"`), and an axe-core audit in the smoke
  suite checks the naming rules on a sample of demos. Elm gets accessibility from native elements; the
  others leave it to the author.

**What bambik costs.**
- **A vocabulary:** a few dozen words (`looped`, `with`, `fold`,
  `joined`, `inCase`, `provided`, `settled`, `foreach`, `anyOf`, …), each
  with a shape.
- **A PureScript fork** for its variant-row sugar.
- **17% ceremony** in imports.
- **No disabled state** in the vocabulary: the flight booker's return
  date is absent rather than disabled, one of its two partial marks.
- **A reload loop, not state-preserving hot reload.** `npm run dev`
  reloads in about 2 s.
- **A runtime that is a trace and a starvation watchdog**, not a state
  devtool.

## What would close bambik's gaps

Two partial marks are left:

1. **Disabled versus absent** is a design position, not a gap to close
   blindly: a `disabled` decorator would be data-reactive, which
   `clWhen`/`attrWith` already allow; the panes are the current idiom,
   and they meet the spec's Book, Update and Delete rules by
   construction.
2. **Circle drawer's adjustment frame** is a panel above the canvas, its
   Close button beneath it, rather than a window of its own. A modal
   holding a *live* editor needs the editor's releases to reach the
   loop while the dialog is open; `confirmed`'s content is released only
   on confirm, so this is library vocabulary, not an application change.

## Reproduce

```sh
mkdir -p /tmp/7guis && cd /tmp/7guis
git clone https://github.com/dwayne/elm-7guis dwayne_elm-7guis
git clone https://github.com/eugenkiss/7guis-React-TypeScript-MobX eugenkiss_7guis-React-TypeScript-MobX
git clone https://github.com/vuejs/docs vuejs_docs
git clone https://github.com/sveltejs/svelte.dev sveltejs_svelte.dev
git clone https://github.com/joysofcode/svelte-7GUIs joysofcode_svelte-7GUIs
# check out the commits pinned in scripts/count-7guis.mjs, then:
cd - && npm i --no-save gpt-tokenizer
node scripts/count-7guis.mjs /tmp/7guis --files
```

The script refuses a clone whose HEAD is not the pinned commit, so a
re-measure measures the same code; bump the pins and this document
together.
