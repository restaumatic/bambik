---
name: writing-bambik-apps
description: How to write web/MDC applications with bambik in the style of the repo's demos — app shape, vocabulary choice, separation of concerns, code style, demo-page conventions, tracing, build/verify workflow. Use when creating or reworking a bambik application or demo.
---

# Writing bambik applications

A bambik application is one profunctor pipeline. Every widget is a
`PUI m i o` — it displays `i` and emits `o` — and the app composes
widgets with `Semigroupoid.do` (data-flow stages) and the four
qualified-do row merges:

- `RecordToRecord.do` (×→×) — all-at-once: forms, editor groups
- `RecordToVariant.do` (×→+) — model in, events out: button rows
- `VariantToVariant.do` (+→+) — event dispatch: backend actions
- `VariantToRecord.do` (+→×) — events in, display out: status snackbars

Every MDC component is a citizen of exactly one direction and speaks a
canonical row, adopted to the business label at the use site:

- editors (`filledTextField`, `checkbox`, `slider`, ...) are
  `{ value :: _ } → { value :: _ }`; adopt with `# asField @l`
- displays adopt with `# forField @l` (read one field) or
  `# projection f # forValue` (format the whole value)
- event emitters (`button`, `fab`, `iconButton`, `menuItem`) emit
  `[ clicked :: _ ]`; adopt with `# asCase @l`
- statuses (`snackbar`, `banner`) consume `[ event :: String ]`; adopt
  with `# forCase @l`
- type-changing selectors (`select`, `radioButton`, `segmentedButton`)
  are `{ value :: Maybe a } → { value :: a }`; always-selected ones take
  `# required # asField @l`

Oculars (`card { caption }`, `dialog`, `layoutGrid`, `topAppBar`,
typography, elevations, ...) are shape-preserving decorators — wrap
freely; code order = DOM order.

## App shape

Demos are standalone modules exporting a single entry function. The
shape of the pipeline follows the app, not a blessed template — a pure
self-feeding loop reads `# mvu seed`, a loop-free flow reads
`# with seed` (demo/1: load action → form → events → backend dispatch →
statuses → `silence`), and the two combine freely (crud: load action
feeding a `looped` form whose commands dispatch through write actions).
A minimal MVU example:

```purescript
module Counter (counter) where

counter :: Effect Unit
counter =
  body $
    elevation20 $ card { caption: "Counter" } $ ( Semigroupoid.do
        ...form merge... # completed
        ...event merge... # updates (match { ... })
    ) # mvu freshCount
```

- `body` registers the wiring and feeds nothing; initial data enters via
  `# mvu seed` (`mvu seed w = looped (with seed w)`), `# with seed`, or
  an `action` load stage.
- `# completed` after a form merge widens its output back to the full
  model row (unproduced fields carried from the retained input).
- `# updates (match { case: handler, ... })` folds each event into the
  model; each handler is `payload -> Model -> Model`.
- Nest sub-composites with `# field @l` (wrap a group as one model
  field) or `focusRecord` (sub-record focus).
- Live readouts are pipeline stages: `# tapped` (display and pass on),
  `text # projection f # forValue`, `debounced` for summaries.
- Variant editing: keep the variant in the model, a record of every
  payload in the editor — `dimap` brackets around
  `looped RecordToRecord.do { selector; shownWhen panes }`.
- Custom leaves: `view` with a typed `i -> Array Markup` render function
  (auto-escaped, injection-proof) plus event wiring (`onKeyClick`,
  `onClickXY`); dynamic collections via `foreach` or `MDC.listOf`.
- Async: `indeterminateLinearProgress # action (s -> Aff t)` as a
  pipeline stage; dispatch variants with
  `action (on (Proxy @"case") handler case_)` inside
  `VariantToVariant.do`.
- State/loop semantics beyond MVU: the trace forms — `feedback`
  (state sub-record loops output→input), `iterate` (retry loops),
  `folding @w` (wizards), `unfolding @w` (generators) — and the
  `Shutter`/`Reel` business optics.

Type errors from the row layer are catalogued with reproduced output in
doc/type-errors.md — read it before fighting a merge error.

## Separation of concerns

Organize each module (by inlining and extracting) until every function
belongs to exactly one of two classes:

1. **UI wiring** — lives inline in the entry function (or is unavoidably
   standalone like a `Model -> Array Markup` render function). Anything
   that mentions PUI types, variants-as-events, `Markup`, DOM wiring.
2. **Pure business** — standalone functions over the model and plain
   data: `Model -> Model`, `Model -> String`, parsers, evaluators, Aff
   actions. No variant types, no PUI types, no UI vocabulary in their
   signatures.

**File order**: the one purely UI-related entry function
(`counter`, `cells`, ...) comes first, followed by the pure business
functions over the model. (This structure is also stated in the demo
pages' code-style note; keep the two in sync.)

### What to inline (delete the named glue)

- **Update dispatchers** — a `handle :: [ cases ] -> Model -> Model` that
  merely `match`es cases becomes an inline dispatch at the update stage:

  ```purescript
  # updates (match { cellClicked: selectCell, undo: undo, ... })
  ```

  Each case's body is extracted first (see below). `match { ... }` applied
  point-free curries correctly: `updates` wants `e -> Model -> Model` and
  `match { c: f }` gives exactly that when each handler `f` is
  `payload -> Model -> Model`.

- **Event constructors** — a `clickedCell :: String -> [ cellClicked :: String ]`
  wrapper becomes the variant sugar applied inline at the wire:

  ```purescript
  (\node emit -> onKeyClick node \key -> emit (.cellClicked key))
  ```

### What to extract (name the business)

Each case lambda inside the old dispatcher becomes a standalone pure
function named for the business action, payload first, model last:

```purescript
selectCell :: String -> Model -> Model
selectCell key m = m { selected = Just key, formula = fromMaybe "" (lookup key m.cells) }
```

Existing `Model -> Model` functions (`commit`, `applyDiameter`, …) already
belong to the business class — leave them standalone.

**Business literals hiding in UI code are a smell.** Numeric bounds and
steps (slider `min`/`max`/`step`), seed/initial models (`mvu`/`with`/`seeded`
arguments), tick periods, default payload values — extract each as a named
business definition in the business section:

```purescript
) # mvu tenSecondFreshTimer
sliderLive { min: minDuration, max: maxDuration, step: durationStep }
every tickPeriod tick

tenSecondFreshTimer :: Timer
tenSecondFreshTimer = { duration: 10.0, elapsed: 0.0 }

minDuration :: Number
minDuration = 0.0
```

UI code keeps only presentation: labels, captions, icons, styles, structure.
Name the extracted values in **business language**, not lifecycle language —
`initial`/`default`/`seed` are UI-lifecycle words and a smell of their own.
Say what the value *is*: `roomTemperature`, `peopleCatalogue`, `emptyCanvas`,
`tenSecondFreshTimer`, `plannedTrip`.

The same rule applies to **type aliases**: `type Model = ...` is
architecture language, not business language. Name the type after what it
models — `PeopleCatalogue`, `Canvas`, `Temperature`, `Sheet` — typically
the seed's name capitalized (`mvu peopleCatalogue` seeds a
`PeopleCatalogue`). And to the **entry function**: name it after the
application (`crud`, `counter`, `temperatureConverter`), not `main` —
`Model`, `initial`, even `main` are all the same smell: architecture
words where business words belong. Models stay row-shaped and structural as far as
readable — anonymous Record rows for all-at-once, anonymous Variant rows
for one-at-a-time; a named alias only for the top aggregate.

### Type-inference gotchas (both hit in practice)

- **Inline variant sugar needs a closed-row annotation.** A named
  constructor wrapper pinned the row via its signature; inlined, the sugar
  is open and the merge's `Nub` fails. Annotate at the use site:

  ```purescript
  emit (.clicked { x, y } :: [ clicked :: { x :: Number, y :: Number } ])
  # rmap (\e -> .picked e.key :: [ picked :: Int ])
  ```

- **Ignored button payloads still pin rows.** A `button # asCase @l`
  emission's payload row is inferred *from the handler*. `const f` leaves
  it free and the whole merge becomes ambiguous (the error surfaces at a
  sibling stage). Dispatch by applying the business function to the payload
  snapshot instead — it is the same model value:

  ```purescript
  # updates (match { create: \m _ -> createPerson m, ... })
  ```

### Boundary cases

- `Model -> Array Markup` render functions are UI but too large to inline —
  they stay standalone; that is fine (they are *purely* UI-related).
- `Model -> String` caption/validation formatters are pure business — keep.
- **A `forall click. click -> Model -> Model` handler is a smell**: the
  phantom payload parameter is UI (the event) smuggled into an otherwise
  pure business function. Strip it — the business function is
  `Model -> Model` — and absorb the event in the inline dispatch. Note the
  bare (un-`asCase`d) button emits the canonical variant `[ clicked :: _ ]`,
  so the dispatch is a one-case match applying the business function to the
  payload snapshot (which also pins the button's row):

  ```purescript
  button { label: "Count" } # updates (match { clicked: \m _ -> increment m })

  increment :: { count :: Int } -> { count :: Int }
  increment r = { count: r.count + 1 }
  ```

## Code style

The demo pages state this contract in their footer note; the code must
honor it, and changes to either side keep the two in sync:

- **Comments are deliberately absent** — code should read on its own.
- **Imports are 100% explicit (including `Prelude`)** — code is honest
  about its dependencies. Add/remove names the change touched.
- **The listing is a complete standalone application**, entered at its
  single exported function.
- **One purely UI-related function first, then pure business functions.**
- **Each UI-related line leads with the visual concern with `$` plumbing
  and trails with the data concern with `#` plumbing** —
  `card { caption: "CRUD" } $ ... # asField @"prefix"`.

## Demo page conventions (index.html)

Each demo page follows the 7guis template: CDN `<link>`s for MDC CSS,
Material Icons, and highlight.js (CSS is never bundled into JS), a
header with back link / task description / source and bundle sizes, a
source panel fetching and highlighting the module beside the running
app, and — before `bundle.js` — tracing enabled:

```html
<script>window.__bambikTrace = true</script>
<script type="module" src="bundle.js"></script>
```

The source panel footer carries two notes (exact wording):

```html
<p class="note">Code style: Comments are deliberately absent - code should read on its own. Imports are 100% explicit (including <code>Prelude</code>) - code is honest about its dependencies. The listing is a complete standalone application, entered at its single exported function. The code is structured as one purely UI-related function followed by pure business functions. Each UI-related line leads with the visual concern with <code>$</code> plumbing and trails with the data concern with <code>#</code> plumbing.</p>
<p class="note">Tracing: This page runs with enabled tracing, so data propagation is logged to the browser console with <code>[bambik]</code> prefix.</p>
```

Tracing can also be toggled at runtime:
`localStorage.setItem("bambik-trace", "true")`. It logs every propagation
decision — stage-to-stage flow, `looped` re-feeds/swallowed echoes, and
gate-withheld emissions (the otherwise-invisible ones) — as
`console.debug`, so enable the Verbose log level in DevTools.

## Build, verify, deploy

1. `npm install` first (forked compiler pinned in package.json:
   `node_modules/.bin/purs --version` must report
   `0.15.16 [development build ...]`; stock purs fails with "Module
   Prim.Variant was not found"), then
   `export PATH=$PWD/node_modules/.bin:$PATH`.
2. **Agent loop: use watch mode.** Keep `spago build -w` running in the
   background and read its output after each edit (~0.7s incremental)
   instead of one-shot builds — it covers library, tests, and all 7GUIs
   demos (project sources; the `Main`-module demos — 1, 2, mdc,
   helloworld — need their own `--path`, one at a time). Caveats:
   spago -w reads stdin and dies on EOF, so keep stdin open (never
   `</dev/null`); run only one watcher over the shared `output/` at a
   time. Tests: `spago test`.
3. Interactive dev loop: `npm run dev <demo>` serves the demo at
   `http://127.0.0.1:1234/` with auto-rebuild and browser auto-reload.
4. Bundle with `npm run bundle-demo-7guis` (or `bundle-demo-1|2|mdc`;
   7guis demos are named modules entered at their own function, bundled
   via scripts/bundle-7guis.mjs — `spago bundle-app` can only call
   `Main.main`).
5. Verify behavior with the headless-Chrome CDP harness (bundle +
   http.server + scratchpad cdp.mjs), commit to main, deploy with
   `npm run deploy-demo-*`, and check
   `http://erykciepiela.xyz/bambik/demo/<d>/` returns 200 (plain HTTP).
