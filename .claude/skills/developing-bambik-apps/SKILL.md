---
name: developing-bambik-apps
description: How to write web/MDC applications with bambik in the style of the repo's demos — app shape, vocabulary choice, separation of concerns, code style, demo-page conventions, tracing, build/verify workflow. Use when creating or reworking a bambik application or demo.
---

# Developing bambik applications

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
  `{ value :: _ } → { value :: _ }`; adopt with `# asField @l` — a lone
  adopted editor (`filledTextField {...} # asField @l # completed`) is a
  complete `×→×` stage on its own, no `RecordToRecord.do` needed for one
  field (slider's `step` is optional)
- displays adopt with `# forField @l` (read one field) or
  `# projection f # forValue` (format the whole value); a live readout
  as a pipeline stage is the same display made pass-through with
  `# tapped`: `body2 (text # projection f # forValue) # tapped`.
  Two wrappers make a stage pass-through, and they are not
  interchangeable: `# completed` widens a *row-shaped* stage's output to
  its full input row from the retained input — safe over editors and
  displays alike, the default inside record pipelines; `# tapped`
  forwards the whole input value of *any* shape but is honest only over
  displays (the display's echo triggers the forwarding — an editor
  inside would replay stale upstream values on every edit). So: editor
  or record display stage → `# completed`; display over a non-record
  value (a `projection`-formatted readout) → `# tapped`. A terminal
  **collection display** (a projection rendered as a list/grid, passing
  the model through) writes `item # foreach _.key # lcmap proj #
  displayed` — the keyed `foreach` renders the projection, and
  `displayed`'s unconditional carrier echo is the collection's announcing
  unit (so an empty array never starves). A constant-fed stage (a
  fixed catalogue driving `listOf`/`foreach`) reads `constantly
  catalogue` instead of an input-annotated `lcmap (const catalogue)`
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
For worked examples read the 7GUIs demos (demo/7guis/ — counter is the
smallest MVU shape, crud combines a load action with a looped form and
write-action dispatch, cells and circle-drawer show **channel-fed
structure-from-data** in `PUI Web` — a fixed grid/canvas fed as data
through the retaining `foreach`, each cell built once and updated in
place via `attrWith` (value-computed attribute) + `text`, emitting its
key via `clicked # rmap`; `onClickedXY` for canvas coordinates), the
nGUIs demos (demo/nguis/ — todomvc shows `listOf` with
click-to-toggle plus `clWhen` styling, tip-calculator is an all-`×→×`
form with `tapped` readouts, quiz shows `provided` panes over
multi-stage pipelines keyed on `Maybe`-projected state, tic-tac-toe and
calculator are channel-fed `foreach` grid apps (cells styled by
`attrWith`, keys emitted via `clicked # rmap`, folded by `updates` — no
`data-*`, no wholesale rebuild), stopwatch drives `every`
with pause-by-`Nothing` and a keyed `foreach identity … # lcmap lapLines
# displayed` laps list, reorder is the keyed-reconciliation showcase (a
playlist keyed by track id, Rotate moves each row's DOM node with its
track so a DOM-local checkbox tick follows it),
shopping-cart is `dataTable`/`dataRow`/`dataCell` over `foreach` with a
`constantly`-fed catalogue, password-generator is the effectful shape
(`button # asCase` → `action`/`onCase` → `updates`), color-mixer pairs
`sliderLive` with an `attrWith` swatch + static `foreach _.name` chips,
markdown-previewer renders a recursive `PUI Web` tree via
`displayed (dynamic \doc -> each …)` (`el ("h" <> show level)`) — structure
genuinely varies per block, so it stays builder-built — from a hand-rolled
parser, helloworld is the bare minimum),
demo/1 (loop-free pipeline), and the trace-quartet demos in demo/nguis —
auction (`feedback`), checkout (`folding`), payment (`iterate`),
ticket-dispenser (`unfolding`), one focused combinator each.

Conditional visibility is view-model data, never an in-UI predicate:
`provided :: PUI Web a b -> PUI Web (Maybe a) b` attaches and feeds its
content on `Just`, detaches on `Nothing`. Pair it with a named
`Maybe`-valued projection so the pane consumes the payload, not the whole
model, and the visibility logic is a testable business function:
`pane # provided # lcmap currentQuestion`. A pane whose content only
exists sometimes is exactly this; the mode-of-a-live-editor case (a
variant editor's per-selection panes) is the same shape inside a `looped`
pipeline — selection component `# completed`, then each pane
`# provided # lcmap <paneOf> # updates <setPane>`. `clWhen pred name`
stays predicate-driven — it toggles a class (styling), not visibility,
and is deliberately last-element-only.

Modals: `dialog`/`simpleDialog` open on feed and close on emission —
feed them selectively (`# provided # lcmap toMaybe` off a model flag, or
behind an event case via `onCase`), put the deciding emitters inside
(their emission closes the dialog and flows on), and keep echoing
displays off the content's final stage (an echo would close the dialog
on open). `drawer`'s nav slot is live: nav and content are sibling
stages over the same types (a selectable nav merges its selector with
static chrome in one `RecordToRecord.do`); pure chrome nav embeds via
`# muted`.

Collection items may hold stateful stages (`completed`, `updates`) —
refs are per-instance. `foreach _.key` (and `listOf`, which index-keys
internally) **retains** items: it reconciles *by key* — matched keys
re-fed in place, new built, absent removed, DOM reordered only when the
key sequence changed — so a channel-fed item keeps its DOM/state across
feeds (fixed-key grids never rebuild; growing lists append; a reordered
list moves each node with its key, so focus/local state follows the
item). The closure builders (`foreachWith`/`dynamic`/`each`) rebuild per
value, since their content lives in the builder closure — reach for them
only when an element's *structure* genuinely varies with the data
(markdown blocks); when only *values* change over a fixed structure, feed
the structure as data through `foreach` and compute per-element
attributes with `attrWith`. Durable state still belongs in the model,
with `listOf`'s click-replay folding it back.

The API and its semantics are documented in the source module headers —
read them, not a summary: src/PUI.purs (the core type, pipeline
semantics, combinators: `mvu`/`with`/`looped`/`updates`/`completed`/
`action`/`onCase`/`tapped`/the adopter family re-exports),
src/PUI/HTML.purs (HTML vocabulary, `body`, element/SVG oculars, the
keyed retaining collection `foreach` (= `Sequence.sequenced`) + `attrWith`
for channel-fed structure-from-data, the builders
`foreachWith`/`dynamic`/`each` for structure-from-value,
`clicked`/`onClickedXY` events), src/PUI/MDC.purs (the MDC component and
ocular catalog, the editors' `dimap` round-trip contract), and
src/Data/Profunctor/Row/ (the four merges, adopters, trace forms,
business optics — laws in the module headers).

Type errors from the row layer are catalogued with reproduced output in
doc/type-errors.md — read it before fighting a merge error.

## Separation of concerns

Organize each module (by inlining and extracting) until every function
belongs to exactly one of two classes:

1. **UI wiring** — lives inline in the entry function (or is unavoidably
   standalone like a `a -> PUI Web {} o` builder for `dynamic`/`foreachWith`).
   Anything that mentions PUI types, variants-as-events, DOM wiring.
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
  wrapper is unnecessary: a channel-fed cell replays its own value on
  click and `toCase` introduces the case. The grid is fed as data through
  the retaining `foreach`; each cell renders from its fed value and emits
  its key:

  ```purescript
  cellWidget = clicked (td >>> attrWith "style" cellStyle $ text # lcmap (\c -> { value: c.text })) # rmap _.key
  table $ foreach (tr $ foreach cellWidget) # lcmap gridRows # toCase @"cellClicked"
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
`Model`, `initial`, `default`, even `main` are all the same smell: architecture
words where business words belong. Models stay row-shaped and structural as far as
readable — anonymous Record rows for all-at-once, anonymous Variant rows
for one-at-a-time; a named alias for the top aggregate, and for any
view-model row (a collection entry, a pane payload) reused across two or
more signatures — e.g. `type Entry`, `type Choice`, `type Photo`. A row
used in a single projection's return type can stay anonymous.

### Type-inference gotchas (both hit in practice)

- **Introduce an output case with `toCase @l`, not an annotated lambda.**
  At a collection site the item's bare output becomes a business case
  through `toCase` — no inline variant sugar, no annotation, and the label
  shows up in tracing:

  ```purescript
  listOf { selected: _.selected } (text # projection _.label # forValue)
    # rmap _.key # toCase @"picked" # lcmap entries # updates (match { picked: pick })
  ```

  A channel-fed cell (`clicked … # rmap _.key`) and the container coordinate
  emitter `onClickedXY` both produce a **bare** payload — a `String` key, an
  `{ x, y }` — so introduce the case with `toCase @l` (which closes the row
  itself, no annotation needed) rather than an inline `inj`/`.label` lambda:

  ```purescript
  svg [...] $ onClickedXY (foreach circleWidget # lcmap renderData) # toCase @"clicked"
  ```

- **Ignored button payloads still pin rows.** A `button # asCase @l`
  emission's payload row is inferred *from the handler*. `const f`
  (`\_ -> f`) ignores the payload and leaves the row free — the whole merge
  becomes ambiguous, the error surfacing at a sibling stage. Write
  `const <<< f` instead: it is `\payload model -> f payload`, applying the
  business function to the payload snapshot (the same model value), which
  pins the row while staying point-free:

  ```purescript
  # updates (match { create: const <<< createPerson, ... })
  ```

### Boundary cases

- `a -> PUI Web {} o` builder functions (for `dynamic`/`foreachWith`) are UI
  but too large to inline — they stay standalone; that is fine (they are
  *purely* UI-related).
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
- **Lean on MDC2's defaults; write no custom chrome.** Reach for a stock
  component and its built-in look before any `attr "style"`/`"style" :=`.
  MDC surfaces (`card`, `elevation*`), typography (`headline*`/`body*`/
  `caption`), lists (`listOf`/`list`), grids (`layoutGrid`/`layoutCell`)
  and the components' own spacing already carry the design language, so a
  flex/border wrapper is a smell: drop the presentational `div` and let
  the MDC components flow inline (MDC buttons/fields are `inline-flex`; a
  `listOf` already scrolls; block typography stacks) — the minimal-MDC
  look is the intended one. Custom styling is a last resort for genuinely
  data-driven graphics (an SVG canvas, a colour swatch), never for layout
  the design system already gives you. Every avoided style string is code
  you don't write.

## Demo page conventions (index.html)

Follow any 7guis demo's index.html (e.g. demo/7guis/counter/) as the
template: CDN `<link>`s for MDC CSS, Material Icons, and highlight.js
(CSS is never bundled into JS), a header with back link / task
description / source and bundle sizes, a source panel fetching and
highlighting the module beside the running app,
`<script>window.__bambikTrace = true</script>` before `bundle.js`, and
the two footer notes (code style, tracing) — copy their wording
verbatim; they state the code-style contract above, so changes to
either side keep the two in sync.

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
   and nGUIs demos (project sources; the `Main`-module demos — 1, 2,
   helloworld — need their own `--path`, one at a time). Caveats:
   spago -w reads stdin and dies on EOF, so keep stdin open (never
   `</dev/null`); run only one watcher over the shared `output/` at a
   time. Tests: `spago test`.
3. Interactive dev loop: `npm run dev <demo>` serves the demo at
   `http://127.0.0.1:1234/` with auto-rebuild and browser auto-reload.
4. Bundle with `npm run bundle-demo-7guis` (or `bundle-demo-1|2|nguis`;
   7guis demos are named modules entered at their own function, bundled
   via scripts/bundle-7guis.mjs — `spago bundle-app` can only call
   `Main.main`).
5. Verify behavior with the headless-Chrome CDP harness (bundle +
   http.server + scratchpad cdp.mjs), commit to main, deploy with
   `npm run deploy-demo-*`, and check
   `http://erykciepiela.xyz/bambik/demo/<d>/` returns 200 (plain HTTP).
