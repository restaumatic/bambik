# Writing a bambik application

The rules below govern the app module (`src/<Module>.purs`) the scaffold
ships. They are the **definitive statement of bambik application code
style** — the [Code style](#code-style) section at the end is the strict
contract, and everything before it is the vocabulary and the shapes that
contract is written in. Nothing else restates them; other documents
point here.

The demos named throughout are worked examples in the fetched library,
under `.spago/bambik/<tag>/demo/7guis/` and `demo/nguis/` — read one
when a rule needs a shape. Their directories carry a vocabulary suffix
(`counter-mdc2`, `counter-mdc3`); the pair is the same module with the
import switched, so read whichever matches the app's design system.

## The pipeline

The app is one profunctor pipeline, composed with `Semigroupoid.do`
(data-flow stages) and the four qualified-do row merges:

- `RecordToRecord.do` (×→×) — all-at-once: forms, editor groups
- `RecordToVariant.do` (×→+) — model in, events out: button rows
- `VariantToVariant.do` (+→+) — event dispatch: backend actions
- `VariantToRecord.do` (+→×) — events in, display out: status snackbars

The merges are imported from the row modules
(`Data.Profunctor.Row.RecordToRecord` and its three siblings), not from
`QualifiedDo` — only `Semigroupoid` lives there.

## Component citizenship

Every component is a citizen of exactly one direction and speaks a
canonical row, adopted to the business label at the use site:

- **editors** (`filledTextField`, `checkbox`, `slider`, ...) are
  `{ value :: _ } → { value :: _ }`; adopt with `# asField @l`. A lone
  adopted editor followed by `# completed` is a complete `×→×` stage on
  its own — no `RecordToRecord.do` for a single field.
- **displays** adopt with `# projected f` (feed `f` of the whole value).
  A one-field read of a context-pinned wider row is the label-indexed
  `# forProperty @"label"`; at merge-operand and `completed` positions,
  which must state their row, the closed form is
  `# forField @l identity`, and `# projected f # forField @l` reads one
  field formatted (`forField` takes the bare-value display `projected`
  produces). A named projection whose body merely reads one field is a
  smell — use the label-indexed form and delete the function. The same
  applies to mechanism arguments: a feed projection that merely reads a
  field is the accessor. The exception is row-stating positions, where
  the named function's closed signature *is* the footprint declaration
  and stays.
- **event emitters** (`button`, `fab`, `iconButton`, `menuItem`) emit
  `[ clicked :: _ ]`; adopt with `# asCase @l` to rename, or
  `# toCases f` to fire the business outcome `f` computes from the
  payload.
- **statuses** (`snackbar`, `banner`) consume `[ event :: String ]`;
  adopt with `# forCase @l copyOf` for one case, or
  `# forCases (match { … })` when one status instance serves several
  mutually exclusive outcomes (flight-booker's booking toast). A status
  mid-pipeline — showing events that must also flow on — wraps with
  `# observed` (payment's retry toast narrates the retry loop); the
  status may consume a narrower variant than the stage carries,
  background cases pass untouched.
- **type-changing selectors** (`select`, `radioButton`,
  `segmentedButton`) are `{ value :: Maybe a } → { value :: a }`;
  always-selected ones take `# required # asField @l`,
  possibly-unselected ones `# optional # asField @l` — the model keeps
  the `Maybe` seeded `Nothing` (no default pick), and the stages
  demanding the bare selection stay `provided`-gated until the user
  picks (meeting-booker is the no-defaults showcase).

**Oculars** (`card`, `dialog`, `layoutGrid`, `topAppBar`, typography,
elevations, ...) are shape-preserving decorators — wrap freely; code
order = DOM order.

Component configs are anonymous records whose field names belong to the
vocabulary, not to a convention — `filledTextField`'s `floatingLabel`
and `button`'s `label` differ — so read the component's signature in the
vocabulary module, or copy a demo's call, instead of guessing; a guessed
label surfaces as a `TypesDoNotUnify` on the config record.

## Pass-through stages

Two wrappers make a stage pass-through, and they are not
interchangeable:

- `# completed` widens a *row-shaped* stage's output to its full input
  row from the retained input — safe over editors and displays alike,
  the default inside record pipelines.
- `# tapped` forwards the whole input value of *any* shape but is honest
  only over displays: the display's echo triggers the forwarding, so an
  editor inside would replay stale upstream values on every edit.

So: editor or record display stage → `# completed`; display over a
non-record value (a `projected`-formatted readout) → `# tapped`. A live
readout as a pipeline stage is just a display made pass-through this way
(tip-calculator's money readouts).

A terminal **collection display** — a projection rendered as a list or
grid, passing the model through — is the keyed `foreach` inside its
container ocular, trailed by `# displayed`, whose unconditional echo is
the collection's announcing unit (so an empty array never starves). Here the rows projection must be a **named projection with a
closed row**, not an accessor: `displayed` widens a *closed* narrow row,
so an open one leaves no `Union` instance and the error lands on
`displayed`. This is the row-stating exception to the
delete-the-one-field-projection rule. Stopwatch's laps list is the
worked example.

A constant-fed stage — a fixed catalogue driving `listOf`/`foreach` —
reads `constantly catalogue` instead of an input-annotated feed.

## App shape

The shape of the pipeline follows the app, not a blessed template: a
pure self-feeding loop reads `# mvu seed`, a loop-free flow reads
`# with seed`, and the two combine freely.

Worked examples, by shape:

- **smallest MVU** — counter.
- **loop-free pipeline** — order-form (load action → form → events →
  backend dispatch → statuses → `silence`); it is also the
  four-direction showcase.
- **both combined** — crud (a load action feeding a `looped` form whose
  commands dispatch through write actions).
- **channel-fed structure-from-data** — cells and circle-drawer (7guis),
  tic-tac-toe and calculator (nguis): a fixed grid or canvas fed as data
  through the retaining `foreach`, each cell built once and updated in
  place via `attrWith` (value-computed attribute) + `text`, emitting its
  key via `clicked` + `toCase @l _.key`. No `data-*`, no wholesale
  rebuild. `onClickedXY` is the container-level coordinate emitter for
  canvases.
- **collections** — todomvc (`listOf` click-to-toggle plus `clWhen`
  styling), shopping-cart (`dataTable`/`dataRow`/`dataCell` over
  `foreach`, catalogue fed by `listOf`'s projection argument), reorder
  (keyed reconciliation and the `edited` collection editor), potluck
  (`acted`, the gather gate as UX).
- **panes** — quiz (`provided` panes over multi-stage pipelines keyed on
  `Maybe`-projected state).
- **effects and time** — password-generator (`button # asCase` →
  `action`/`onCase` → `updated`), stopwatch (`every` with
  pause-by-`Nothing`), color-mixer (`sliderLive` driving an `attrWith`
  swatch).
- **structure-from-value** — markdown-previewer: a recursive `PUI Web`
  tree built by `displayed (dynamic …)`, because the structure genuinely
  varies per block.
- **the floor and the plain-HTML end** — helloworld (bare minimum),
  restaurant-menu (no design system at all: element oculars +
  `staticText` merged as `{} → {}` chrome, data via `each`, look
  supplied by page CSS).
- **one focused combinator each** — auction (`feedback`), checkout
  (`folding`), payment (`iterate`), ticket-dispenser (`unfolding`),
  parcel (`focusRecord`), cashbox (`focusVariant`), departures
  (`dispatched`), scoreboard (`accumulated`).

## Conditional visibility

Conditional visibility is view-model data, never an in-UI predicate.

When the model field is a payload-carrying variant, case adoption *is*
conditional existence — `# atCase @l identity # atField @l'` shows the
pane while the variant sits at that case, fed its payload
(ticket-dispenser). Mutually exclusive derived states classify once into
a variant-returning business function, each pane adopting its case
(signup-form: two classifiers replaced five `Maybe` projections,
exclusivity by construction).

Otherwise `provided` attaches and feeds its content on `Just`, detaches
on `Nothing`. Pair it with a **named `Maybe`-valued projection** so the
pane consumes the payload, not the whole model, and the visibility logic
is a testable business function. A pane whose content only exists
sometimes is exactly this. The mode-of-a-live-editor case — a variant
editor's per-selection panes — is the same shape inside a `looped`
pipeline: selection component `# completed`, then each pane
`# provided <paneOf> # updated <setPane>`.

`clWhen` stays predicate-driven: it toggles a class (styling), not
visibility, and is deliberately last-element-only.

## Modals

`dialog`/`simpleDialog` open on feed and close on emission. Feed them
selectively (`# provided` off a model flag, or behind an event case via
`onCase`), put the deciding emitters inside — their emission closes the
dialog and flows on — and keep echoing displays off the content's final
stage, since an echo would close the dialog on open. Cashbox is the
worked example.

`drawer`'s nav slot is live: nav and content are sibling stages over the
same types, so a selectable nav merges its selector with static chrome
in one `RecordToRecord.do`.

## Collections

Collection items may hold stateful stages (`completed`, `updated`) —
refs are per-instance.

`foreach @l` (keyed by the row's materialized identity field; `listOf`
index-keys internally) **retains** items: it reconciles *by key* —
matched keys re-fed in place, new built, absent removed, DOM reordered
only when the key sequence changed — so a channel-fed item keeps its DOM
and state across feeds. Fixed-key grids never rebuild, growing lists
append, and a reordered list moves each node with its key, so focus and
local state follow the item.

The closure builders (`foreachWith`/`dynamic`/`each`) rebuild per value,
since their content lives in the builder closure. Reach for them only
when an element's *structure* genuinely varies with the data (markdown
blocks); when only *values* change over a fixed structure, feed the
structure as data through `foreach` and compute per-element attributes
with `attrWith`. Durable state still belongs in the model, with
`listOf`'s click-replay folding it back.

A **collection editor** is `edited @l` — `foreach`'s editor form. The
key is a **label**, not a function, and the element editor's output row
**excludes** it: each emission's key is re-attached as the edit's return
address, so an element structurally cannot change its key.
It folds every element emission back into the array by key, emitting the
whole updated array immediately, input-primed. An element whose merge
covers less than the full row widens its input (`# widenRecordInput`)
instead of `completed`-ing the id through. The result is a first-class
`Array a → Array a` stage: nest it in a form via `# field @l` or feed it
straight to `# mvu`.

Rows need stable identity (an id field) — the key is both the
reconciliation identity and the return address of each edit, so an array
of bare strings cannot be edited in place. Add, remove and reorder are
array-level concerns: sibling `updated` stages over the enclosing model,
not part of the element. Reorder is the worked example (in-row rename
via `edited`, Rotate and Shuffle as sibling action stages).

## Separation of concerns

Organize the module (by inlining and extracting) until every function
belongs to exactly one of two classes:

1. **UI wiring** — lives inline in the entry function, or is unavoidably
   standalone like a widget-builder function for `dynamic`/`foreachWith`.
   Anything that mentions PUI types, variants-as-events, DOM wiring.
2. **Pure business** — standalone functions over the model and plain
   data: model transformers, formatters, parsers, evaluators, Aff
   actions. No variant types, no PUI types, no UI vocabulary in their
   signatures.

**File order**: the one purely UI-related entry function comes first,
followed by the pure business functions over the model.

### What to inline (delete the named glue)

- **Update dispatchers.** A named handler that merely `match`es cases
  becomes an inline `match { … }` at the update stage, each case's body
  extracted as its own business function. Applied point-free, `match`
  curries correctly: `updated` wants `payload -> model -> model`, which
  is exactly what a `match` of such handlers gives.
- **Event constructors.** A wrapper function that injects a payload into
  a case is unnecessary: a channel-fed cell replays its own value on
  click and `toCase @l` introduces the case, closing the row itself.

### What to extract (name the business)

Each case lambda inside the old dispatcher becomes a standalone pure
function named for the business action, dispatched with `informed` so it
takes one row (see [Code style](#business-functions)). Existing
model-to-model functions already belong to the business class — leave
them standalone.

The model row is spelled once, at the seed and the merges; every
business helper states its own exact narrow footprint as a closed row,
never the whole model. Values that legitimately live in the business
section — seed models, tick periods, default payloads — are named there
in business language.

### Type-inference gotchas (both hit in practice)

- **Introduce an output case with `toCase @l`, not an annotated lambda.**
  At a collection site the item's bare output becomes a business case
  through `toCase`, which closes the row itself — no inline variant
  sugar, no annotation, and the label shows up in tracing. A channel-fed
  cell and the container coordinate emitter `onClickedXY` both produce a
  **bare** payload, so both take `toCase @l` rather than an inline
  injection lambda.
- **Ignored button payloads still pin rows.** A button emission's
  payload row is inferred *from the handler*. A handler that discards
  the payload with a plain `const` leaves the row free, the whole merge
  becomes ambiguous, and the error surfaces at a sibling stage.
  Composing instead — applying the business function to the payload
  snapshot and `const`-ing the result — pins the row while staying
  point-free.

### Boundary cases

- Widget-builder functions (for `dynamic`/`foreachWith`) are UI but too
  large to inline — they stay standalone, and that is fine: they are
  *purely* UI-related.
- Caption and validation formatters are pure business — keep them.
- **A handler with a phantom payload parameter is a smell**: the
  payload it never reads is UI (the event) smuggled into an otherwise
  pure business function. Strip it — the business function is
  model-to-model — and absorb the event in the inline dispatch. Note a
  bare (un-`asCase`d) button emits the canonical `[ clicked :: _ ]`, so
  the dispatch is a one-case match that applies the business function to
  the payload snapshot, which also pins the button's row.

## Code style

The definitive contract for application code. Each rule is strict: code
that breaks one is wrong even if it compiles and behaves. They build on
the structural rules above — anonymous view-model types, one UI function
then the business ones, a single exported entry function.

### Layout

- **Comments are deliberately absent** — code should read on its own.
- **Imports are 100% explicit (including `Prelude`)** — code is honest
  about its dependencies. Add and remove the names each change touches.
- **Each UI-related line leads with the visual concern with `$` plumbing
  and trails with the data concern with `#` plumbing.** No data word
  ever leads a line: an announced payload trails like every other data
  concern (`button { … } # with patch # asCase @l`, never a leading
  `with patch (button …)`), and `# with {}` is written inline when the
  payload is the informationless unit, since naming `{}` is ceremony.
- **Closing parens and trailing `#` chains never start a line.** A
  trailing chain is written on one line (never one `#` per line) and
  rides at the end of the widget's last content line — close the paren
  inline and continue. When a bracketed widget nests, the enclosing
  levels' closers and chains cascade onto that same final line. The one
  exception is the app-level closer: the last UI line stays
  `) # mvu seed` / `) # with seed` on its own.
  **Precedence caveat:** `#` (`infixl 1`) binds tighter than `$`
  (`infixr 0`), so where the chain must apply to the *whole element* —
  `foreach` multiplying an ocular-wrapped widget — the paren must open
  *before* the ocular, never after its `$`, which would put the chain
  inside the element (one container around the collection instead of one
  per item). `lcmap`-only adopters (`forField`, `projected`) are safe
  either side of a shape-preserving ocular.
- **The architecture is readable off the types.** The application is a
  compass walk written as one pipeline — load → form (×→×) → live
  summary → events (×→+) → dispatch (+→+) → statuses (+→×) — closed by
  `mvu seed` / `with seed` to `PUI Web {} model`. If the top-level types
  do not tell that story, the structure is wrong, not the types.
  Indirection layers, widget registries and config objects that assemble
  UIs reflectively are out.

### Types and values

- **No nominal types in UI.** A view-model type is one-off and specific
  to this UI, so it earns no name: no `data`, no `newtype`, no `type`
  synonym for anything a widget displays, emits, or is configured with.
  Anonymous record rows for all-at-once, anonymous variant rows for
  one-at-a-time, `{}` for unit payloads (never `Unit`), primitives at
  the leaves, `Array`/`Maybe` as the only generic containers. Role names
  live on **values** (`mvu plannedTrip`, `with emptyCanvas`) and on
  business function names, never on types. Nominal types belong below
  the UI — a directly recursive type (a formula AST) or an ecosystem API
  (`Aff`, `Either`, `Milliseconds`) — and enter only as rows projected
  by business functions.
- **Business literals never hide in UI code.** A component parameter is
  presentation config iff the design system owns it; if the business
  owns it, it is model data riding the canonical row. Sliders and
  ratings edit a **bounded quantity** `{ current, min, max, step }`, so
  bounds arrive from the seed, may change while the app runs, and an
  editor cannot invent its own. What legitimately stays config or seed
  content — tick periods, default payloads, seed models — is a named
  top-level definition in **business language** (`smallestLoan`,
  `tickPeriod`, `gameStart`, `roomTemperature`), never lifecycle
  language: `initial`, `default` and `seed` are the smell's second form,
  as is naming the entry function `main`. UI code keeps only
  presentation — labels, captions, icons, styles, structure; layout
  numerics (a textarea's `rows`, a grid's `columns`) stay UI.
- **Simple text concatenation is UI structure.** A displayed line glued
  from fields and literal separators or prefixes is a merge of
  `staticText` pieces and per-field displays — each field its own text
  node, updated in place:

  ```purescript
  headline6 ( RecordToRecord.do
      staticText "Till balance: €"
      text # forField @"balance" euros ) # tapped
  ```

  never `text # projected balanceLine` over a
  `balanceLine { balance } = "Till balance: €" <> euros balance`. If
  deleting the literals would leave only field reads, the function is UI
  structure in disguise. Business functions format *values* (a money
  formatter, a time formatter), never *lines*. String-channel copy
  (toast lines) and shape-varying lines (case analysis, conditional
  fragments) are the exemptions.
- **Business emissions carry bare data, never UI copy.** Toast and
  banner text lives in `PUI Web`-returning widget functions
  (`welcomeToast = snackbar # forCase @"registered" welcomeLine`); the
  event carries the order, the outcome, the reason — the data, not the
  sentence. Validation results are payloads, not strings destined for a
  particular widget.

### Business functions

- **Exact footprints.** Every business function states its footprint as
  a closed narrow row — what it reads ∪ writes, never the whole model.
  The reading stages (`updated`/`tapped`/`displayed`/`edited`/`acted`/
  `completed`) absorb the widening, so rows are read narrow while
  payloads stay exact; never coerce a row at the call site. A handler
  that reads nothing is not a transformer but a **constant patch**
  (`beginTiming :: { running :: Boolean }`, dispatched with
  `const (const beginTiming)`, or announced to a bare button as
  `button { … } # with reset # updated (match { clicked: const })`).
- **One record of data per business function.** Several record
  parameters that travel together are one row in disguise — merge them
  and let the field labels name the roles that positional currying
  loses:

  ```purescript
  returnBetween :: { out :: Date, back :: Date } -> Maybe Itinerary
  returnBetween { out, back } = …            -- never  returnBetween out back
  ```

  Fold handlers are no exemption: the payload and the retained model do
  travel together into every fold, so dispatch with `informed`, which
  lays the payload's fields over the model's (fresh knowledge wins) —

  ```purescript
  # updated (match { refunded: informed applyRefund })
  applyRefund :: { amount :: Number, balance :: Number } -> { balance :: Number }
  ```

  — and reads become per-branch exact. Only scalar and `Array` payloads
  (a key, an operator symbol, a fetched list) stay positional; they are
  not rows. Where a payload label would shadow a model label of another
  type, name the payload for its **role** (`{ seats }` over the model's
  `attendees`).
- **A handler carries no field it does not touch.** Its row is exactly
  its reads ∪ writes — a field that only rides through is a smell — and
  every field of a `match`'s shared row is written by *some* branch.
  When a branch would carry fields only its siblings touch, pick by
  separability: **separable emitters** group into stages by patch row
  (undo/redo split from the canvas click, so they shed the field only
  the click reads); **inseparable branches** — one dialog's two
  outcomes, one backend stream — keep the shared row, the carried field
  being a sibling's write; **disjoint footprints** mean the events or
  the model want redesign until the branches genuinely share. An
  **identity handler** is the smell at its purest: the event was never
  model data, so the honest wiring is `# displayed`, a display
  interaction, not an `updated` fold. Bounded quantities ride whole even
  where a handler replaces only `current`.
- **Lossy conversions live in the model, not in leaf brackets.** An
  editor's bracket must round-trip; a lossy normalization belongs after
  `completed`, where the loop makes it a transaction — never hidden
  inside a component's `dimap`.

### Wiring

- **Application code never imports `Data.Profunctor`.** Speak the
  vocabulary: the adopters, the merges' qualified-do, and the mechanisms
  with their projection arguments — `provided paneOf`, `foreach @l
  rowsOf`, `listOf opts rowsOf`, `dispatched envelopeOf`,
  `toCase @l payloadOf`, `forCase @l copyOf`, `projected f`,
  `forProperty @l`, `toCases outcomeOf`, `forCases lineOf`,
  `settled normalize`, `bracketed stateOf caseOf`, with `identity`
  saying verbatim. Every raw `lcmap`/`rmap`/`dimap` an application would
  write has one of those homes. A shape none of them fit is a
  missing-vocabulary signal to report — never a reason to import the
  module one floor down. Business optics (`Shutter`/`Reel`) in business
  code *below* the UI are exempt by location.
- **Visibility is business logic.** Conditional visibility is a
  `Maybe`-valued projection plus `provided`, or case adoption
  (`atCase @l`) on a payload-carrying variant field — never an in-UI
  predicate. A projection that *derives* visibility is named, so the
  rule lives in testable business code, and mutually exclusive derived
  states classify once into a variant-returning function so exclusivity
  holds by construction. Where the model field itself is the `Maybe`
  **and the context pins the row**, the bare accessor says it; at
  row-stating positions the named projection stays, its closed signature
  being the footprint declaration. `clWhen` stays predicate-driven — it
  toggles styling, not existence.
- **State lives in the model or in the algebra's loops. Nowhere else.**
  No FFI stashes, no module-level `Ref`s, no reading the DOM back as
  state, no window globals. The model under `mvu` holds the entity; a
  widget's private state is a residual threaded by the trace forms.
- **Lean on the design system's defaults; write no custom chrome.**
  Reach for a stock component and its built-in look before any style
  attribute. Surfaces (`card`, `elevation*`), typography, lists, grids
  and the components' own spacing already carry the design language, so
  a flex or border wrapper is a smell: drop the presentational `div` and
  let the components flow inline (buttons and fields are `inline-flex`,
  a `listOf` already scrolls, block typography stacks). The minimal look
  is the intended one. Custom styling is a last resort for genuinely
  data-driven graphics — an SVG canvas, a colour swatch — never for
  layout the design system already gives you. Every avoided style string
  is code you don't write.

## Reference

The API and its semantics are documented in the source module headers —
read them, not a summary. Paths are inside the fetched library,
`.spago/bambik/<tag>/`:

- `src/PUI.purs` — the core type, pipeline semantics, and the
  combinators: `mvu`/`with`/`looped`/`updated`/`completed`/`action`/
  `onCase`/`tapped`, the adopter family re-exports, and the collection
  combinators `foreach @l`/`edited @l`/`acted @l`/`dispatched`/
  `accumulated`.
- `src/PUI/Web/HTML.purs` — HTML vocabulary, `body`, element oculars,
  `attrWith` for channel-fed structure-from-data, the builders
  `foreachWith`/`dynamic`/`each` for structure-from-value, and the
  `clicked`/`onClickedXY` events. SVG oculars are in
  `src/PUI/Web/SVG.purs`.
- `src/PUI/Web/MDC2.purs` — the MDC2 component and ocular catalog, plus
  the editors' `dimap` round-trip contract. The sibling design systems
  sit beside it (`MDC3.purs`, `Shoelace.purs`, `Fluent.purs`,
  `Bootstrap.purs`) — same two-sorted vocabulary, switch by switching
  the import.
- `src/Data/Profunctor/Row/` — the four merges, the adopters, the trace
  forms and the business optics; laws in the module headers.
- `doc/type-errors.md` — row-layer type errors catalogued with
  reproduced output; read it before fighting a merge error.
- `doc/no-nominal-types-in-ui.md` — the design rule behind the
  anonymous-rows discipline above.
