# Guardrails

*The normative document. [CLAUDE.md](../CLAUDE.md) tells the story
(*Overview*, *The algebra in brief*); this states the rules. Every MUST
and MUST NOT below is strict: a change that violates one is wrong even if
it works, ships, and demos well. Each rule names its grounding — a law, a
checkable form, or a precedent where the library already paid to learn
it; the precedents' full record is the git log at the dates given.*

The rules exist because bambik is a bet, and a bet pays only if it is not
hedged:

> A UI component is a profunctor. Everything is structure **on** the one core
> type — instances, plain combinators, laws — never machinery beside it.
> Refuse anything that does not come from the algebra.

Every guardrail is a consequence of taking that seriously.

---

## Part I — The library

### L1. One core type. Everything is structure on it.

```purescript
newtype PUI m i o = PUI (m (Registration i o))
type Registration i o = (o -> Effect Unit) -> Effect (i -> Effect Unit)
```

Registration hands the output continuation over and returns the input
leg, so a feed cannot precede it and every cell a component keeps is
allocated there, once per instantiation (2026-10-16).

- Every feature MUST be an instance of a class on `PUI m`, a plain
  combinator over such instances, or a law relating them. There MUST NOT
  be a runtime beside the type: no scheduler, no store, no context
  registry, no lifecycle machinery the wiring record cannot express.
- Before a bespoke class or combinator is admitted, it MUST be located in
  standard algebra first (monoidal structure, Tambara modules, traces,
  container actions); a bespoke concept is admitted only with a proof of
  irreducibility. *Precedents:* `Sequencing` rejected for the standard
  container action (`Acting`); `synced`/`latch` dissolved into `looped`;
  debouncing a theorem, not a gadget (`coresolve (resolve g >>> seeded
  (Right c0)) ≈ debounced g`). `looped` is the model irreducibility proof
  (a gated `unfirst` cannot self-feed); `Resolving`/`Retaining` the model
  honest coinage.
- The novelty budget is spent in exactly one place: the **carrier** — a
  duplex, asynchronous, stateful profunctor supports more actions
  (mixed, temporal, keyed) than a pure one. New capability MUST chart
  that territory, not invent abstractions beside it.

### L2. Carrier polymorphism, honest instances.

- The algebra layer (`Data.Profunctor.*`) MUST stay carrier-generic.
  `Web` is one carrier; the laws MUST be value-testable on a probe
  carrier (`PUI Effect`) without a DOM.
- A class MUST NOT be given an instance a carrier cannot honestly
  support. `(->)` has no `Retaining` (nowhere to keep memory), no
  `Resolving` (no quiescence), no point or knot (no registration moment,
  no feedback — which is why `announce` and `looped` are `PUI`'s words,
  not classes). A missing instance is a theorem about the carrier and
  MUST be kept missing; a faked one (an always-`Done` `resolve`, a
  `Default`-fabricated `retain`) makes the types lie.

### L3. The compass is closed: four shapes, and only four.

- Models come in exactly two shapes — Record (`×`, all at once) and
  Variant (`+`, one at a time) — giving exactly four component shapes.
  There MUST NOT be a fifth: anything that looks like one is a corollary
  of closure (collections: the algebra closed under `μ`) or wrongly
  conceived.
- **Shape is the type.** Every exported component MUST end in exactly
  one of the four row forms, each side a record or a variant; the type
  cannot enforce this (`forall i o` compiles), so every new or changed
  signature MUST be read against this table before it is accepted:

  | Type | Shape | Sort |
  | --- | --- | --- |
  | `PUI Web { \| a } { \| b }` | `×→×` | editors (selectors included, each with `…Unpicked` and `…Optional` siblings), the introducing form (`introduced @l init`), the labelled group (`group @l`), displays, panes (`inCase`), lists of displays (`actedOf`, `actedAt`), the updatable selection (`actedWithin`), the event path as a line (`stepped`), sources (fed `{}`, owning one field) and a source below a line (`# supplied`), stages |
  | `PUI Web { \| a } [ \| b ]` | `×→+` | emitters (`button`, `listOf`, `tile`, `onClickedXY`), `ticks @l`, `load f`, a source joined with its row (`joined @l`), the emitter pane `provided`, the emitting lists `gatheredOf` and `editsOf` |
  | `PUI Web [ \| a ] ({ \| b } -> { \| b })` | `+→×` | the folds (`fold`, `foldInto`, `reset`, `capture`) and the statuses, each a fold with a face (`snackbar @l f`) — the record side written as the **step**, which `stepped` applies to the row its line was last fed (`landed` before the knot, free to change the row's type); the action slot owns `started`/`ended` only, so a status is never the slot |
  | `PUI Web [ \| a ] [ \| b ]` | `+→+` | actions (`action @l f`), the dialogs — `confirmed @l`, and `modal @l` under `# acknowledged` or `# continued exit` |

  A closed row (`{}`, `[ event :: String ]`) is a row. A word that would
  need two shapes is two words, each lawful at its own: a pane is
  `provided` for emitters (a detached one's silence is `×→+`'s answer)
  and `inCase` for displays and editors (the wire while detached); a
  list is `actedOf` for displays and `gatheredOf` for emitters. A display
  is `×→×` like an editor: it releases the row it shows (a `{}` output
  made every display a decorator of its neighbour; 2026-10-13). A
  selector is one word because it is one shape: the widget stores the
  option it shows, so it is an editor — a `×→+` picker plus a completing
  stage misread a stored value as an occurrence (2026-09-26); its
  `…Unpicked` and `…Optional` siblings share a view model and differ in
  behaviour, which is why they are two words. **Decorators** and oculars
  are outside the rule: `clWhen`/`attrWith` read the row to style the
  element just built and keep whatever shape they decorate. A bare
  `i`/`a`/`Maybe a` side on an exported word is a violation (HTML's
  scalar `radioButton`, 2026-09-23; `dynamic`, fed the row).
- The four shapes are the four arrows of an application's **cycle**, and
  the rows between them are named by it — model, view model, view event,
  model event ([CLAUDE.md](../CLAUDE.md), *Core Type*). Documentation
  MUST use these four names for these four positions and for nothing
  else; the **view model module** is the module holding the functions of
  all four arrows.
- Every component MUST be a citizen of exactly one shape, and its row
  MUST speak the **business label**, stated once as the leaf's own type
  argument. No canonical label (`value`/`clicked`/`event`) appears in
  application code, and a leaf reading structured data MUST NOT make the
  application name its rows by the leaf's private names (`src`/`label`):
  it takes a read function and the view model translates at the boundary
  (`imagePane developedShot` replaced a `{ src, label }` row). Business functions are arguments of leaves, never adopters: every
  leaf narrows its input and its output itself — a status or an action
  owns the case its label names, an emitter is closed to the case it
  emits, an action is typed at exactly its outcomes and the folds and
  statuses after it name them, a dialog intercepts the case that opens
  it, an editor's release leaving as an event is `editsOf @l @k`'s to
  say — so a view places nothing at a row and `PUI` re-exports no
  adopter. The structural adopters (`atField`/`atCase`, `toField`) stay
  inside the vocabularies. There is no rename adopter: re-labelling a
  component built at a fixed internal label is what this guardrail
  forbids (`asField`/`asCase`, 2026-10-14). Two vocabulary value types
  are sanctioned in application rows, as units rather than a leaf's
  private names: the bounded quantity `{ current, min, max, step }` and
  the duration `{ ms :: Number }`. Statuses keep their internal payload
  case private; components MUST NOT have scalar or polymorphic model
  interfaces.
- **Conditions are cases.** No published word may need the application
  to store a `Maybe`, or a `Boolean` no Boolean editor edits. A word
  whose behaviour turns on a condition takes a variant with named cases,
  stored (`inCaseAt @c @l`, `providedAt @c @l`) or classified (`inCase
  @l f`, `provided @l f`), each pane naming only its own case; a choice
  not yet made, or cleared, is a case (`…Unpicked @l @c`, `…Optional @l
  @c @n`); a fact the platform may withhold arrives as a variant
  (`geolocated`'s `[ located, unknown, denied ]`, `filePicked`'s `[
  picked, none ]`), so no gate starves on it. A `Boolean` enters a row
  only through `checkbox`, `toggleSwitch`, `iconToggle`. A decorator may
  take a named predicate (`clWhen isCompleted "todo-done"`): it styles an
  element and never decides whether it exists. *Precedents:* the
  selector's unmade state, once a leaf silent on `Nothing` (2026-09-23
  to 26); `optioned`, the action over a `Maybe` field, deleted as
  unreachable by design (2026-10-14; L14).
- **A label is the copy it draws**: a captioned leaf's caption is its
  label verbatim — nothing derives copy from an identifier — so labels
  are written as the words they render, usually quoted, in the business
  rows as much as at the leaf (a quoted label cannot be a record pun;
  bind explicitly). An emitter MUST NOT take a `label:`
  config (two buttons under one handler are two business actions, each
  its own case and fold); an editor takes no caption config, its label
  MUST be only the field's name, and a format, example or unit rides the
  primed sibling (`filledTextField' @l { hint }`, `sliderLive' @l {
  unit }`); a case label is never shaped to fit a sentence elsewhere. A
  leaf takes no options record unless one is genuinely needed; optional
  presentation is a primed sibling, so no call site writes `{}`. A
  selector's options are leaves, one per case (`radio`, `segment`,
  `option`, `tab`), merged `VariantToVariant.do`, an option nothing but
  its case and its primed leaf's presentation; their order MUST be the
  order written, never the variant row's, which the compiler sorts.
- **The anchor invariant** (application side: writing.md's *The anchor
  invariant*): every view line names exactly one semantic anchor, in the
  anchor's own position — a model field or business case as the leaf's
  type argument, a named read function as a display's positional
  argument (L17), or nothing (chrome). **A visible `@"…"` names a row
  label and nothing else**: copy that is no label — a static's text, a
  bar's title, a dialog's caption, a column heading, a tip — is a String
  value (`staticText "Hours"`, `topAppBar "Espresso Bar"`, `columnHeader
  "Qty"`); a quantity display takes only its read function. A published
  word MUST NOT take, as a visible type argument, a row its function or
  value argument brings (L18) — the model's, a classifier's cases, a
  projection's element row, a payload, an action's outcomes (declared on
  the action until 2026-10-16) — and MUST NOT take an anchor or a
  required value inside a record: records carry only optional
  presentation or same-typed values whose field names prevent a swap.
  An ocular MUST NOT take a label (nothing to anchor) nor copy that has
  a place in its content; copy with no content position is a String
  value when static (`tooltip "…"`, `dialog "…"`) and a copy function
  when read from data; a tooltip's text is always static; a display
  carries no label except as an accessible name (L17); no mechanism may
  leave a line's meaning in an anonymous position. *Precedents:* the
  statuses (`snackbar @"Card charged" captured`, replacing a copy record
  and then a copy function); `modal @l` and `confirmed @l` telling their
  case as title and confirm caption, their dismiss captions dropped as
  chrome (`confirmed' { cancel }` keeps the one a screen in another
  language needs, 2026-11-17); the one-field ocular configs made
  positional (`tooltip`, `dialog`, `topAppBar`, `menu`, `imageList`,
  `layoutCell`, `badge`). The stronger rule — every line a field, oculars
  included — was rejected: a label on an ocular is a dead parameter
  (gate 4), line ↔ field is no bijection, and display lines as fields
  would move copy back into state (L17).
- **The labelled group** `group @l` is the boundary confirmed: a
  component, not an ocular — its label is the sub-record it nests,
  doubling as heading copy and accessible group name, an anchor doing
  work. The **leaf-fusion criterion**: `@l` fuses onto a wrapper exactly
  where the label does work a trailing `# focusField @l` cannot (the
  floor's `input @l`/`textArea @l` fused for their `name` stamp). The
  blind `card` stays for surfaces that edit nothing; around editors
  (which share a card only by sharing a sub-record) and around the whole
  app (`body $ …`, never `body $ card $ …`) it is wrong. With every
  public editor lifted inside its vocabulary, `focusField` left the
  application surface: a nesting no mechanism fits is a
  missing-vocabulary signal (L16), never a reason for the lens.
  Checkable form: `grep "focusField @" demo/` is empty, always.

### L4. The merge law: sharing is open, responsibility is exclusive.

- A shared record input is **one row**: every operand is fed the merge's
  whole row (an equality) and its business functions are typed at it
  (L18). A shared variant output at `+→+` is one row every operand is
  typed at (an inclusive union could not be split back out under a
  typed hole). At `×→+` the variant output is **owned**: an emitter is
  closed to the case its label names, a `joined` source to the cases its
  label list names (`joined @l`, `joined @(l1 + l2)`), the block's row is
  exactly the union of its operands' cases — so an emitter block lists
  the events that can occur, and a case emitted twice, a handler for a
  case nothing emits and a case with no handler are all compile errors.
  The sharing is each merge's universal property (`Data.Profunctor.Row`):
  the pairing at `×→×` and `×→+`, the copairing at `+→+` and `+→×`.
  Variant handling MUST be disjoint, and so MUST production where the
  outputs are owned (a field at `×→×`, a case at `×→+`). Runtime evidence
  appears exactly where a merge's action is driven by labels (splitting
  a variant input, assembling owned fields) and nowhere else: the owned
  cases at `×→+` are only forwarded, so `OwnedVariantOutputs` carries no
  evidence, its disjointness checked at compile time (`DisjointLabels`).
- Merge operands' emissions MUST be runtime-exact: trimmed to their
  declared row before the gates combine them, so no stale sibling field
  can shadow a fresh value. Exactness is enforced by the merges, never
  left to convention.

### L5. Units are forced, not designed.

- A merge MUST NOT carry a unit of its own where a wire fits. The merge
  laws make a merge a commutative semigroup, no more (`Data.Profunctor.Row`,
  "The laws"; 2026-10-17); where `p` is a `Category` the wire at the unit
  object is the unit by construction — `lcmap (const {}) identity` at
  `×→×`, `identity @(Variant ())` at `+→+`, `lcmap case_ identity` at
  `+→×` — because a record gate MUST treat a contribution of zero fields
  as no contribution (L6). The one unit no wire reaches is `×→+`'s
  (nothing maps terminal → initial): it is the carrier's silent
  component, `PUI`'s private `silent`, named by no class (a `Silencing`
  class was tried and withdrawn, 2026-10-14). Silence is what an event
  source removed by `provided` observationally *is*, but NOT an operand
  `provided` is built from: copairing with silence leaves the source's
  emission channel connected (verified on the probe carrier), so absence
  is `Hosting`'s and `provided` stays a carrier primitive (deleted and
  restored on that argument, 2026-09-11). Pointing is `announce`, never
  a unit's: units carry no information and add none. Every starvation bug
  in the library's history was a `+`-behaviour where a `×`-behaviour was
  required; the units and the point make that a diagnosable error.

### L6. Knowledge gates: nothing flows until it is genuinely known.

- A gate MUST withhold until every field has a producer that has spoken,
  and MUST retain last-known values thereafter. An operand owning **zero
  fields is pre-satisfied** (its only contribution is `{}`, always known;
  the zero-field law in test/Main.purs) and **inert** (its emissions
  neither open nor re-fire the gate), so a wire, a silent display and an
  announcing one are one operand and the `{}` wire is the unit exactly —
  which is what makes a display-side operand unable to starve its
  siblings. There is **one gate**: the `×→×` merge and the container
  action's gather run the same pure machine (`PUI.Gate`), the merge
  enrolling its owned labels and `acted` the fed keys (the `+→×` merge
  has none; it is the copairing), so a gating rule proved on the step
  holds for both. A merge whose operands own no field enrols no
  participant, so each step that broadcast a shared record input MUST
  release it once (`StepEnded Shared`): a `{}`-output merge answers every
  feed, and a dispatched `+→×` step, owed nothing, stays quiet (before
  this clause `imagePane`'s merge of two displays never answered; the
  leaf-law bench found it).
- A gate MUST NOT be papered over with invented data. Three designs are
  permanently rejected: **type-derived seeds** (`Initial`/`Default` — a
  class invents wrong values with right types; initial state is a term),
  **auto-announcing leaves** (the union of leaf initials is a bogus first
  model), and **a `Maybe initial` slot in the wiring** (re-encoding the
  optionality the types eliminate). Any proposal whose effect is "gates
  open at registration" is wrong by construction. The gate is UX, not
  overhead: a summary withheld until its inputs are known.

### L7. Pointedness is a typing discipline, supplied by terms.

- A record input row is an initial-state obligation; `{}` is the one
  self-pointed record. Every vocabulary's `body` MUST demand a closed app
  (`PUI Web {} o`) and MUST feed it `{}` exactly once after registration.
  The knot MUST be primed by a term the caller supplies — a seed after it
  (`# looped # with seed`) or a load before it landed on `body`'s `{}`
  (`# landed`) — and an unprimed knot is a compile error at `body` naming
  the fields.
- An application MUST NOT have more than **one knot**, `looped`, around
  every line that changes the model, and a looped state is a model field
  (a variant knot or a per-shape knot over a hidden field was a second
  way of writing the loop with a state the rows did not show; 2026-10-06
  and 2026-10-14). Every line of the body MUST pass the row it is fed,
  so the event path MUST be written as one line, `( emitters; dialogs;
  folds ) # stepped` — a bare `emitters >>> folds` answers no feed and
  leaves the knot holding the row from before an edit above it. So no
  `looped` stands inside another, a body may hold several event lines,
  and a path that primes the knot is `landed`, not `stepped`. The point's
  *value* is row-forced (`announce a ≈ lcmap (const a) identity`); what
  the carrier adds is its *earliness*, so a proposal to derive `announce`
  from the wire is right about the value and wrong about the moment.
- An emitter MUST NOT be primed — no canonical first occurrence exists;
  an application's opening act is a model or `{}` into a load, never an
  event. Record-carried, event-natured channels (`select` before its
  first pick, `acted`'s withheld aggregate) stay unpointed.

### L8. Values on the wire are plain business values.

- No metadata rides the channel: no transiency flags, dirty bits or
  wrapper types on emissions. Temporal semantics MUST be derived from
  time itself (quiescence, the registration moment), never encoded in
  the payload. *Precedent:* the `New` continuity-`Boolean` wrapper that
  once rode every wire, dissolved by deriving `resolve` from time.

### L9. Identity is data; reconciliation is naturality.

- Collection identity MUST be materialized: a field `@l` of the element
  row, on the `×`-members and the `+`-members alike — never a DOM
  annotation, a render index, carrier-private guesswork, or anything
  read back from what an element shows (the `+`-members took a `{ key,
  value }` envelope until 2026-11-08).
- Identity MUST be unforgeable where the carrier owns it: in `acted` the
  element is typed at its whole row, key included, and the carrier
  re-sets the key on every emission. Stateful carriers MUST make identity
  follow the key: nodes move with their keys, matched elements re-feed in
  place. A key may also be shown where it is itself business copy
  (`tile @"Key entered" @"key" $ textAt @"key"`).

### L10. Direct wiring. No virtual DOM, ever.

- The library MUST NOT diff trees, re-render to reconcile, or reconstruct
  at runtime a correspondence the program already wrote down. A feed
  updates exactly what the wiring reaches; code order is DOM order.
- Fixed structure is fed as data through the retaining collection
  members (built once, updated in place via `text`/`attrWith`); wholesale
  rebuild (`dynamic`, `each`) is admitted only where structure genuinely
  varies with the data, and each rebuild owns its container.
- There MUST NOT be a markup DSL, a template language or an HTML string
  surface in the public vocabulary (`staticHTML` is internal chrome
  plumbing). UI structure is `PUI` values all the way down.

### L11. No global anything.

- No global store, dispatcher, message bus, subscription registry,
  context providers, portals or ref escape hatches. All communication
  travels the wires: composition, merges, and the sanctioned loops — the
  one knot, `looped`, and `debounced`'s seeded quiescence loop. State the
  application needs is a model field (L7, L14), never hidden; what a
  component keeps beside it is protocol, allocated in registration (L1):
  a leaf's replay of the row last fed, a gate's known slots, the knot's
  re-entrancy guard, `debounced`'s window. The per-shape knots that kept
  state the rows did not show were deleted for it (2026-10-14).
- A vocabulary MUST NOT act on the page at import time: page-level setup
  (a theme, a stylesheet, an icon base path, a typography baseline) is
  the work of its `body`, run at mount (the Fluent theme and Shoelace
  base path once ran at module load).

### L12. The vocabulary never forces a nominal type.

- Rows speak business labels (L3); options are anonymous records
  (`button' @l { icon }`); durations are `{ ms :: Number }`, never
  `Milliseconds`; unit payloads are `{}` (no `Default Unit` instance,
  deliberately); selector options carry variant-row values, never
  strings as enums (`labeled` was removed because it existed only to feed
  that antipattern).

### L13. Laws before API; failures have names.

- A combinator without laws stated in its module header is not
  vocabulary. Value-level laws run in `spago test` on probe carriers;
  carrier-only laws (DOM identity, live gates) in the smoke harness. The
  module headers are the single source of truth for contracts (`npm run
  api-docs` renders them); the six shape laws are stated once in
  `Data.Profunctor.Row` ("The laws") and what they buy an application in
  writing.md *What the laws guarantee*.
- Wrong programs MUST fail with names, as early as possible: compile
  errors where the types can carry the judgment (an unclosed app at
  `body`, a duplicated label via `DisjointLabels`); named runtime
  watchdogs where they cannot (gate starvation names the gate and the
  missing fields). A blank screen with no diagnosis is a library bug.
- **No silent information loss.** A combinator MUST NOT discard the
  emissions of the component it wraps unless they have nowhere to go: a
  display releases the row it shows, a pane releases what its content
  releases, the event path passes the row it is fed, a list over a
  stored array writes its elements' releases back (`actedAt`,
  `actedWithin`). Two slots discard by construction, because their
  content's release has no row to land in: `actedOf` over a derived
  array, and a `tile`'s or `listOf`'s content; an editor placed in either
  is a mistake the type cannot catch, so `npm run check-view-model`
  rejects an editor word inside an `actedOf` element. Nothing discards
  deliberately either — there is no counit; a chain whose output is not
  the model ends the block (`muted` went with the last slot that demanded
  `{}`, 2026-10-13).

### L14. The library stays small by rule, not by accident.

- **Demo-reachability:** every export is reached by a demo or a law
  test; what nothing reaches gets pruned. A vocabulary word a demo may
  yet reach — a catalogue entry carrying its laws — is unreached, not
  unreachable, and stays.
- **Three grounds delete a word**, each stated when it is used:
  **subsumption** — a new form makes an old one derivable, so the old one
  is deleted, not deprecated alongside (`synced`/`latch` into `looped`,
  `Sequencing` into `Acting`, `labeled`, `constantly`, `humanizeLabel`,
  the hidden-state words of 2026-10-11, the per-shape knots of
  2026-10-14); convergence is the health metric. **Narrowing and
  widening** — a word that shrinks a stage's input to a sub-row or grows
  its output by the leftover is a coercion, never vocabulary (a stage is
  typed at one row, L18): inlined where the library needs it and written
  by no demo; the model-side idioms are `group @l` and a pane. The one
  exception is a fact of the platform read beside the model:
  `timestamped @l` feeds a line its row with the time under a field the
  author names and releases the row without it, and `timestamping @l`
  adds the time to an event's payload — the time is no state of the app,
  so it is no model field, and a line that reads it follows the clock
  without the knot turning. **The current time is never a source**: a
  source owns a field of the model, so a clock among the sources made
  every beat a feed of the whole row (`clock @l`, deleted 2026-11-22);
  a stream of beats that carries the time is `ticks @l period #
  timestamping @t`. **Time measured is read the same way**, never
  counted: `timed @l`, `timing @l` and `timingEdits @l` are the three
  with the time run since the page opened, to the millisecond, for the
  wall clock — a moment the model keeps (a start, a lap, an edit) is a
  field, what has run since is read by the line that shows it, and no
  beat is folded into the model (timer, stopwatch, 2026-11-22). Neither carries `Lacks`, which is stuck on a
  row whose tail the view model fills (L18); the named field must be one
  the row does not have.
  **Unreachable by design** — a word whose only use another guardrail
  forbids is deleted even though it is lawful (a rename adopter, L3; an
  action on a `Maybe` field), and the catalogue exception does not apply.
- **State the app needs is a model field; an event folds in the app's
  loop.** A word MUST NOT keep application state the model does not hold,
  nor fold events into a row beside the loop's folds: a choice whose
  cases carry fields is a selector field with those fields beside it
  (order-form's Fulfillment); an event folds on the event path, landing
  on the model as it is then (order-form's estimate); a keyed collection
  of editors is `acted` (reorder); a reusable sub-form is a sub-record
  under `group @l` (parcel). Each once had a word that kept the state or
  folded beside the loop, and lost edits on a re-broadcast (2026-10-11).
- **Scope:** bambik owns model↔UI wiring and its algebra. It MUST NOT
  grow a router, an animation system, a theme engine, a CSS-in-PS layer,
  a state-management add-on or an HTTP client. Styling is ordinary CSS
  plus the design-system vocabularies; effects enter through the carrier
  (`action`) and stay at the edge. Anything in this list that ever seems
  needed must first be shown to be a corollary of the algebra.

### L15. Demos are the contract. They must keep working, whatever changes.

- The demo suites (demo/7guis/, demo/nguis/) are bambik's compatibility
  fixtures. A change to the library, however internal, MUST leave every
  demo compiling, bundling and **behaving correctly**, verified by
  running the full stack, never assumed:
  1. `spago build` — library, tests and all demos compile together;
  2. `spago test` — the value-level laws on probe carriers;
  3. `npm run bundle-demos` — every demo bundles;
  4. `npm run smoke` — in headless Chrome, every demo mounts clean, every
     published leaf keeps its laws on the leaf-law bench, and the
     carrier-only laws hold in the DOM.
  All four green is the precondition for merging anything. A demo that
  compiles but misbehaves is a failure the same as a compile error; the
  harness checks only what the laws cannot reach, never a page-by-page
  walk.
- A demo may change only when the demo itself is the subject of the
  change. A library change that forces demo edits is an **API break**:
  deliberate, documented, applied across all demos and docs in the same
  change — never a drive-by fix in one demo to get the build past.
- Coverage grows with the vocabulary and never shrinks: every new word
  lands with a demo reaching it *and* a law test or bench row asserting
  its behaviour.

### L16. The import tower: only the algebra layer touches the ecosystem's algebra.

Three floors, each greppable:

- **Algebra layer** (`PUI`, `Data.Profunctor.Row.*`,
  `Data.Profunctor.Keyed.*`, the optics) — the **only** importer of the
  ecosystem's `Data.Profunctor` and its `.Strong`/`.Choice`/`.Cochoice`
  siblings; the instances live here, so it is forced. `PUI.Web`, the web
  carrier, sits with it for its internals and may import the ecosystem's
  `Data.Profunctor` for the few reshapings it writes bare.
- **Vocabulary layer** (the design-system modules, `PUI.Web.HTML`,
  `PUI.Web.SVG`, packaged controls) — builds from the carrier
  (`fromHandles`/`handles`, `PUI.Web`, FFI) plus the row vocabulary, and
  never imports the ecosystem algebra: a design-system module proves the
  vocabulary complete by being its first customer. Checkable form: `grep
  "import Data.Profunctor (" src/PUI/Web/ demo/` is empty. **The floor is
  the namespace**: every web vocabulary is a submodule of its carrier,
  `PUI.Web.*`.
- **Application layer** — vocabulary only: no `Data.Profunctor`, no
  carrier internals (Part II; writing.md's *Wiring*). `PUI.Web` holds
  both — its internals (`element`, `Node`, the DOM FFI, `staticHTML`) and
  its element-neutral vocabulary (the decorators, `text`/`staticText`,
  the panes, the sources, `dynamic`/`each`/`el`) — and an application
  imports the second and never the first.

The consequence is the **mechanism-argument doctrine**: a projection is
an argument of the mechanism that consumes it, never a loose
`lcmap`/`rmap` stage (`provided @l classifierOf`, `actedOf @l rowsOf`,
`settled normalize`). A shape none fits is a missing-vocabulary signal
addressed to the library, never a reason to import one floor down.
Business optics (`Shutter`/`Reel` below the UI) are algebra-layer
material and exempt by location.

### L17. Copy is a function, not a field.

- What a pipeline operates over is the app's **state**, never its
  rendering. A display whose content *is* copy MUST take a **read
  function** (`text :: ({ | r } -> String) -> PUI Web { | r } { | r }`)
  and carry no label; the function MUST be a named function of the view
  model module; a field shown as stored is named by its label (`textAt
  @l`, L3); a formatter bracket, a view-side lambda and a
  `staticText`-plus-leaf text run are forbidden; there is no read
  adopter. A display that renders a **number** (`linearProgress`,
  `progressBar`, `gauge`, `ratingDisplay`) MUST take a read function too
  — a fraction is derived, and derivation is formatting — and only that:
  what a bar measures is no field, so it carries no label and no
  accessible name (a known gap; a name would come back as a String
  value, never a label). Quantity *editors* keep label-as-field.
- A model field MUST exist because the app's state needs it, never
  because a display wanted a `String` or a `Number`. `settled` therefore
  maintains invariants among **edited** fields only; a `present<App>`
  normalization feeding a display is a violation. Checkable forms: every
  `# settled` in `demo/` sits on an editor stage; `npm run
  check-view-model` rejects any lambda in a `text` read.
- A status **tells its case**: label-indexed at its business case, it
  takes no copy function and its label is its whole copy (`snackbar
  @"Card charged" captured`). Copy a status would compute from its
  payload is either data the user must keep (a model field shown by a
  display) or a reason (a sibling case with its own status); so an
  action's outcome row names its outcomes as the words the user reads,
  and a command straight from an emitter folds silently. Its face is
  private to the vocabulary, adopted as its case by `atCase @l`. A
  message that stays while a state holds is a surface over a display
  under a pane (MDC2's `banner`), not a status; HTML's `output` is the
  plain floor's status.
- The gain is the point: the screen's copy is a pure function under
  `spago test`, and the view line names its own writer. Application side:
  writing.md's *Copy is a function, not a field*.

### L18. A view determines its view model's interface up to the tails.

- **A business function is typed at the stage's row — no `Union`.** A
  read function, a handler, a classifier, a normalizer and an action's
  `Aff` all take the row the stage is fed, and its signature is **the
  view's hole hint with its tails filled** (`Record t0 -> String`
  reported, `{ counted :: Int } -> String` written): checked by
  unification, never stuck on a hole as a `Union small rest big` is. An
  open-row footprint narrowed by hand from the hint was tried and
  withdrawn — the hint never suggested the quantifier (2026-10-03). A
  name two lines call at different rows is generalized to what they
  share, and only then carries a `forall`. An emitter block's output is
  the union of its operands' cases, so the folds after it must own
  exactly those; an action widens nothing, its followers naming its
  outcomes (its own `Union t b o` was stuck on a hole's `t` until
  2026-10-16); a `joined` source is closed by the labels it names.
- **No `Eq`/`Ord` a business type must close.** Options and keys are
  compared structurally (`Data.Structural`, read through `Foreign`), so a
  type only a logic function names leaves nothing to solve. The rule
  tightens the library: an exported signature MUST NOT carry a
  constraint only a concrete row or type can discharge (`listOf` once
  passed `Union r () r` on to every caller).
- **Splits at labels the view names.** The knot splits nothing: `looped`
  re-feeds the whole row; a sub-record is focused by the field that holds
  it (`group @l`).
- **A field is named, never picked** (application side: writing.md's
  *The anchor invariant*). A line that reads one stored field MUST name
  it by its label in the reading word's own slot, and application code
  MUST NOT write an accessor section (`_.l`; `npm run check-view-model`
  rejects one). So every word taking a read function that an application
  feeds a stored field has a **label sibling**, `…At` (`textAt @l`,
  `attrAt @l name`, `providedAt`/`inCaseAt @case @field`,
  `actedAt`/`gatheredAt @key @field`, `listOfAt @l @key @field`); the
  function form stays for what is derived. A word whose every reach reads
  one field takes only the label (`listOf @l @k`, `tile @l @k`). A new
  reading word MUST come with its label sibling as soon as a demo reads a
  stored field through it, and not before (L14).
- **The view determines the interface up to its tails.** A view compiled
  with a typed hole in place of every value it imports MUST report each
  hole at the type the view gives it, and that type MUST be the view's
  alone: the **skeleton** — which holes share a row, what each returns,
  every field and case a line names at the type its leaf gives it — with
  the **tails** (the fields, cases and payloads only business functions
  reach) left unknown. The tails are the model: many models fit one view,
  and the view model module's signatures are the hints with the tails
  filled, one filling per row. What it demands of the library: a word
  MUST NOT take, as a visible type argument, a row its function or value
  argument already brings (L3), with no exception, and an option block
  MUST close its row. What it demands of the application: a stored
  variant field is named by label on its pane line, a view helper applies
  one view model function per value, the view writes no row (`npm run
  check-view-model`). Rows were declared on the view line (`looped @( …
  )`) until 2026-10-15.
- **Checkable form**: `npm run check-determined` compiles every demo's
  view with a typed hole for every imported value, reads the hole list
  back, and checks each exported signature is its hint's instance — by a
  probe module where the hint has unknowns — failing on an error that is
  not a hole, a signature that quantifies, or an export no view imports.
- **There is no runtime hole.** Until 2026-11-12 every view also ran with
  its view model stubbed to a runtime `hole`; it was removed with its
  sweep, and the compiler's typed hole is the only one.

### L19. What is drawn leads; what is wired trails.

- Every view line is written as **the element it draws, then its data
  words**: `uiElement # plumbing` (writing.md, *Formatting*). The library
  MUST make that form available for every word it publishes: the API is
  shaped by the line, not the line by the API.
- A word that **draws nothing** — a pane, a collection, the path's
  landing (`stepped`, `landed`), the knot and its seed (`looped`,
  `with`), a normalizer (`settled`), an action over its progress face
  (`action`, `load`), a join (`joined`), the four words of the square
  (`introduced`, `supplied`, `continued`, `acknowledged`) — MUST take the
  stage it acts on as its **last** argument and be written trailing,
  `stage # word …`, and MUST NOT be published in a form that can only
  lead the line.
- A word that **draws** — an ocular, a component whose argument is the
  content it frames (`group @l`, `confirmed @l`, `modal @l`, `listOf @l
  @k … item`), a `body` — leads with `$`. A decorator rides its element
  (`>>>` or trailing `#`) and never leads. The one exception is `static
  (…)`, whose ocular argument is polymorphic and cannot pass through
  `$`.
- When a behaviour cannot be written trailing, the fix is in the leaves:
  give the element the behaviour (as displays became stages), or close
  the leaf so a trailing word can read what it needs off it (as
  `acknowledged` reads its case off `modal @l`). *Precedents:*
  `prefixed`/`suffixed` deleted two days after they were added
  (2026-10-13); `acknowledged @l content` became `( modal @l content ) #
  acknowledged` and the nineteen `…From` editors `editor @l # introduced
  @l init` (2026-11-16).
- **Checkable form**: none yet; review enforces it.

### L20. Design systems are interchangeable.

- One view model runs under every vocabulary (`PUI.Web.HTML`, `MDC2`,
  `MDC3`, `Shoelace`, `Fluent`, `Bootstrap`), and a twin over another
  design system MUST be a view-only diff. Wherever two catalogues have
  the same concept, their vocabularies MUST give it the **same name and
  the same signature**: the same label arguments, the same row, the same
  primed siblings.
- A catalogue's differences MUST appear **under its own names, never
  papered over**: its renames as the catalogue makes them (`headlineLarge`
  where MD2 has `headline4`; Shoelace's `toast`), its exclusives
  (Shoelace's `rating`, Fluent's `messageBar`), its gaps left as gaps
  (MDC3 has no `banner`; Bootstrap no dialog). An app needing a missing
  leaf has no twin in that vocabulary; a twin MUST NOT build the leaf
  from a bare element (the twelve twins deleted with `clicked`,
  2026-10-15).
- Every vocabulary MUST keep the leaf protocols identical, each leaf
  built over the shared wirings, and is entered through its own `body`,
  of the plain one's type, which does that catalogue's page setup at
  mount (L11).
- **Checkable form**: twins share their view model module verbatim, so a
  signature that differs fails `spago build` for every twin; the
  leaf-law bench (demo/laws/) checks each vocabulary's leaves against the
  same shape laws on the real DOM. Agreement of names elsewhere rests on
  review.

---

## Part II — The approach (applications built on bambik)

The rules for application code are stated once, in the authoring
skill's [writing.md](../.claude/skills/developing-bambik-apps/writing.md),
written from the application developer's perspective and shipped with
the skill; this document governs the library and restates none of them.
The library's obligations to it:

- The demos are the executable form of that contract — L15 makes them
  the compatibility contract, gate 5 makes reachability a merge
  condition. A demo that breaks a rule in writing.md is a broken demo.
- L16 fixes the import tower whose top floor writing.md states as
  "never import `Data.Profunctor`": `grep "import Data.Profunctor ("
  demo/` is empty, always.
- A shape no mechanism fits is a **missing-vocabulary signal** addressed
  to the library, answered by admitting vocabulary through the gates
  below, never by relaxing a rule in writing.md.
- When a library change alters what application code should look like,
  writing.md is the file that changes, and the demos change with it.

---

## Admission gates

Any proposed change — combinator, class, component, demo idiom — passes
these gates in order:

1. **Derivation.** Locate it in the algebra: an instance of a standard
   structure, a row form of an existing strength, a corollary of
   closure. If it needs a new concept, prove irreducibility (the
   `looped` standard); otherwise it stays out.
2. **Laws.** State its laws in the module header before writing demos
   against it; land value-level tests on the probe carrier, bench rows or
   smoke tests for carrier-only behaviour.
3. **Subsumption sweep.** Ask what the new form makes derivable — and
   delete that. A feature that only adds is suspect; the best features
   shrink the library.
4. **Honesty audit.** No instance a carrier can't support, no default a
   type can't justify, no metadata on a wire, no name for a concept the
   ecosystem already names (coin only where the literature is silent).
5. **Reachability.** A demo or law test reaches it, or it doesn't merge.
   Exactly one nominal type (cells' `Expr`) survives in the demos, and
   that ratio is a regression test on this whole document; type synonyms
   are not nominal (L18), and the law bench's `Bench` newtype is harness.
6. **Green stack.** The verification suite of L15 passes in full, with
   no demo edited except where the demo is the subject. A red smoke is a
   veto, not a nuisance.
7. **Sync.** Library rules are stated in the module headers and this
   document; application rules **only** in writing.md. A change that
   alters how applications are written edits writing.md and the demos,
   never a second copy here or in CLAUDE.md, which carry pointers. The
   deployed restatements are exactly two, both deliberate (deployed HTML
   cannot read the skill): the demo pages' code-style note and
   demo/workflow.html (*Writing order* replayed as a session, its compiler
   responses verbatim); re-read both against writing.md when the
   contract changes.

The historical record is the proof this process works: every rejected
design (the type-derived seed, `Sequencing`, the `New` wrapper,
`synced`/`latch`, `labeled`) is recorded here with its reasons, and every
survivor carries its laws. Guardrails are cheap to state and expensive to
recover once breached — this document exists so no future convenience,
contributor or agent trades a rule for a feature without noticing the
price.
