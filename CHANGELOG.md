# Changelog

What changed in the library and when, newest first. The module headers
and doc/guardrails.md state the rules as they stand; this file holds the
history they used to carry inline — what was added, what was tried and
withdrawn, and the date — so a rule can be read without its obituary.
Before 2026-07 the history is the git log.

## 2026-10-13

- **A fold's status shows the event, never the fold's result.** `fold f
  status` feeds the status the occurrence as it arrives, so its copy
  function is typed at the payload; since 2026-10-07 it had been fed
  `f` of the payload under the same case, which put a second row
  variable and a second `Cons` in the signature. Whatever a status says
  must therefore be in the event: a status with copy stands after a
  `+→+` block that returned what it tells (crud's actions, payment,
  ticker reading the fetched price), and a command straight from an
  emitter folds under `blankStatus`, since its result is already on
  screen. Counter, ticket-dispenser, cashbox, checkout, inbox and
  shopping-cart drop their snackbars (and their copy functions);
  writing.md's *A status only where a real app would tell* (backend
  outcome, money moved, destructive action) becomes *A status only after
  a `+→+` block*.
- `blankStatus @l` is renamed `unshown @l`: it owns a case and shows
  nothing, whether the case comes from an emitter or a `+→+` block, so
  "status" claimed an origin it does not have.
- Demos: order-form's load is told by a snackbar (`orderLoadedLine`,
  read from the loaded order); espresso-bar's Place order runs a `+→+`
  action, `placeOrder`, returning `"Order placed"` with a ticket
  (`{ number, "Your name" }`) its snackbar reads, in place of a snackbar
  that restated the model; the order resets the form,
  `# reset usualOrder`. Demo views use 2-space indentation steps
  throughout.
- **`reset x`** is `fold (const x)`, opened by its status like `fold`
  (`unshown @"New game" # reset openingPosition`); every `fold (const …)`
  in the demos (tic-tac-toe, quiz, shopping-cart, espresso-bar) becomes
  a reset.

## 2026-10-11, last

- **`bracketed`, `updated`, `applied`, `every`, `edited`/`editedAt` and
  `subStrong` are deleted.** Each kept application state the model did
  not declare or folded events beside the app's loop, and each now has an
  idiomatic replacement (guardrails L14, *State the app needs is a model
  field*):
  - `bracketed` kept the other cases' payloads of a variant field inside
    the editor, so every re-broadcast re-seeded them from defaults —
    order-form's takeaway time was lost on a trip through another tab (a
    retaining fix, earlier today, is withdrawn with the word). A choice
    whose cases carry fields is now a selector field and every case's
    fields side by side in the model (order-form's `Fulfillment`: `Mode`,
    `Table`, `Time`, `Address`, `distance`), each edited
    `# inCaseAt @c @"Mode"`; `fulfillmentState`/`fulfillmentCase` and the
    law test of their round trip are gone.
  - `updated`/`applied` folded events in place. Order-form is now one
    loop of the four shapes around its form's own `# looped`: Estimate
    distance stands with Submit and Receipt (shown only for Delivery, `#
    provided @"Delivery" … deliveryAddress`), its button and action
    `joined` together so the outcome is paired with the model it lands
    on, and `blankStatus @"Distance estimated" # fold setDistance` sets
    only the distance — an edit made while the estimate ran is kept (the
    `distanceEstimate` sub-form of the entry below is gone). The summary's
    `# debounced` went with it: it delayed only the row the buttons
    replay, so a click within the window estimated a stale address.
    Photo-gallery's drawer nav is the list, `joined`, and
    `blankStatus @"opened" # fold openAlbum`; `openAlbum` takes `{ event,
    model }`. `every`, derived from `updated`, was unreached.
  - `edited` duplicated the container action for editors: `acted @"id"`
    gives back the whole array on every feed and every edit (reorder), and
    `actedAt @key @field` (`Data.Profunctor.Acting`) replaces `editedAt`
    for the plain-HTML twin.
  - `subStrong` lifted a closed-row sub-form into a flat row: parcel's
    address is now a sub-record, edited by `group @"Address" addressForm`.
- **`drawer` (MDC2, MDC3) feeds its nav and its content the same row**,
  and its output is the nav's: before, the content was fed only what the
  nav released, which is why a nav had to fold its pick in place.
- **`each` is built at registration**, like every static: its list is
  known up front, so it is on screen before any feed (photo-gallery's
  favorites trail the nav's fold, which releases only on a pick).

## 2026-10-11, later

- **Order-form's distance estimate is a reusable sub-form.**
  `distanceEstimate`, over its own row `{ "Address", distance }` and placed
  `# subStrong # inCaseAt @"Delivery" @"selected"`, holds the four shapes:
  the address editor, the Estimate distance button, the `estimateDistance`
  action (its `Aff` now returns the row under `"Distance estimated"`) and
  `blankStatus @"Distance estimated" # fold identity`, closed by `updated
  setDistance`; the view's `match { estimated: setDistance }` dispatcher is
  gone, and the estimate's functions are typed at the sub-form's row instead
  of the Fulfillment editor's. It is not closed by `looped`: a stage must
  echo every feed, and a loop of the four shapes releases only on its
  events, which would starve the Payment and Kitchen groups after it.
  `setDistance` applies the returned estimate to the latest row, since the
  memoryless fold releases the row the button replayed and would revert an
  address typed while the estimate runs. `settled staleDistanceForgotten`
  moved from the address editor to the end of the sub-form, so it also
  covers the estimate's release. Before, an address edited during the
  estimate kept a stale estimate until the next edit.
- **The demos' view model tests are gone.** The six `<Demo>ViewModelTest`
  modules (counter, temperature-converter, flight-booker, todo-list,
  checkout, order-form; added 2026-10-07) and their run at the head of
  `spago test` are deleted, and the pages no longer list them: testing
  an app's business functions demonstrates nothing about the library.
  writing.md keeps the one sentence that the laws leave business
  functions to the view model's own unit tests.
- **Review residue.** `splitRow` (`Data.Profunctor.Row`): `subStrong` and
  `subResolving` feed their focus exactly its row and keep the background
  apart, instead of passing the fed record twice under a coercion;
  `replaying`'s input coercion, an identity, is gone. `action`'s outcome
  widening stays a coercion: the solver cannot discharge a `Union` whose
  open tail is on the left. `Hosting Web` places the **run of nodes** an
  element's builder appended (`Array Node`), so a bare group of siblings
  or a `blank` element detaches and restacks whole; before, the parent's
  last child stood for the element. Covered by reorder-html, the
  reorder demo's plain-HTML twin — a `dl` of bare `dt`/`dd` pairs, with
  `dl`/`dt`/`dd` added to `PUI.Web.HTML` and `editedAt @l @t` to `PUI`
  (`edited` over a stored array field, `foreachAt`'s sibling) — walked
  by scripts/smoke/tests/node-runs.mjs. HTML's `button` no longer disables
  itself on click (every other vocabulary's stays live). `filledTextArea`
  defaults `columns`/`rows` (40 by 4) instead of demanding them through
  an options record whose error named neither.
- **Scopes.** `PUI` gains `Scope` (`Active`/`Suspended`/`Cancelled`,
  nesting, finalizers) and `class Scoped m` (`Web` keeps the scope in its
  builder state, `Effect` reads an ambient one under `inScope`). A pane
  builds its content in a scope it suspends while detached and resumes
  on attach; a collection's leaver and a `dynamic`/`each` rebuild cancel
  theirs. `ticks` skips its beats while suspended and ends its loop once
  cancelled; an `action` whose outcome settles in a suspended or
  cancelled scope drops it (the slot is still told `ended`); `separating`
  disconnects its observer on cancel. So the pane law — a detached
  emitter is `silence` — holds for the emitters that originate
  occurrences themselves, and a removed subtree stops acting. `Hooks`'s
  `instantiate` returns a `cancel`; `ticks`, `every` and `action` need
  `Scoped m` in place of `MonadEffect m`.

## 2026-10-11

- **Shared leaf layer.** `PUI.Web` gains the leaf protocol as functions:
  `editorWiring` (write unless held, echo every feed, report the
  platform's change through a busy guard), `selectorWiring` (the same over
  an option list, in index terms, echo left to `selectedAt`),
  `displayWiring` (render, answer `{}`) and `statusWiring` (render,
  release nothing) — plus `eventLeaf`, `eventText`, `listenNode`,
  `listenNodeDeferred`, `ensureStyle`, `autoDismiss` and the option tags
  `OptLabelIcon`/`OptLabel`/`OptSelected`/`OptIcon`. Every leaf of every
  vocabulary is one of the four wirings over its own element; the eight
  focus-guarded text leaves, nine boolean leaves, seven displays, six
  select cores and five indeterminate bars that each carried the protocol
  by hand are gone, with the four `listenNode`, two `ensureStyle` and
  three `autoDismiss` FFI copies (`Bootstrap.js` is deleted).
- **Escaping.** `PUI.Web.escaped` and every runtime string a vocabulary
  interpolates into `staticHTML` markup — option labels, captions,
  titles, tips, image sources — passes through it.
- **Soundness.** `widenRecordInput` (an unconstrained `lcmap unsafeCoerce`)
  is deleted; `atAnyRow = lcmap (const {})` places `{}`-input chrome at any
  row, and `SharedRecordInputs` is the three-way row equality itself,
  with `fedFirst`/`fedSecond` as the witnesses the merge instances feed
  their operands through. `armed` (the identity under a name) is deleted.
  `updated` loses its vestigial `unsafeUnion`/`unsafeCoerce`; `text`'s
  callback slot is a `Maybe` like every other leaf's. The `type-equality`
  dependency goes.
- **Separators.** `separating` (`PUI.Web`): a rule shown only between
  things, decided on the DOM by a `MutationObserver`; every vocabulary's
  `divider` and HTML's `hr` are `separating` over their rule.
- **Displays as oculars.** `prefixed d w` / `suffixed d w` in `PUI`;
  `shown`, `shownWhen`/`shownWhenAt` and `shownEach` deleted; the display
  pane is `pane`/`paneAt`. A bambik-owned `Pipeline.do` that inserted
  `prefixed` itself was built and withdrawn the same day: application
  code keeps the ecosystem's `Semigroupoid.do`.
- This file; the dated sentences in the module headers and guardrails
  moved here.

## 2026-10-10

- `RecordUpdate.do` (experiment of 2026-10-07) removed as not idiomatic.
- A field is named, never picked: `textAt`, `attrAt`, `providedAt`,
  `paneAt`, `inCaseAt`, `foreachAt`, `listOfAt`; `clicked @l @k` names the
  field a click reports; `npm run check-view-model` rejects accessor
  sections in views.

## 2026-10-07

- `fold` is opened by its status (`snackbar @"booked" line # fold f`) and
  moves from `Data.Profunctor.Row.VariantToRecord` into `PUI`, derived as
  the wire and the status under the `×→×` merge.
- The `+→+` merge's output is one row every handler is typed at (an
  equality), no longer the inclusive union.
- `ticks` is opened by its status (`blankStatus @"tick" # ticks period`).
- `staticText` takes a `String` value (before, a type-level `@"…"` with
  `staticString` beside it); a visible `@"…"` names a row label and
  nothing else. The indeterminate progress bars carry no label, and every
  vocabulary has `indeterminateLinearProgress`.

## 2026-10-06

- One knot: `looped @( … ) # with seed`. The variant knot `cycled`
  (2026-10-05) and `fold`'s second row argument are deleted; a load action
  stands before the knot (crud, order-form); nine redundant `# with {}`
  lines dropped.

## 2026-10-05

- `mvu`, `feedback @l`, `folding @w @l`, `unfolding @w @l` and `iterate`
  deleted: a looped state is a model field. `with` seeds at any type.

## 2026-10-04

- The `+→×` merge is the copairing: a shared output row, each operand
  releasing the whole row, no gate. `joined @l` (an event joined with the
  row its emitter was fed). `updated`, `applied` and `every` become
  library-level. A whole-variant `fold (match …)` lasted the day.

## 2026-10-03

- View model signatures are the view's hole hints verbatim (closed rows,
  no open-row footprints); `npm run check-determined`. The `state` leaf
  and `handler @l f` (2026-10-02) retired. Type variables name their kind
  (`r` record row, `v` variant row, `b` background).

## 2026-10-02

- Every demo has a `<Demo>ViewModel` module the view determines.
  `choice @l` and `<+>` close a selector's option row.

## 2026-10-01

- Determination pilot on inbox; panes take the classifier's variant as a
  visible type argument.

## 2026-09-30

- Holes against the real library (guardrails L18): every view runs with
  its logic stubbed to `hole`; shadow `.Holey` modules and free rows
  rejected on measured branches.

## 2026-09-28

- The blind `card` is for content that edits nothing; anchors sit in
  their own position; `imagePane` takes a read function.

## 2026-09-27

- Business functions are arguments of leaves, never adopters: `toCases`,
  `forCases`, `forField`, `asCase`, `forProperty`, `atField`,
  `backgroundProperty`, `recordToCase`, `focusCase @l @w` and
  `backgroundCase` deleted. `updated`, `observed` and `edited` derived from
  the vocabulary; `PointedCostrong` added; a word lives in the module of
  the sides it constrains.

## 2026-09-26

- Selectors are editors in three words (plain, `…Unpicked @l @c`,
  `…Optional @l @c @n`), undoing the 2026-09-23 picker split. The per-demo
  smoke walks are deleted; the laws cover them.

## 2026-09-23

- The pane family by content shape: `provided` (emitters), `shown`
  (displays, now `pane`), `inCase` (editors) over `attachable`. The
  element-neutral vocabulary moves into `PUI.Web`. The scalar
  `radioButton` is deleted. A gate with no participants releases once per
  shared step.

## 2026-09-15

- `body` feeds `{}` once; `Seeding` restated (`announce` the primitive,
  `seeded` derived); `clicked` is `replaying` of a click source; `every`
  derived from `ticks`; `bracketed` lifts into its field; one pure gate
  (`PUI.Gate`) shared by the record merge and `acted`, checked
  exhaustively (begun 2026-09-14).

## 2026-09-13

- Progress indicators are statuses, `[ started, ended ] → {}`.

## 2026-09-11

- `silence` deleted and restored: an absent emitter is `silence`
  observationally, but absence is `Hosting`'s, not a copairing.

## 2026-09-06

- Every vocabulary is entered through its own `body`; nothing acts on the
  page at import time.

## 2026-09-05

- `focusField` is no longer re-exported; the HTML floor's `input` and
  `textArea` are lifted inside.

## 2026-09-04

- `group @l`, the labelled model group (MDC2/MDC3).

## 2026-09-03

- A label on an ocular rejected: no model, nothing to anchor.

## 2026-08-31 and 2026-09-02

- `projection` and `projected` deleted: copy is a function at the leaf.

## 2026-08-27

- Identity is the exact unit of both diagonal merges; `pempty` gone,
  `Joining` deleted.

## 2026-08-26

- `informed` deleted, `applied` added; approachability sweep.

## 2026-08-25

- `completed` and `focusProperty` deleted.

## 2026-08-13

- Fused dispatch (`handled`/`FactsFold`) rejected.

## 2026-07-29

- Pointedness merged: `Seeding`, `body`, `with`.
