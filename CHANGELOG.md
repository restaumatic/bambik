# Changelog

What changed in the library and when, newest first. The module headers
and doc/guardrails.md state the rules as they stand; this file holds the
history they used to carry inline — what was added, what was tried and
withdrawn, and the date — so a rule can be read without its obituary.
Before 2026-07 the history is the git log.

## Unreleased (branch experiment/scope)

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
