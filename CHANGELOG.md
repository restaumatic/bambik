# Changelog

What changed in the library and when, newest first. The module headers
and doc/guardrails.md state the rules as they stand; this file holds the
history they used to carry inline — what was added, what was tried and
withdrawn, and the date — so a rule can be read without its obituary.
Before 2026-07 the history is the git log.

## 2026-10-16, latest

- **Answer's `×→×` half is a leaf law; its `×→+` half is a law of every
  stage.** The headers and doc §3 stated Answer per shape, as if any
  `×→×`-typed stage answered its feeds, and listed `confirmed` and the
  gather gate as the only two places liveness is refined. But
  `emitter >>> fold` — every app's loop body — is `×→×` by type and
  releases on occurrences only, so it contradicted every such statement
  while being exactly what `looped` is for. Audited what the
  stage-level reading had been carrying: transport and gate priming
  need only leaves to answer (demos already order what must show at
  mount before the loop's emitters); "every field fresh" was already
  false for sources, which answer with the platform's value, not the
  feed's; the denotational claim rested on Repetition, not Answer; and
  "loops settle" was owed to the `×→+` half (a feed never fires an
  emitter), not the `×→×` one. So the law is now stated with its reach:
  the `×→+` half is `>>>`-closed and read off the type, the `×→×` half is
  owed by leaves, kept by the merges, by `>>>` of answering stages and
  by the `×→×` words, and not owed by a route through a variant, which
  owes only row totality — a stage answers iff its route stays at `×`.
  Preservation at `×→×` is conditional on its operands answering (the
  no-torn clause is not). Restated in `Data.Profunctor.Row` "The laws",
  `RecordToRecord`'s laws, doc §3 law 2 and 3, §3.1 and §3.3,
  writing.md *What the laws guarantee* ("a change arrives whole" keeps
  once-and-untorn and drops "every field fresh"; "a blank pane" gains
  the line-after-an-event case) and CLAUDE.md. No code or test changed:
  the tests exercise leaves and merges of leaves, where the law reads as
  before.
- **`action @l f`: an action takes one label; its outcomes are closed by
  what tells them.** `action @l @t` was the one word taking a visible
  row, the outcome labels it needed to widen itself into its block
  (`Union t b o`, stuck on a hole's `t`). Measured: the widening is
  needed only where an action stands among emitters in a
  `RecordToVariant.do`; everywhere else the statuses or capture after it
  own exactly its cases and close its row. So `action` is typed at
  exactly its outcomes, like `load`, and the widening moved to the word
  that already names the labels: `joined @l` now **closes its source**
  to case `l` (`Cons l e () v`) and widens the joined case into the
  block's row — before, it passed the source's other cases through,
  which left the rest of an exact action's row open to its siblings.
  One label closes one case, so a chain of `joined`s cannot close a
  two-outcome source; that is `joined2 @l1 @l2` (order-form's submit).
  flight-booker, whose two outcomes were `joined` only so their statuses
  could fold `kept`, now has `submit` return the trip under each case,
  folded `identity` — no join, its statuses closing the row
  (`bookFlight`, a stub that always booked, is inlined). The view writes
  no row at all now, and `check-view-model` admits no `@( … )`. Real
  build, holey sweep and `check-determined` green on every demo; the
  hole hints are unchanged (demo/workflow.html's re-captured responses
  differ only in columns).

## 2026-10-15

- **The view writes no row; L18 determines the interface up to the
  tails.** L18's converse said a view compiled with typed holes reports
  every hole "at a concrete type with nothing unknown", so the hole list
  *is* the view model's signatures. It held only because the view
  declared the model: `looped @( counted :: Int )`, `with @{ … }`, a
  classifier's cases on its first pane (`inCase @l @( … )`), a
  projection's element row (`foreach`/`anyOf`/`anyEdit`/`listOf @… @(
  … )`), `dispatched`/`accumulated @k @a`, `each @a`, `load @( … )`.
  Each repeated a signature the view model module states anyway. Without
  them the compiler reports what the view really fixes, the
  **skeleton** — which holes share a row, what each returns, every field
  and case a line names at its leaf's type — and the rest of each row as
  an unknown, the **tail** (counter: `Record t0 -> String`, `Record t0 ->
  Record t0`, `Record t0`). The tails are the model: many models fit one
  view (a next id stored or derived, a phase one variant or two fields),
  so a view does not determine *the* model, and the module fills each
  tail once. All those visible row arguments are gone from the library
  (`looped`, `with`, `load`, `inCase`, `provided`, `foreach`, `anyOf`,
  `anyEdit`, `acted`, `dispatched`, `accumulated`, `each`, `listOf`/
  `listOf'` in MDC2/MDC3) and from every demo, 163 rows in all. This
  reverses the morning's entry below for `load`: the case a capture takes
  is named by the capture and typed by the `Aff`. The one row a view
  still writes is an action's outcome **labels**, every payload now `_`:
  the widening `Union t b o` is stuck on a hole's `t`, so the holey
  build (L18's first half) needs them — tried without them, it fails. The
  real build and the holey build both pass with no other declaration.
  `npm run check-determined` now accepts unknowns and checks each
  signature is an **instance** of its hint: verbatim where a hint has no
  unknown, otherwise by a probe module that applies a function typed at a
  declaration's hints, their unknowns quantified, to the module's values,
  so the compiler decides that one filling of the tails fits; a
  quantified signature still fails. `npm run check-view-model` rejects a
  restated row in any view, an action's `_`-payload outcome labels
  excepted. guardrails L3 and L18, writing.md (*Types and values*,
  *Business functions*, *Writing order*), the skill's other files, the
  module headers and demo/workflow.html follow; the workflow's compiler
  responses are re-captured with the pinned compiler, the first now
  showing `| t0` and the second showing it gone once one signature fills it.

- **`load` declares its outcomes on its line.** `capture @"People loaded"`
  (crud) and `snackbar @"Order loaded"` (order-form) consumed a case
  nothing in the view produced: the label arose only in the logic's `Aff`
  type. The load line now names it, `load @( "People loaded" :: _ )
  loadPeopleCatalogue`, exactly as `action @l @t` declares its outcomes,
  so every case a view consumes is named where it arises in the view.

- **`clicked` deleted; `tile` (MDC3) and `anyEdit` added; the twin matrix
  follows the leaves.** `clicked @l @k` made any element clickable — a
  wrapper that was neither a leaf nor an ocular, discarded its content's
  release, and stamped no role, name or tab stop on the `div`s and `td`s
  it listened to. The protocol it carried stays as plumbing every emitter
  is built from (`PUI.Web.occurrences`, `replayed @l f`, `clickedRow`,
  `eventLeaf`), now exported to the vocabularies, which build their picks
  on it: `listOf @l @k` (MDC2/MDC3) and the new MDC3 `tile @l @k content`
  — one clickable cell of a grid, a real `<button>` with its role and tab
  stop, its content its accessible name, styled outside by `attrWith`.
  Movie-browser's `iconToggle … # toCase @"Favorite changed" # anyOf` was
  the one application `toCase`, and an echo masquerading as an event
  (every editor echoes its feed; the knot's re-entrancy guard was
  swallowing them): the editing list `anyEdit @l @k proj editor` (`PUI`)
  keeps the element an editor and emits a user's edit as case `l`,
  dropping an element's answer inside the list's own feed, so it is
  `×→+` by behaviour. `PUI` re-exports `settled` and nothing else of the
  floor's single-label words. By decision, the missing leaves are not
  added to every vocabulary: a twin exists only where its vocabulary has
  the leaves its app needs, so crud keeps its Material twins (`listOf`),
  cells, tic-tac-toe, calculator and color-mixer are MDC3-only (`tile`),
  and twelve twins are deleted with their page templates and registry
  entries (95 demos on 43 pages). Shopping-cart's remove is an
  `iconButton @"Unit removed"` in the row, `joined`, its fold reading the
  row's product. The HTML law bench no longer benches `clicked`; MDC3's
  benches `tile`.
- **Every leaf narrows its input and widens its output itself; no
  adopter on a view.** `action` is the `+→+` leaf of its button's case
  and declares its outcomes: `action @l @t f` (`Cons l a () v`, `Union t
  b o`), the labels at least — a payload the status or fold after it
  fixes is written `_`, one nothing fixes written out — widened into the
  row its block shares by the action itself. The `# atCase @l` after
  every action (30 lines) and the `# expanded` beside sibling emitters
  (order-form, ticker; 7 lines) are gone; `expanded`, one day old, is
  deleted. Measured first: a `Union` on an undeclared outcome row is
  stuck under a hole, and an instance chain that would equate the rows
  when nothing is declared cannot fall through (a fundep-determined
  mismatch is a type error, not apartness), so the declaration is the
  price of a self-widening action — the labels, which L18 says to
  declare where introduced anyway. The load before the knot, fed
  `body`'s `{}` rather than a case, is the `×→+` word `load f` (crud,
  order-form), typed at exactly its outcomes. `confirmed @l caption f
  content` (MDC2/MDC3) is a `+→×` leaf like a status: opened by case
  `l`, which it tells as its title verbatim, its content shown the
  payload, the confirm folding `f` of it — cashbox's two dialogs are one
  line each, weather's about-dialog folds `identity`; `simpleDialog`, the
  same word at `{ | i } → { | o }`, is deleted (a dialog is opened by an
  event, so a `×→×` form would open on every broadcast), and
  `PUI.Web.witnessed` takes the fold. `PUI` re-exports no adopter but
  `toCase` (the one shape change a business case names: an editor's
  release leaving as an event, movie-browser) and `settled`; `atField`/
  `atCase`/`toField` stay on the floor for the vocabularies, where a
  face is adopted at its label. check-determined's message and the docs
  follow; demo/workflow.html re-captured.
- **The shared merge sides are rows, not classes.** `SharedRecordInputs`
  (the three-way row equality with its `fedFirst`/`fedSecond` witnesses,
  since 2026-10-11; an `InclusiveRows` bundle before L18) is deleted:
  `recordToRecord` and `recordToVariant` are typed at one input row `i`,
  as `variantToVariant` and `variantToRecord` are at one output row `o`
  since 2026-10-07, and the three instances (`PUI`, `Cont`, `(->)`) feed
  their operands directly. `SharedVariantOutputs`, a one-instance alias
  of `InclusiveRows`, is deleted with it; `recordToVariant` names
  `InclusiveRows`. The floor keeps the owned bundles and the blocks they
  are built from; no demo changes, since the equality was already in
  force. The four merges name their rows by side (`i`/`o`, numbered per
  operand, `RowList`s `i1l`/`o1l`), the one exception to the
  kind-named convention.
- **Open on the shared side, closed on the owned side, in all four
  shapes.** The `×→+` output is one row every operand is typed at, like
  the other three shared sides; `InclusiveRows` is deleted (its last
  user) and `recordToVariant` carries no constraint — the merge forwards
  each emission as it is, the `rmap expand` it did per operand gone.
  Every emitter is open on its output (`Cons l a b v`, as an editor is
  `Cons l a b r`): `button` and its emphasis siblings, `fab`,
  `iconButton`, `menuItem`, the `listOf` family in all six vocabularies,
  `clicked`/`clickedRow`/`replayed`/`onClickedXY`/`eventLeaf`, `ticks`;
  `toCase @l` is open again (closed 2026-10-14). The block's row reaches
  an emitter from the folds that own it, so a hole determines as before
  (`check-determined` 106/106, signatures verbatim; holey sweep 107/107).
  What cannot sit in the block's row by a label is a `+→+` stage of a
  **declared** row standing beside emitters — order-form's three
  actions, ticker's — and these are `expanded` (`Data.Profunctor.Row`,
  re-exported by `PUI`: `rmap expand`, lawful by `Union t b o`,
  `… # action @( … ) submitOrder # atCase @"Submit order" # expanded`).
  The price is `joined @l`, label-indexed again (label-free since
  2026-10-14): with every emitter open, the source's row is the block's,
  and nothing but the label says which case is the source's, so the
  label is the source's own restated (`listOf @"Person picked" … #
  joined @"Person picked"`; a two-outcome action is joined twice), 54
  lines in 17 demos. Measured first the other way: `action` typed with
  `Union t b o` broke every undeclared action under a hole (21 holey
  twins), and an output-driven label-free `joined` cannot tell its
  cases from its siblings' — the two findings that fix the design
  (branch experiment/top-down-event-row, the same day). The `×→+` law
  probes are typed at one shared row; the silent unit is polymorphic in
  its row.

## 2026-10-14

- **Demo sweep after the day's cleanup.** Every demo read against
  writing.md; the residue fell into a few patterns, each fixed wherever it
  occurred. Ten view-model "copy functions" returned one stored field
  verbatim (`titleLine { title } = title`, `courseName = _.name`), the
  accessor the view-model side of the field-labels rule — each is now
  `textAt @"field"` at the leaf (cells' row label, movie-browser's title,
  password-generator's password, photo-gallery's album, potluck's guest,
  shopping-cart's product, restaurant-menu's course and dish lines), and a
  stored array is `# actedAt @"name" @"dishes"`. Three fixed catalogues
  were functions ignoring their row (`productCatalogue _ = …`); they are
  values under `(const …)` in the projection slot, as calculator's keypad.
  Two derived classifiers in meeting-booker only renamed a stored
  variant's cases and are `# inCaseAt @"chosen" @"Room"`. Todos carry an
  `id` so the list is keyed by it, not by position. Reorder's Rotate is a
  fold (`fold @"Rotate" rotateSetlist`), Shuffle the one action.
  Ticket-dispenser's `issue` updates its row instead of rebuilding it;
  calculator's views call `isOperatorKey`/`isFunctionKey` instead of
  testing membership themselves. Layout: an ocular over a block is
  applied with `$` everywhere (`chipSet $`, `menu "Presets" $`,
  `list $`), the plain-HTML floor writes `( p $ label $ Semigroupoid.do …
  ) # settled …`, decorators ride their element (`listItem >>> clWhen
  isFavorite …`, Bootstrap's `listGroupItem >>> cl … >>> clWhen …`),
  payment's action and status own the button's case as one bracketed
  pair, ticker's and potluck's long rows break per the rule, and every
  view-model signature over 100 columns is broken in the layout of
  scripts/signature-layout.mjs (68 of them, circle-drawer's six exports
  the longest). writing.md loses the sentence that still said a status
  could stand in a fold block releasing nothing.
- **A status tells its case: the copy function is gone from every
  `+→×` status.** `snackbar`/`toast`/`messageBar`/`output` take their
  fold and nothing else — `snackbar @"Flight booked" kept`, `toast
  @"Review submitted" identity`, `snackbar @"Order placed" (const
  usualOrder)` — and show their case label verbatim, as a button draws
  its label (L3: a label is the copy it draws). Eryk's reasoning: a
  status is ephemeral, so elaborate copy in it only raises cognitive
  load. The census of the twenty demo copy functions agreed: four
  ignored their payload, seven named an entity already on screen, three
  dispatched on a sub-variant (a disguised sibling status), four flashed
  data that existed nowhere else (an order number, a report reference,
  an attempt count, a server reason — data the user cannot keep from a
  five-second toast, so a model field shown by a display, or nothing),
  and three recapped the whole form in a sentence. The consequences:
  an action's outcome row names its outcomes as the words the user
  reads, a reason being a sibling case (order-form's `"Order rejected:
  no total"`); a dead payload is `{}` (flight-booker's itinerary,
  order-form's short id, espresso-bar's order number, incident-report's
  reference); and a status on a button's own case — the four
  non-Material showcases and signup-form told the *command* — is
  replaced by a stand-in action returning the model under an outcome
  case (`toast @"Review submitted" identity` after `action
  submitReview`, loan-calculator's `applyForLoan`, signup-form's
  `signUp` behind `# provided @"ready" validation`). `PUI.Web.textOf`
  is `statusText :: String -> PUI Web a { | r }`, the face rendering
  its caption per occurrence; the `…Face` words take a `String`. The
  leaf-law bench checks that a status shows its case label. The
  previous day's entry below records the form this replaces.
- **`banner` is an ocular; `output` stays the plain floor's status.** The
  two non-ephemeral statuses were asked which shape they are. Material
  defines a banner as a message that stays while a state holds, which
  is a pane over model state, so MDC2's `banner` is a surface over a
  display, `( banner $ staticText "Inbox zero!" ) # inCase @"empty"
  inboxState` (inbox; its MDC3 twin shows the line as plain typography,
  MD3 having no banner), with no dismiss of its own — the MDCBanner
  foundation and its Dismiss button went with the status. HTML's
  `<output>` is specified as the result of a user action, and the plain
  floor needs one status, so `output @l f` stays a status, in place
  because nothing in plain HTML dismisses itself. doc/research-copy-is-a-
  function.md keeps its display half and records the status half as
  withdrawn.
- **Every `+→×` leaf folds: statuses take their fold, the action slot is
  lifecycle-only, `joined` joins every case.** `fold @l f`, `reset @l x`
  and `capture @l` are the faceless folds, label-indexed leaves
  (`fold @"Count" increment`, `reset @"New game" openingPosition`,
  `capture @"People loaded"`); the tick source is `ticks @"Clock ticked"
  tickPeriod`, taking no status (a tick that needs telling is `ticks @l p
  >>> (status # observed)`). A status is the same fold with a face: every
  vocabulary status takes its fold after its copy function —
  `snackbar @"Person created" personCreatedLine identity`, `snackbar
  @"Order placed" orderPlacedLine (const usualOrder)`, `snackbar @"Quote
  fetched" quoteFetchedLine recordQuote` — so nothing stands in a fold
  block releasing nothing. The three places a status stood alone each
  went: an action's slot is typed at `[ started, ended ]` (`Contractable
  ( started :: {}, ended :: {} ) v1`), so a snackbar is never the slot
  and the outcome leaves to be folded after the action (flight-booker,
  incident-report); an outcome that is told and changes nothing is
  `joined` with its button in the event ensemble and its status folds
  `kept` (`kept = _.model`, order-form's submit and receipt,
  flight-booker); a knotless flow's closing status folds `identity`
  (signup-form, product-review, loan-calculator, meeting-booker). A
  narrating status under `observed` folds `identity` too, and `observed`
  drops it. `joined` is **label-free and joins every case** of its
  source (class `JoinedCases`, the row mapped at the variant's
  representation), so a two-outcome action needs no second word; the
  `@l` on every `# joined` is gone. `folding @l f` is plumbing, the one
  fold behind every status (`snackbar @l copy f = snackbarFace copy #
  atCase @l # folding @l f`) and behind `fold @l f` (`unshown @l #
  folding @l f`); `resetting` and `capturing` lasted the morning.
  The day's first cut kept the status-opened optics (`# folding`,
  `# capturing`, `# resetting`) beside the leaves; Eryk's call was that a
  status should never be an action slot and that a no-change fold is
  acceptable where necessary, which left no standalone status to keep
  an optic for.
- **`observed` DELETED.** The pass-through that let a status narrate an
  event on its way to an action (payment's "charging…" snackbar) was its
  only use, and a run in progress is the action indicator's to show; a
  status that tells something folds it in the fold block (inbox's banner
  now folds the deletion it announces, `banner @"Delete" inboxZeroLine
  deleteOpened`). With it goes the last status that stood without a fold.
- **An element over one leaf applies it with `$`.** `headline4 $ text
  countLine`, `card $ body1 $ text summaryLine`; a chain on the wrapped
  element opens the paren before the wrapper (`( headlineSmall $ text
  wonLine ) # inCase @"won" gameOutcome`), a decorated list item composes
  its decorator onto the element (`span >>> clWhen isCompleted
  "todo-done" $ textAt @"title"`, todo-list), and `static`, whose
  argument is a polymorphic ocular `$` cannot pass, keeps its parentheses.
  writing.md *Layout* states it; every demo view follows it.
- **`witnessed`, and the vocabularies import no ecosystem algebra.**
  MDC2's and MDC3's `confirmed` unwrapped the confirm button's case with
  the ecosystem's `rmap` — the one L16 violation in the vocabularies.
  `PUI.Web.witnessed` is that button as a carrier word: `{}`-chrome whose
  click releases the row the stage was fed, `eventLeaf` with its case
  unwrapped, the one lawful record-output emitter (a dialog is a `×→×`
  stage, Answer refined in time). L16 now places `PUI.Web`'s internals
  on the carrier side and states the floor's check as
  `grep "import Data.Profunctor (" src/PUI/Web/ demo/`, empty.
- **Documentation sweep.** CLAUDE.md, guardrails, the skill, the design
  docs and the extras READMEs state the current library; dated history
  is this file's. The research notes keep their bodies and carry a
  Status paragraph saying what of them still holds.
- **`PUI`'s export list says what is vocabulary.** `announce`, `seeded`
  and `resolveFor` are grouped apart as carrier plumbing — exported for
  the vocabularies and the law tests, written by no demo (`with` closes
  over the point, `debounced` over the other two) — above the words a
  view writes. CLAUDE.md's vocabulary table folds the point's laws into
  `with`'s row. (`focusCase` was never re-exported, so the focus pair was
  already plumbing on both rows.)
- **`Seeding` and `Looping` deleted; `optioned` with them.** The two
  classes each stated one thing a timeless carrier lacks — a beginning
  (`announce`) and feedback (`looped`) — and each had one real instance,
  `PUI`; `Seeding`'s `(->)` instance was `const` and its three pure
  assertions tested that, `Looping` had none by its own theorem, and
  every law with content was temporal and ran on `PUI` already. They
  were carrier facts dressed as algebra, so the four words
  `announce`/`seeded`/`with`/`looped` are now `PUI` functions (`with`
  leaving the floor, the floor staying `Profunctor`-only), their laws
  and the trace-asymmetry account moved into `PUI`'s headers, `Cont`
  losing its `Seeding` instance. Demos import both words from `PUI` as
  before. `Acting` stays a class: its `(->)` instance is `map`, its
  `Cont` instance `traverse`, and `acted`'s key re-set is tested as a
  pure equality, so it is the merge classes' peer. Its `optioned` (the
  `Maybe` action) goes on `asField`'s grounds — unreachable by design,
  the view-model rule admitting no `Maybe` field. extras/row-profunctor
  now holds exactly the floor, the four shape modules and `Acting`,
  every class with a `(->)` instance.
- **`isHole` is `PUI`'s.** It read a field the carrier's JavaScript
  invents (`__bambikHole`) from inside `Data.Profunctor.Seeding`, the
  carrier-agnostic algebra. It now sits beside `tr` and `gateGuard` as
  the third development diagnostic, exported from `PUI` for the
  vocabularies; `Seeding` holds the class, its `(->)` instance and
  `seeded`, with no `unsafeCoerce` in it.
- **`Silencing` deleted; the algebra names no unit but `identity`.** The
  one-method class coined earlier today for the `×→+` unit is gone with
  its two instances: no shape needs a unit word of its own, the three
  wires are `Category`'s `identity` placed, and the fourth, which no wire
  reaches, is the carrier's own silent component — `PUI`'s private
  `silent` (the one thing `observed` needed `silence` for) and `Cont`'s
  `mempty` — stated and law-tested per carrier (test/Main.purs and
  test/Exhaustive.purs write it as a probe), named by no class. `silence`
  leaves `PUI`'s re-exports: no vocabulary or demo ever wrote it, every
  situation that observationally is silence (a detached pane, a status,
  a hole) having its own word.
- **`toCase` closes its row.** `Cons l a () v` like `toField`'s `Cons l a
  () r`, where its rest was open (`Cons l a b v`) since it arrived. Every
  call site sits in a `RecordToVariant.do` block, whose merge widens each
  operand into the union with `rmap expand`, and `joined` already closed
  it, so a stage introducing a case is now typed at exactly that case,
  its hole hint one case rather than an open row. The single-label grid
  is uniform in both halves.
- **`atAnyRow` and `widenVariantOutput` dropped, inlined.** The floor's
  two named reshapings were one-liners (`lcmap (const {})`, `rmap
  expand`) with one caller each outside a test: the vocabulary's
  `eventLeaf` placing `{}`-input chrome at the emitter's row, and the two
  `×→+` merge instances (`PUI`, `Cont`) injecting an operand's cases into
  the union. Both now write the ecosystem function, so no export of the
  library names a widening or a placement: the floor holds the row
  evidence, the two owned-side row functions and the one-sided
  vocabulary (`atField`/`toField`, `atCase`/`toCase`, `with`, `settled`).
- **`replaying` deleted.** Since yesterday its body was `joined` at the
  empty payload with the event dropped and the case renamed — a
  derivation, with `PUI.Web.clicked`/`clickedRow` its only callers. They
  now write it themselves (the private `replayed`), the occurrence-source
  pin (`RowToList v1 (Cons c {} Nil)`) going with the word since the
  private click source is `[ occurred :: {} ]` by construction, and its
  law test folds into `joined`'s primed `Strong` law. The `×→+` module
  exports `joined` alone beside its merge.
- **The six per-shape row forms deleted: `feedback`, `cycled`,
  `folding`, `unfolding`, `subResolving`, `subRetaining`.** All six were
  restored on 2026-10-13 for the completeness of the shape table's
  strength-optic and co-strength-optic columns; they go on two grounds,
  neither of them reach. The four co-strength forms are **subsumed**: each
  kept one state the rows did not show (a seeded field, a re-entering
  case, a fold's accumulator, an unfold's resume case), and a model field
  under `looped` does the same job with the state declared — the
  replacement every demo had already made on 2026-10-05/06 (auction's
  `top`, checkout's `step`, ticket-dispenser's `next`, payment's retry
  inside its `Aff`). The two strength forms are a **narrowing and
  widening**: a stage's input shrunk to a sub-row and its output grown
  by the leftover, which the one-row-per-stage rule (L18) excludes —
  coercions of `atAnyRow`'s kind, plumbing and never vocabulary; the
  model-side idioms are `group @l` for a nested sub-record and a pane
  for a case. With them go the floor's `splitRow` (only `subResolving`
  called it) and `LoopCases`/`LoopCasesK` (only `cycled` constrained on
  them). Nothing else moves: the classes `PointedCostrong`, `Cochoice`,
  `Coresolving`, `Coretaining` and the lens modules in extras stay, their
  `PUI` instances law-tested in test/Main.purs, and
  test/BusinessOptics.purs writes its two sub-row examples out as
  `shutterE`/`reelE` existentials. The shape modules now export their
  merge, its sugar and, per shape, `focusField` / `focusCase` /
  `joined`+`replaying` / `unshown`. No demo or vocabulary module named
  any of the six.
- **`asField`/`asCase` deleted.** The rename column of the single-label
  grid, back since 2026-10-13 for the grid's symmetry: a closed singleton
  relabelled on both sides, `dimap` alone. Its one use is re-labelling a
  component built at a fixed internal label — exactly what guardrails L3
  rules out, every leaf being label-indexed with no canonical label — so
  it was unreachable by design, not merely unreached (L14's clause for
  catalogue words a demo may yet use does not apply). The grid is three
  words per shape: the focus and the two halves of the closed-singleton
  iso, `focus = to <<< at`, the halves its generators; nothing reached the
  rename but its law test, now gone with it.
- **`replaying` is `joined` at the empty payload.** Its source is pinned
  to an **occurrence source** — one case carrying `{}`, the row read by
  `RowToList v1 (Cons c {} Nil)` so the label `c` is determined at the
  call site — where before any `×→+` component was accepted and its
  output dropped unread (`\(Tuple _ r) -> f r`), a silent discard the
  type did not admit to. The body is now `joined @c src` with the
  `{}` event dropped and the case renamed to `l`, so there is one
  `first` behind both words and replay is `Strong`'s retention and
  nothing else; `clicked`/`clickedRow` and the law test are unchanged.

## 2026-10-13

- **The single-label grid.** The two pure shapes carry the same four
  words: `focusField`/`focusCase` (one label, the rest threaded),
  `asField`/`asCase` (a closed singleton renamed), `atField`/`atCase`
  (the input half of the iso between a closed singleton and its
  payload) and `toField`/`toCase` (the output half); at the closed
  singleton `focus = to <<< at`, the halves and the rename being
  generators. New: `asCase`
  (back, as `asField`'s mirror), `atField` (back at the closed
  singleton), `toField`; all re-exported by `PUI`. None takes a
  function: `toCase` loses its projection (`toCase @l @a`; cashbox's
  `identity` dropped, movie-browser's `favoriteMark` deleted, its fold
  taking the whole element row), and `forCase` is deleted — a status
  face takes its copy function (`textOf copy`, now at a bare input)
  and is adopted `# atCase @l`; `eventText` goes with it. `replaying`
  and `joined` are written through `toCase @l` over `first`, the case
  introduced rather than injected by hand.
- **`silence` is `Data.Profunctor.Silencing`'s.** The `×→+` unit leaves
  the `RecordToVariant` class for a one-method class beside `Seeding`
  and `Looping` — *nothing* as carrier structure, as they are a
  beginning and feedback — so that no merge class carries a unit: every
  shape borrows its own from the class that can supply it, three from
  `Category`, this one from `Silencing`. Instances `PUI m` and
  `Cont r` (under `Monoid r`) move with it; `PUI` re-exports it as
  before. The shape table in `Data.Profunctor.Row` lists only shape
  modules' functions, so `silence` leaves it beside `looped`.
- **One-sided words live on the floor.** `atField`/`toField`, `atCase`/
  `toCase`, `with` and `settled` moved from their diagonal modules to
  `Data.Profunctor.Row`, beside `atAnyRow`/`widenVariantOutput`: a word
  constraining one side is a side's, not a shape's, and filing it in a
  diagonal module made `RecordToVariant` import `VariantToVariant` for
  `toCase`. The rule is now: both sides constrained → the shape's
  module; one side → the floor; no shape module imports another. `PUI`
  re-exports are unchanged; vocabularies import `atCase` from the floor.

- **Every shape has its strength's and its co-strength's row form
  again.** Restored for completeness and symmetry of the placement table
  in `Data.Profunctor.Row`: `subResolving @w` (`Shutter`) and
  `subRetaining @w` (`Reel`), with `splitRow`; `feedback @l`
  (`Colens`, over `PointedCostrong`), `cycled` (`Coprism`, with
  `LoopCases`), `folding @w @l` (`Coshutter`) and `unfolding @w @l`
  (`Coreel`) — as they stood before their deletions (2026-10-13,
  2026-10-06, 2026-10-05). They are law-tested (test/Main.purs probes,
  test/BusinessOptics.purs) and reached by no demo: an app's knot stays
  `looped` and a looped state a model field. The table now marks each
  empty cell as placement (`·`, the words filed elsewhere) or a genuine
  limit (`—`, a word that cannot exist).

## 2026-10-13, later

- **A demo app is its module; its views are named by their design
  system.** `demo/7guis/counter/` holds `Counter.purs`
  (module `Counter`, the view model — the app itself) and `MDC2.purs`,
  `MDC3.purs`, `Shoelace.purs`, `Fluent.purs`, `Bootstrap.purs`,
  `HTML.purs` (modules `Counter.MDC2`, …, `Counter.HTML`, every one
  entered at `counter`); crud's stand-in server is `Crud.PeopleServer`.
  Before: `CounterMDC2.purs` (module `CounterMDC2`, entry
  `counterMDC2`) and `CounterViewModel.purs`. Any further module is
  named the same way, its file the module minus the app prefix
  (order-dashboard's `ControlsMDC3.purs` is `OrderDashboard.ControlsMDC3`).
  The two plain-HTML apps are one `html` variant each, their pages
  carrying one template like every other (helloworld's directory is
  `hello-world`, so its entry derives as `helloWorld`). The registry
  (scripts/demos.mjs) lists an app as its design systems in page order
  and derives module, file, entry and view model from the app's name;
  the holey sweep, `check-view-model` and `check-determined` ask the
  registry which module is the view model instead of matching a
  `ViewModel` suffix. demo/workflow.html's session was replayed under the
  new names with the pinned compiler (view `src/MDC3.purs`, module
  `Order.MDC3`; view model `src/Order.purs`). The law benches keep their
  `LawBenchMDC2` names: not apps.
- **Displays release the row they show.** A display — `text`/`textAt`,
  the statics, `each`/`dynamic`, every progress bar, `imagePane`,
  `columnHeader` — is `{ | r } → { | r }`: `displayWiring` renders the
  value and echoes it, so a display is a stage like an editor and a view
  line sits where it sits on the screen. Before, a display output `{}`,
  which made a display line a decorator of its neighbour: `prefixed d $
  stage` put the stage at the end of the display's last line and
  `# suffixed d` put the display inside the stage's statement, and
  neither let a line begin with the element it draws. With the `{}`
  output go the words that existed for it:
  - `prefixed` and `suffixed` DELETED (two days old); `confirmed`'s
    content, the drawer's content and `group`'s heading are plain stages
    inside the library.
  - `pane`/`paneAt` DELETED: `inCase`/`inCaseAt` is the pane for displays
    and editors alike. **Panes no longer narrow**: `inCase` and
    `provided` feed their content the whole model, never a case
    payload, so a classifier's cases carry a payload only where the
    business needs one and every read function inside a pane is typed at
    the model (inbox's `subjectLine` finds the open message itself;
    checkout's `checkoutStep` dissolved into the stored `step` field).
    A button inside a `provided` pane replays the model, so it needs no
    `# joined` (stopwatch's four, inbox's, checkout's two); `joined` is
    for list elements and canvases.
  - The list splits by shape: `foreach @l f` is the list of **displays**
    (the fed row passes on, so the list is a stage line), `anyOf @l f`
    the list whose elements **emit** (the old `foreach`'s forwarding),
    with `anyOfAt @l @t` over a stored array (cells). `foreachAt`
    DELETED: a display list over a stored array is `actedAt`, the
    container action, which writes the elements' echoes back. `muted`
    is DELETED with the last slot that demanded `{}`: departures and
    scoreboard, whose board's output is the stream and not the model,
    are fed by the `clock` source instead of a `beat` loop — no loop,
    fold, seed or merge, the board after the clock, the `dispatched`/
    `accumulated` chain ending the block.
  - Every `RecordToRecord.do` in the demos became `Semigroupoid.do`: a
    display owns the row it releases, so two cannot merge, and composing
    them is the same screen; the vocabularies' groups of statics the
    same, since a static is the wire at its row.
  - Guardrails L13's no-silent-loss rule is restated: the type no longer
    tells a display from an editor, so `npm run check-view-model` rejects
    an editor word inside a `foreach` element or a `clicked` face, the
    two slots whose content's release has nowhere to go.
- **`blank` DELETED**: with displays releasing their row the faceless
  leaf became `identity` under a name, and an element whose whole face is
  its decorators is written as the wire, decorated (`SVG.circle >>> attrWith
  "fill" circleFill $ identity`, circle-drawer; MDC2/MDC3's `imagePane`
  and absent icon slots the same).
- **Sources.** `source @l now watch` (`PUI.Web`) and the facts over it —
  `geolocated`, `online`, `clock`, `colorScheme` — plus HTML's capture
  `filePicked`: a fact the platform knows and the app cannot seed, owning
  one field, pointed (its first value announced at registration), every
  feed answered with the last value known, each platform change an
  emission outside any feed; a fact the platform may withhold is a
  variant with named cases. Merged `RecordToRecord.do`, the `×→×`
  merge's one application use now that every editor owns the whole row.
  Two demos: **here-and-now** (MDC3 + HTML, a knotless flow of four
  facts) and **incident-report** (MDC3, facts and a photo filed by an
  action). `text` renders nothing of a hole read, and `dispatched`/
  `accumulated` treat a hole projection as absent, so a row that reaches
  them before the logic is written (as from a source) reaches no hole.
  The law-bench pages and `scripts/smoke/tests/leaf-laws.mjs` follow;
  test/Main.purs gains the display-list law (the fed row passes once per
  feed, the elements' releases stay inside) and loses the
  `prefixed`/`suffixed` test.

## 2026-10-13

- **`captured`**, beside `reset`: one case folded by taking its payload
  as the model whole, `captured = fold identity`, as `reset x = fold
  (const x)`. It names the commonest fold, an effect's outcome carrying
  the model (crud's six outcomes and its load, order-form's load,
  payment, weather, password-generator, reorder); every demo's `# fold
  identity` now reads `# captured`.
- **`checkbox @l @c @n` captions itself from its label**, as every other
  editor does: the trailing content argument is gone (MDC2, MDC3, the only
  vocabularies with a checkbox). The demos' fields take the copy they
  drew as their label: espresso-bar's `"Loyalty member"`, signup-form's
  `"I accept the terms of service"`. A sweep of every vocabulary found no
  other label-indexed leaf taking its caption as an argument.
- **A design system's `…With` sibling is primed**: `buttonWith`,
  `listOfWith` and `listOfAtWith` (MDC2, MDC3) are now `button'`,
  `listOf'` and `listOfAt'`. `attrWith` in `PUI.Web` keeps its name: it
  names no design system's leaf.
- **A tooltip is static.** `tooltipWith` (briefly `tooltip'`), whose tip
  was a copy function re-read on every feed, is DELETED from MDC2 and
  MDC3: `tooltip "…"` is the one form. espresso-bar's loyalty tip was its
  one use; it reads `# tooltip "Members get 10% off"` and
  `loyaltyNote` left the view model.
- **A static inside a pane is a static.** Seven copy functions that
  ignored their row — calculator's `faultLine`, tic-tac-toe's
  `drawnLine`, signup-form's `unnamedLine`, ticket-dispenser's
  `noTicketLine` and `firstTicketHint`, meeting-booker's
  `roomRatingCaption` and `seatsTakenCaption` — are DELETED from their
  view models, their lines now `staticText "…"` inside the same pane
  (`staticText "Error" # inCaseAt @"faulty" @"condition"`). They existed
  because `staticText` was pinned at `{} → {}` and could not sit in a
  row-typed block; with every static the wire at any row (above), the
  pane decides when the text shows and the text reads nothing, so a
  function with a wildcard argument is a static in the wrong module.
  writing.md's *Fixed copy is static or constant* says so; a constant
  is now fixed copy that reads data or carries a business value
  (cashbox's `refundLine`, which interpolates `standardRefund`).
- **A demo app is one directory and one page.** `demo/<suite>/<app>/`
  holds every design system's view (`InboxMDC2.purs`, `InboxMDC3.purs`),
  the shared view model and one page whose query picks the design system
  (`inbox/?mdc3`), demo/page.js installing its stylesheets from a
  `<template data-variant>` and loading `bundle-<variant>.js`; the
  `<app>-<ds>/` sibling directories are gone. scripts/demos.mjs groups an
  app's design systems under one entry and derives each one's bundle and
  page, which the bundler, dev server, holey twins, check-determined and
  smoke tests now read instead of building paths themselves. Both
  suites, each 7GUIs task with its six design systems included.
- **A fold's status shows the event, never the fold's result.** `fold f
  status` feeds the status the occurrence as it arrives, so its copy
  function is typed at the payload; since 2026-10-07 it had been fed
  `f` of the payload under the same case, which put a second row
  variable and a second `Cons` in the signature. Whatever a status says
  must therefore be in the event: a status with copy stands after an
  action that returned what it tells (crud's actions, payment,
  ticker reading the fetched price), and a command straight from an
  emitter folds under `blankStatus`, since its result is already on
  screen. Counter, ticket-dispenser, cashbox, checkout, inbox and
  shopping-cart drop their snackbars (and their copy functions);
  writing.md's *A status only where a real app would tell* (backend
  outcome, money moved, destructive action) becomes *A status only after
  an action*.
- `blankStatus @l` is renamed `unshown @l`: it owns a case and shows
  nothing, whether the case comes from an emitter or an action, so
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
- **No call site writes `{}`.** A leaf takes no options record unless one
  is genuinely needed: the text fields, sliders, switches, chips,
  selectors, `fab`, `iconButton`, `menuItem`, the button emphasis
  siblings and `rating` lose theirs (and with them the unused caption
  overrides `floatingLabel:`/`label:`); `checkbox` loses its ticked
  payload, now always `{}`. The words with optional presentation
  reached by a demo get a `…With` sibling: MDC2/MDC3 `buttonWith`
  (`icon`), `listOfWith` and `listOfAtWith` (`selected`). The textareas,
  `iconToggle` and `drawer`, whose every call needs its record, keep it.
  `OptLabel` (the fab's optional label) is deleted.
- **`subChoice` is deleted.** An action is folded by its own outcome
  statuses, and that pair owns the button's case in the fold block
  (`( progress # action createPerson >>> VariantToRecord.do { … } ) #
  atCase @"Create"`), so no event needs passing around a `+→+` stage and
  every action is typed at exactly its own outcomes (crud's at two cases
  each, where the shared `+→+` row typed each at all six; order-form's
  `printReceipt` names its one). Its shape — a `+→+` block between the
  emitters and the folds with the untouched cases passed through — is
  gone from the demos (crud, order-form, espresso-bar, cashbox, whose
  dialogs now own their cases in the fold block).
- **`subResolving` and `subRetaining` are deleted**, with `splitRow` and
  their two `BusinessOptics` examples: the last of the sub-row family
  (one `dimap` around each shape's strength, the background threaded
  past a stage as a case or a field), reached by no demo — an app keeps
  its background as model fields. `Shutter`/`Reel` stay as the
  value-level optics; `debounced` still stands on `Resolving`.

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

## 2026-08-11

- **`Cont` builds again.** The CPS carrier had been parked out of the
  build, which let its instance inventory drift; back in the glob, so a
  layout move that invalidates it is a compile error, not silent rot.

## 2026-07-29

- Pointedness merged: `Seeding`, `body`, `with`.
