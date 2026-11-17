# Changelog

What changed in the library and when, newest first. The module headers
and doc/guardrails.md state the rules as they stand; this file holds the
history they used to carry inline — what was added, what was tried and
withdrawn, and the date — so a rule can be read without its obituary.
Before 2026-07 the history is the git log.

## 2026-11-17
- **MDC3 layout refined.** `layoutGrid` is a stylesheet class, spans its
  container and adds no padding of its own (the surface or page owns the
  margins), its cells hold a card edge to edge, and in a grid narrower
  than 600 px every cell takes the whole row. `tabBar` spans its
  container. A card's headings carry no margin (the card's gap is its rhythm) and a list in a
  card spans it. A data table scrolls sideways inside its border rather
  than overflowing the page. Diner's tables are all full width, its
  kitchen tickets sit on the layout grid, the standalone page stacks the
  app like the demo surface (a flex column, 16 px apart), and each
  Magazyn row gains **Wyzeruj**, zeroing that dish's stock after a
  confirmation.
- **`dataTable'`**, MDC2 and MDC3: `{ fullWidth: true }` stretches a
  table across its container instead of hugging its content; `dataTable`
  is `dataTable' { fullWidth: false }`. Diner's Statystyki uses it.
- **Diner review applied.** The menu cards sit on the vocabulary's
  `layoutGrid`/`layoutCell` instead of an inline grid, and the sold-out
  fade is gone (the disabled button says it). Unreadable or negative stock
  is left as it is by every return. − is disabled at one portion, so only
  the bin, which asks first, removes a line. `markReady` and `settle`
  advance the stage the order has in the model, and the send to the
  kitchen carries the order's number. One copy function each for the
  number, the table and the quantity, over every row that has the field
  (check-determined pools a name's hints across a view's parts, so a
  `forall` over two parts is the two-row case it already allowed). Stage
  predicates replace the echoing `phaseOf`, `plateLine` is `plateDish`,
  the icon columns have headers (Mniej, Więcej, Usuń, Płatność), and the
  view model's exports are sorted and grouped by station.
- **Part tests (`npm run part`)**: one part of a demo's view run in
  isolation from prepared initial data — tests beside each part
  (`MDC3/Hall.test.mjs`, a list of `{ name, seed, run }`), the seed a value
  of `<App>.Fixtures` or the view model, the part mounted on its own and
  driven with real clicks, typing and picks in headless Chrome
  (scripts/part-test.mjs). Diner gets `Diner.Fixtures` (`rushHour`,
  `soldOut`, `emptyDiner`) and a test file per station, 16 tests. Chrome
  discovery and launch moved to scripts/smoke/chrome.mjs, shared with the
  smoke harness.
- **Diner is MDC3-only and split into parts.** Its MDC2 twin is gone
  (MDC2's `select'`, `button'` `disabled`, `iconButton'` and integer fields
  keep their law-bench rows). The view is now an entry with the tab bar and
  one line per station, each station a submodule of the view
  (`Diner.MDC3.Stock`, `Hall`, `Kitchen`, `Pass`, `Report`, `Statistics`,
  plus the model-free `Layout`); check-determined learned to determine view
  submodules and to keep the view model's types a view imports. Review
  fixes: a table with an order in the kitchen or at the pass can no longer
  take a second order (its menu hidden, a note instead, and the fold
  refuses), the kitchen queue follows the order orders were sent in rather
  than the time of day, the report sorts by number, Statystyki's total sums
  its rows, the menu names the stored dishes (`gatheredAt`), and writing.md
  cites diner's current labels.
- **`confirmed'`**, every vocabulary with `confirmed`: `{ cancel }`
  captions the Cancel with the screen's own copy; `confirmed` keeps the
  chrome "Cancel". Diner's trash icon asks before removing an order line,
  its Cancel "Nie, nie usuwaj dania".
- **`button'`'s `disabled` option and `iconButton'`**, MDC2 and MDC3: a
  predicate on the row; while it holds for the row last fed the button is
  drawn disabled and the platform reports no click. A decoration of the
  button, so `disabled: const false` is the button itself; walked with
  real clicks in scripts/smoke/tests/disabled-buttons.mjs. Diner's dish
  cards stay drawn when a dish sells out, their add button disabled, as
  is the + of the dish's order row.
- **`select'`: options the model rules out.** MDC2 and MDC3 gain
  `select' @l { unavailable }`, a `select` whose cases
  `unavailable :: { | r } -> Array [ | w ]` returns for the fed row are
  drawn disabled and cannot be picked, re-read on every feed. A
  decoration of `select`: its editor laws are `select`'s and
  `select' { unavailable: const [] } ≈ select` (bench row on both law
  pages; the disabling walked in scripts/smoke/tests/unavailable-options.mjs).
  Diner's table select reaches it, disabling tables whose order is in the
  kitchen or at the pass. The other vocabularies have no `select'` yet.
- **Integer fields.** MDC2 and MDC3 gain `filledIntegerField` and
  `outlinedIntegerField @l @n @i`, the number fields' whole-number
  siblings over `[ n :: Int, i :: String ]` (`PUI.integral`, the strict
  parse of digits and a leading minus; a fraction stays invalid text).
  Bench rows on both law pages; diner's stock portions reach them.
- **`icon`**, MDC2 and MDC3: a Material icon as chrome, drawn inline
  before its copy and hidden from assistive technology; diner's kitchen
  marks each line plated or still cooking with one, by case
  (`inCaseAt @"plated" @"state"`). A static, so it carries no laws of its
  own and no bench row.
- **`now` joins the dependency list**, for diner's order timestamp: the
  stamp is read inside the action that sends an order to the kitchen
  (`Effect.Now`), the time carried by the order's stage from then on.
- **MDC3's selected list rows are visible again.** `listOf'`/`listOfAt'`
  styled a selected row through `--md-list-item-container-color`, a token
  `@material/web` 2.5's list item does not read, so `selected` rows looked
  like every other row. The selected row now takes the secondary-container
  colour as its background.

## 2026-11-16
- **Guardrail L19: what is drawn leads; what is wired trails.** The
  line form writing.md asks of applications, `uiElement # plumbing`, is
  now a library obligation. A published word that draws nothing takes
  the stage it acts on last and is written trailing; only a word that
  draws leads. The rule had been applied twice already without being
  written down, when `prefixed`/`suffixed` went (2026-10-13) and when
  `acknowledged` and `introduced` became trailing (this date).
- **Guardrail L20: design systems are interchangeable**, and L3 gains
  **conditions are cases**. Both had been stated only as application
  rules or in CLAUDE.md, and both are now library obligations: the same
  concept gets the same name and signature in every vocabulary, a
  catalogue's gaps stay gaps, and no word may need a `Maybe` or an
  unedited `Boolean` in the model.
- **The skill's vocabulary.md is deleted.** It was a third statement of
  what writing.md's *Stages* table and the module headers already say,
  from need to word to demo, and the statement that drifted: its
  composition rows had grown into copies of *Stages*, and it listed
  words no demo uses. The lookup is now *Stages* plus the module
  headers. The demo pages' "New in this demo" note links each word to
  the source of the module it is imported from instead.
- **The skill's walkthrough.md is deleted** for the same reason. It
  copied flight-booker's two modules whole and restated writing.md's
  rules one line at a time, and its reading list repeated the README's.
  A guided read of a real app is a demo's own source, with SKILL.md's
  counter and demo/workflow.html's writing session for orientation. The
  skill is now SKILL.md, bootstrap.md, writing.md and building.md.
- **doc/why-bambik.md is folded into CLAUDE.md, and CLAUDE.md is cut
  from 154 kB to 34 kB.** The long-form introduction became CLAUDE.md's
  *Why at all* paragraph and *The algebra in brief* (shapes, merges,
  units, strengths, the trace quartet, the keyed square, `Category`,
  what the laws buy). The rest of CLAUDE.md had grown into a restatement
  of the module headers word by word, of guardrails.md, of writing.md
  and of every demo, with history inline. It now keeps what only it
  says: where each kind of statement lives, how to build, watch, verify,
  bundle, deploy and release, the core type and the cycle, a map of the
  source tree pointing at the headers, the vocabularies' differences,
  and the demo conventions. Its Core Type block showed the carrier
  before registration returned the input leg; it is now the current
  type.
- **Demo pages lose two notes**: the "See the code style note and how
  an app is written" line under the source listing, and the list of
  words the view imports beyond the reading order's earlier demos. The
  code style note and workflow.html stay reachable from the landing
  page; a word's contract is its module header. The tracing note under
  each running demo stays, trimmed to how to see the trace (DevTools'
  Verbose level) and what the accessibility tree shows.
- **`acknowledged` is defined once, in `PUI`, as a trailing word that
  takes no label.** Each of the five vocabularies with a dialog defined
  `acknowledged @l content` as one line over `PUI`'s `acknowledgedOver`.
  Those five and that name are gone. Each vocabulary exports `modal @l
  content` instead — the `+→+` leaf owning case `l`, titled by it, its
  "OK" releasing the case again (`PUI.modalOver` over its modal) — and
  `acknowledged` is the carrier's word written after it: `( modal
  @"Total missing" $ … ) # acknowledged` (order-form; weather,
  incident-report). It ends the case of the leaf it follows and lets
  every other case pass, and it reads that case off the leaf: a leaf
  closed to one case has a row `RowToList` can read. A vocabulary with
  no modal (Bootstrap) has none to write, so the gap stays a compile
  error. `confirmed` is unchanged.
- **Its body is `introduced`'s, turned round.** `onCase @l` over the
  leaf followed by `rmap case_ silent`, where a private helper rewrote
  the modal's registration to drop its release. `silent` (private) is
  the carrier's silent component at `[]`, the variant with no case: the
  point's mirror, needing no value. The counterpart of `init` is named
  in the header, the **exit** (`payload → [ | b ]`, the case leaving as
  one that goes on); an acknowledgement is the exit into `[]`.
  Law-tested in test/Main.purs with the exit that names a case; the
  bare `modal` joins the leaf-law bench.
- **`continued` and `supplied`: the two missing corners.** `continued
  exit` is `acknowledged`'s sibling and `introduced`'s exact mirror:
  the leaf it follows is handed its case, and what the leaf releases
  leaves as the case `exit :: payload → [ | b ]` names among those that
  go on. Weather carries it — the fetch has a second outcome, a storm
  warning the user must read, which then goes on as the forecast (`(
  modal @"Weather warning" $ … ) # continued asForecast`); a modal
  closed without its "OK" lets nothing go on
  (scripts/smoke/tests/modal-exits.mjs). `supplied` is `introduced`'s
  sibling and `acknowledged`'s exact mirror: the source it follows is
  fed nothing and its field joins the row fed, so a source may stand
  below a line that has
  released a field. Incident-report carries it: a description field
  introduced into the two facts, and the photo capture supplied under
  it (scripts/smoke/tests/supplied.mjs). Both read their label off the
  closed leaf they follow.
- **`introduced` cannot read its label the same way.** The editor it
  would follow is open on the rest of its row, and no solver rule
  yields a label from an open row (`RowToList` wants a closed one,
  `Cons` wants the label given): `filledTextField @"Email" # introduced
  blankEmail` stops at `RowToList ( "Email" :: String | t )`. It keeps
  its label.
- **The `…From` editors are deleted.** Nineteen one-line wrappers over
  `introduced`, one per editor per vocabulary (`filledTextFieldFrom`,
  `debouncedTextFieldFrom`, `radioButtonFrom`, `selectFrom`,
  `checkboxFrom` in MDC2 and MDC3; `textFieldFrom`, `selectFrom` in
  Shoelace and Bootstrap; `textFieldFrom`, `dropdownFrom`,
  `radioGroupFrom` in Fluent; `inputFrom`, `selectFrom` in HTML). A view
  writes the carrier's word after the editor, its label restated:
  `filledTextField @"Email" # introduced @"Email" blankEmail`
  (signup-form MDC3).

## 2026-11-15, later
- **One knot, and the event path a line of it.** An application has one
  `looped`, around every line that changes the model, and every line of
  its body passes the row it is fed. The one line that did not was the
  event path — `emitters >>> folds` answers no feed, so an edit above it never reached the knot,
  and a form of editors therefore kept a `# looped` of its own. The path
  is now wrapped, `( emitters; dialogs; folds ) # stepped`: fed a row it
  passes the row on, and an event releases the next model. The inner
  loops are gone (18 lines, 9 apps). Because the path is a line, a
  display may stand below the buttons, and a body may hold several event
  lines, each where its buttons are on the screen (todo-list, contacts,
  diner, ticker). `stepped` is derived: the wire and the path under the
  `×→+` merge, each owning its case (a feed, a step), landed by the
  lens.
- **A fold reads the model where its event lands.** `fold @l f` took
  `f` of the event's payload as the next model, and the payload of a
  button is the row it was last fed — the model as the click saw it. A
  `+→×` leaf now releases a **step**, `model -> model`, which `stepped`
  applies to the row the line was last fed: `fold @l f` applies `f` to
  that model and reads no payload; `foldInto @l f` takes `{ event,
  model }` for an event whose payload says what the model cannot (a
  pick, a canvas click, an action's outcome); `capture @l` is `foldInto
  @l captured`; a status takes its fold in `foldInto`'s shape
  (`snackbar @"Card charged" captured`, `kept`). So the `+→×` merge's
  shared output is any type, and no view joins an event with the model:
  `# joined @l` went from every top-level emitter, and an action stands
  after the emitters with its outcome's fold instead of being joined
  with its button — an outcome lands on the model as it is then, so
  order-form's estimate keeps an edit made while it ran with no join.
  `joined @l` stays for an emitter inside a list element whose event
  must carry that element's row (diner's plated line).
- **`landed`**, the bare lens under `stepped` (the step applied to the
  row its path was fed, no feed answered, the row's type free to
  change): the landing of a path before the knot, order-form's load
  onto the `{}` that `body` feeds.
- **`edits`, `told` and the context form withdrawn; `editsOf` back.**
  The context form (2026-11-14) typed a view as model in, events out,
  so everything that wrote had to be an entry of the emitter block, and
  `edits @l` turned editors into emitters: an update placed in a
  literal, the one emitter that told an edit from an answer by when it
  came, and seven cases that existed only to be captured. With the
  event path a line, an editor is a line again (`sliderLive
  @"Duration"`, crud's and contacts' forms, diner's table selector) and
  the eight apps' views write their folds, knot and seed; their modules
  import no `PUI` and hold the seed and the pure functions, as
  writing.md asks. `told`, a status as `+→+`, had the context form as
  its one use. `editsOf @l @k proj editor` returns as the editing list,
  derived as before (`gatheredOf @k proj` over `unanswering editor`),
  for a projection that joins or reshapes its rows; law-tested, no
  demo. The tests that drove counter's, crud's and diner's context
  words are gone with the words, and test/Main.purs imports no demo
  module.

## 2026-11-15
- **A source is fed `{}`.** `source @l` and the facts over it
  (`geolocated`, `online`, `clock`, `colorScheme`, HTML's `filePicked`)
  were open on their input, `{ | r } → { l :: a }`, though they read
  nothing of it: `editor >>> online @"connection"` compiled and dropped
  the editor's row. Their input is now the empty record, so a source
  after a stage that releases a row, or merged beside a stage that reads
  one, is a compile error, and a block of sources opens its flow. No
  demo changed: every source already stood at the head.
- **A progress indicator releases `{}`.** `indeterminateLinearProgress`
  and `indeterminateCircularProgress`, in every vocabulary, were open on
  their output, `[ started, ended ] → { | r }`, though they release
  nothing: `progress >>> display` compiled and the display was never
  fed. Their output is now the empty record, the row `action` and `load`
  already took their slot at. No demo changed.
- **An emitter block owns its cases.** The `×→+` merge was the one merge
  with no owned side: every emitter was open on its output and the
  block's row came from the folds after it, so `button @"Deposit"`
  written twice compiled, and so did a fold for a case nothing emitted.
  Its output is now owned (`OwnedVariantOutputs`, `Data.Profunctor.Row`):
  every emitter is closed to the case its label names (`Cons l a () v`,
  42 signatures in `PUI`, `PUI.Web` and the six vocabularies), `joined
  @l` leaves closed to the cases it names, and the block's row is the
  union of its operands' cases with duplicates removed, so the detector
  names a case written twice and the fold block must own exactly the
  cases the block emits. The merge is read as the pairing at
  occurrences — a record of event sources is one stream of tagged
  events — so every merge has one shared side and one owned side, the
  input deciding which. It reverses the 2026-10-15 entry that made the
  `×→+` output one shared row. No demo view changed: none emitted a
  case twice or folded a case nothing emits. timer's context word,
  which merges its `ticks` beside the view, types the view at the two
  cases the view emits. The law tests' `×→+` probes each own a case,
  and "a shared case exits from either operand" is replaced by "an
  operand's case enters the merged row as it is". writing.md's table
  of compiler messages gains the two new mistakes.
- **A line passes, an operand owns.** The placement rule the two changes
  above follow from is stated: writing.md *The pipeline* (merge what
  writes without reading, sequence everything else), CLAUDE.md
  *Composition Patterns*, and the keyed floor's header says which `…Of`
  form passes the fed value. No code changed.

## 2026-11-14
- **Applications over data** (doc/research-data-applications.md is the
  record, its *Costs* and *Open* included). A UI over one stored value
  is typed by how it reads (the state, or a change) and how it writes
  (the next state in place, or an operation an event carries and a fold
  applies). Three new apps, each MDC3 and MDC2: **venue** (a document
  edited in place as deep as it nests, `Venue → Venue`, no event),
  **contacts** (master and detail over one stored array) and **diner**
  (one value, three stations, seven operations, a path two `joined`
  deep). movie-browser, todo-list, shopping-cart and inbox were moved
  onto their data. 105 demos on 46 pages.
- **`actedWithin @l @t select`**, the updatable selection
  (`Data.Profunctor.Keyed.Acting`): the lens between `actedOf` (nothing
  written back) and `actedAt` (the array written back whole) — the rows
  `select` chooses of stored array `t` are edited and land back by key.
  Laws wire, landing and agreement, tested; movie-browser's in-row
  toggle through the filter and contacts' form of the picked row are it.
- **`edits @l` and `told`; `editsOf` deleted.** `edits @l editor` is an
  editor's edits leaving as case `l`, its answers dropped
  (`unanswering`); `told` is a status as a `+→+` stage, its event
  passed on. `editsOf @l @k proj` went on subsumption: it is `gatheredOf
  @k proj (edits @l editor)`, tested as the editing list. `edits`
  rests on its stage answering within its feed, so a stage under
  `debounced` is outside it; the header says so and nothing enforces it.
- **The context form**, in eight apps (counter, crud, timer, payment,
  todo-list, shopping-cart, contacts, diner): the view ends at its
  events, `PUI m model [ events ]`, and the app's module exports one
  word that closes it with the folds, the knot and the seed (`counted`,
  `catalogued`, `timed`, …), so those are no longer the view's to
  write. The other apps keep the four-shape loop in the view.
  **Not synced**: writing.md's app shape and import rule, guardrails,
  CLAUDE.md's demo tables and keyed bullet, vocabulary.md, SKILL.md and
  demo/index.html's `#code-style` still describe the four-shape view
  and `editsOf`.
- **A keyed element's run is read when it moves** (`PUI.Web`'s
  `Hosting`). An element that is a bare run of siblings with a pane
  among them left the pane's content behind when its key left: the run
  was recorded at build, and a pane attaches its content later. Found
  by contacts; scripts/smoke/tests/updatable-selection.mjs holds it.

## 2026-11-12
- **The cycle named.** The four shapes are documented as the four arrows
  of an application's loop, with one name for each row between them:
  `×→×` model → view model (presentation), `×→+` view model → view event
  (interaction), `+→+` view event → model event (interpretation), `+→×`
  model event → model (transition). Stated in guardrails L3 (normative
  for the docs), `PUI`'s header, CLAUDE.md and writing.md's *Terms*. The
  view model module keeps its name, as the module holding the functions
  of all four arrows. No code changed.
- **The runtime hole removed.** Guardrails L18 had two halves: every
  view runs with its view model stubbed to runtime `hole`s, and its
  typed holes report the module's signatures up to their tails. The
  first is gone: `PUI.Web.hole` and `PUI.isHole`; the hole-awareness of
  `announce`, `ticks`, `clock`, `debouncedTextField`, `action`, `load`,
  `introduced`, `text`, `each` and the four `…Of` forms;
  scripts/holes.mjs, holey.dhall, the holey smoke sweep and `npm run
  check-holes`. The second stays whole and checked: `npm run
  check-determined` passes on every demo, and `Data.Structural` stays,
  since an `Ord` or `Eq` asked of a key or field would be left unsolved
  beside a typed hole (measured on inbox and signup-form by asking
  them). check-determined stands a reported hole in with a coerced unit
  of its own, and `--write` appends an unwritten name over a typed hole.
  The skill's *Writing order* drops the runtime-hole steps.
  doc/research-holey-weak-types.md is the record of what was built.
- **The `…Of` forms are the keyed root's.** With no hole to skip,
  `gatheredOf`, `dispatchedOf` and `accumulatedOf` are `lcmap proj` over
  their bare forms and `actedOf` is `lens proj const` over `acted @l`
  (the projection the getter, the fed value kept by the setter), all
  four in `Data.Profunctor.Keyed.*` over any carrier; `PUI` re-exports
  them and keeps only `editsOf`. `actedAt` is type-changing, as `prop`
  and `dispatchedOn` are.
- **The floor's halves are dual.** `toCase` was open on its row (a case
  injected into whatever variant holds it) while `atField` was closed
  to the singleton; `atField` is now open the same way (a field read
  from whatever record holds it), so the two halves the tensors give
  for free are open and the two they do not, `toField` and `atCase`,
  are closed. `toField` and `toCase` lose a second visible type
  argument no caller passed. Cosmetic, with it: the keyed bare forms
  quantify their variables in one order, and `joined` states its row
  constraint before its strength, as the other row words do.
- **`introduced` derived.** It was one hand-written body. It is now
  `onField @l (lcmap init identity) >>> keepingEdits @l editor`: the
  insertion is `Strong`'s, and what is left to the carrier is one
  plumbing word, `PUI.keepingEdits @l` (an editor that keeps the user's
  value of field `l` across feeds that repeat the old one), with its
  three laws in its header and a probe test. `introduced` asks
  `Applicative m` where it asked `Functor m`, for the wire. `actedOf`
  takes an element of any output row, as `actedAt` does, since its lens
  discards it; `gatheredAt` and `accumulatedOn` are written as their
  `…Of` forms over a field getter and a case eliminator.
- **`onField @l`, `onCase`'s mirror.** `onCase` is the prism whose
  builder is `identity` (a case eliminated, the rest passing); the lens
  whose getter is `identity` was missing: `p { | b } a -> p { | b }
  { l :: a | b }`, a field computed by a stage that reads the row. It is
  `Data.Profunctor.Row.RecordToRecord.onField`, law-tested at `(->)`. `joined` is
  the same lens with the fed row distributed into the case, and is now
  written so. `JoinedCases` and its `+` move to the floor with the other
  row-constraint classes.
- **`joined2` deleted: `joined @(l1 + l2)`.** The two-outcome join was a
  numbered word, the same private body as `joined` under a second `Cons`
  chain. `joined` now takes its cases as one label or a `+`-list of them
  (`joined @("Order submitted" + "Total missing")`, order-form and
  cells), decided by a class over the label list, `JoinedCases`, whose
  two instances are the old two signatures' constraints, so a source
  over a hole is typed exactly as before (the holey sweep and
  `check-determined` unchanged). `+` is a type operator a view imports
  as `type (+)` from `PUI`; it lists a variant's summands and nothing
  else. The alternatives fail as recorded: a label-free `joined` cannot
  close an open emitter (2026-10-15), two chained `joined`s cannot both
  close the source and widen it, and a row argument is the view writing
  a row.
- **`editsOf` derived; `unanswering`.** `editsOf` hand-wrote a
  `toUser`/`fromUser` pair around a `feeding` flag. The flag is a word of
  its own, `PUI.unanswering` (plumbing): a stage with its answers
  dropped, what it emits inside its own feed withheld and the rest
  passing, the bridge between `×→×`'s must-answer and `×→+`'s must-not.
  `editsOf @l @k proj editor` is `gatheredOf @k proj` over `unanswering
  editor` with its release injected as case `l`. Laws (silence, passage,
  idempotence) in its header and in test/Main.purs.
- **`actedBy` deleted.** `acting` with the key as a function, left from
  the primitives that took one (2026-11-11) and reached by the law tests
  alone; no sibling class had a `…By`. The wire law and the laxity
  exhibit are stated and tested at `acted @l`.
- **`kept` moves to `PUI`.** It is the plain function `_.model`, a fold
  argument beside `reset`'s `const x` and `capture`'s `identity`, and
  was the one export of the four shape modules that mentions no
  profunctor. Views import it from `PUI` as before.
- **`Data.Profunctor.Keyed`.** The four keyed classes move from flat
  modules beside `Row/` into a directory with a floor, as the row shapes
  have: `Data.Profunctor.Keyed.{Acting,Dispatching,Gathering,Accumulating}`
  over `Data.Profunctor.Keyed`, which states the square once (the one
  formula `p a b -> p (⊗ₖ a) (⊕ₖ b)`, what each side's tensor decides,
  the key discipline, the three forms) and holds the two halves every
  label form is a `dimap` of, `keyed @l` and `keyRestored @l`, written
  inline four times before. Each member's `## The key` section now
  states only what is its own. No word, signature or behaviour changed;
  the demos import the forms from `PUI` and are untouched.
- **No keyed module imports a row shape's.** `actedAt` and
  `dispatchedOn` were `focusField @t` and `focusCase @c` over their bare
  forms, the only imports from `Data.Profunctor.Row.*` under `Keyed/`.
  They are now the ecosystem's `prop` and `prism` directly, as
  `focusField` and `focusCase` themselves are, so the square depends on
  the strengths and not on the row shapes. The cost is `focusCase`'s
  background coercion written a second time in `dispatchedOn`.
- **`extras/keyed-profunctor`.** With that import gone the keyed square
  and the row shapes are independent, so the square leaves
  `extras/row-profunctor` for a root of its own, as `Data.Structural`
  did. Module names are unchanged, and the one `extras/**/*.purs` glob
  covers the new root for the build and for consuming apps.
- **`Data.Lens.Prism.Existential` deleted.** `prismE`, the existential
  constructor of the ecosystem's `Prism`, had one caller, `focusCase`,
  and was `dimap decon recon <<< left` by another name. `focusCase` and
  `onCase` are now the ecosystem's own `Data.Lens.Prism.prism`, as
  `focusField` is `prop`, so the row root no longer imports
  `extras/lenses`, which the library does not reach at all.

## 2026-11-11
- **The merges stated by universal property.** `Data.Profunctor.Row`
  now gives each merge's sharing as a consequence, not a per-side rule
  with an exception: `×→×` is the pairing (input shared, output owned),
  `+→+` and `+→×` the copairing (input owned, output shared), `×→+` the
  carrier's interleaving (shared on both sides, no universal property).
  The old statement, "records own their output", was false of `+→×`
  since 2026-10-04; the input decides what a `×` output is. Each shape
  header names its construction. Documentation only.
- **`Data.Structural`.** The structural `Eq`/`Ord` of plain values moves
  from `Data.Profunctor.Row.Structural` to its own source root,
  `extras/structural`: it mentions no row and no profunctor. Its header
  states what it compares and its laws (agreement with lawful instances,
  an order consistent with the equality), now tested in test/Main.purs,
  and no longer says a selector uses it — none has since options became
  leaves.
- **The keyed square stated by one formula.** The four class primitives
  are unary, `p a b -> p (⊗ₖ a) (⊕ₖ b)` with the tensors of the class's
  strength: `acting :: p a b -> p (Array (Tuple k a)) (Array (Tuple k
  b))`, `dispatching :: p a b -> p (Tuple k a) (Tuple k b)`, `gathering
  :: p a b -> p (Array (Tuple k a)) (Tuple k b)`, `accumulating :: p a b
  -> p (Tuple k a) (Array (Tuple k b))`. They took the key as a function
  (`actedBy` and its three siblings) until now; with the key beside the
  element and out of its reach, unforgeable identity is the type's, and
  the label forms `acted @l`/`dispatched @l`/`gathered @l`/`accumulated
  @l` are `dimap`s that pair the key and write it back — the `second`
  around the element and the `Strong` constraint are gone from all of
  them. `actedBy` stays as a derived function, the form the wire and
  laxity laws are written in.
- **`Accumulating` gathers through the gate.** Its hand-written slots
  and busy flag are replaced by `PUI.Gate`, the keys enrolled in
  first-appearance order: the array is withheld until every key's element
  has spoken and re-released on every entity and emission after. With
  that the element is `p a b` (it was `p a a`, the fed row priming its
  slot), and `accumulated`/`accumulatedOf`/`accumulatedOn` take a separate
  output row. One behaviour changes: an element that does not answer its
  feed withholds the board where the fed row used to fill its slot; over
  displays and editors, which answer every feed, each entity releases
  once as before. No demo changed.
- **`Data.Profunctor.Cont` deleted.** The CPS profunctor `Cont r a b =
  (b -> r) -> (a -> r)` was the one carrier under `extras/profunctor`, kept
  in the build glob for its header: an inventory of which classes a pure
  carrier inhabits and which it cannot. Nothing in the library, the demos
  or the tests reached it, none of its instances ran a law, and its
  impossibility half was prose the compiler never checked — while its
  claim to be exhaustive made every change to a class a change here too
  (24 commits in three months). The impossibilities that do work stay
  where they are used: the class headers (`Resolving`, `Retaining`,
  `Gathering`, `Accumulating`) state why `(->)` has no instance, and
  `PUI`'s headers state why the knot and the point are carrier words.
  With it go the only pure instances of `Gathering` and of the `×→+`
  merge (both under `Monoid r`), so those two are `PUI`-only; `(->)`
  remains the pure carrier the value-level laws run on.
- **`PointedCostrong` deleted.** The class `unfirstFrom :: c -> p (Tuple a
  c) (Tuple b c) -> p a b` — `unfirst` with its state channel started at
  `c` — had been reached by its own two tests alone since the field-level
  trace forms were retired (2026-10-05): no library word stood on it, the
  one-field `×`-knot it would seed has no word by design (a looped state
  is a model field), and the fact it named is already stated and tested on
  plain `Costrong`, as the seeded retraction `unfirst (seeded (Tuple a0
  c0) >>> first g) ≈ seeded a0 >>> g`. Its module, its `(->)` and `PUI`
  instances and its tests go.
- **The `(->)` instances deleted.** `RecordToRecord`, `VariantToVariant`,
  `Acting` and `Dispatching` each had a function instance (`Record.union`,
  the case split, `map`, the function itself) kept as the pure carrier
  the diagonal laws ran on as equalities. No library word or demo reached
  one; they go on `Cont`'s grounds, with their tests (the `×→×` zip,
  exactness, symmetry and associativity, the `+→+` dispatch and
  associativity, `acted`/`dispatched` on a function). The same laws stay
  tested on `PUI Effect` probes and, over every script to a bound, in
  test/Exhaustive.purs. Every class of the row algebra and the keyed
  square now has `PUI m` as its one instance. The ecosystem's own `(->)`
  instances (`Strong`, `Choice`) are untouched, so `focusField`,
  `focusCase` and the single-label grid are still tested on functions.

## 2026-11-09
- **`actedOf` derived.** The list of displays is now `first` around
  `acted @l` with the gathered output dropped — `dimap (\a -> Tuple (rows
  a) a) snd (first (acted @l w))` — instead of the body `foreach` had
  carried since the container action was introduced: the gathering
  reconciler with its emissions discarded and the fed value echoed
  regardless of the elements. The twelve words of the keyed square are now
  all derived from the four `…By` primitives through `Strong`, `Choice`
  and `lcmap`, and the eleventh was the only one that was not. What
  changes: the list's release is `acted`'s gather gate, so the fed row
  passes once the elements have answered — once per feed over displays,
  which answer every feed by the `×→×` laws, so no demo changed — and an
  element that does not answer starves the list and is named by the
  watchdog where it used to be passed over; an element emitting outside a
  feed re-releases the fed row, a repetition. The value test's probe
  became a display (it echoes its feed) and states the repetition.

## 2026-11-08
- **The keyed square: four classes, twelve words, one shape of
  derivation.** `Acting` (`×→×`) gets its three siblings, each the keyed
  analogue of a static strength and each a module beside it:
  `Dispatching` (`+→+`, `Choice`'s; `dispatchedBy`, instances `(->)`,
  `Cont`, `PUI`), `Gathering` (`×→+`, `Resolving`'s; `gatheredBy`,
  instances `Monoid r => Cont r` and `PUI`, no `(->)`: one value cannot
  forward many) and `Accumulating` (`+→×`, `Retaining`'s;
  `accumulatedBy`, `PUI` only — the other keys' slots are state, the
  square's one stated carrier-only corner, as `Retaining` is among the
  static four). Every class has the same three derived forms, so the
  words are symmetric across the shapes: the bare word over the element
  typed at its whole row with its key field (`acted`, `gathered`,
  `dispatched`, `accumulated`, the product-output ones re-setting the key
  from the input through the `Strong` state channel), `…Of @l proj` over
  a projection (`PUI`, hole-aware: `actedOf`, `gatheredOf`,
  `dispatchedOf`, `accumulatedOf`, and `editsOf @l @k` as `gatheredOf`
  over an editor), and the stored form, `…At @l @t` over an array the
  model stores for a `×` input (`actedAt`, `gatheredAt`) or `…On @l @c`
  over the case an entity arrives in for a `+` input (`dispatchedOn`,
  `accumulatedOn`). **Renamed**: `foreach` → `actedOf`, `anyOf` →
  `gatheredOf`, `anyOfAt` → `gatheredAt`, `anyEdit` → `editsOf`;
  `dispatched proj`/`accumulated proj` → `dispatchedOf @l proj`/
  `accumulatedOf @l proj`, the `{ key, value }` envelope gone: a `+`
  member reads identity off the entity's own row, as a `×` member reads
  it off each element (L9), so departures' and scoreboard's view models
  name the key field (`code`, `team`) instead of wrapping it. `PUI` keeps
  the `…Of` forms (where `isHole` is) and the four generic `Hosting m
  node =>` instances; the laws of all four run in test/Main.purs on `(->)`
  where an instance exists and on `PUI Effect` probes everywhere;
  `Cont`'s inventory names what it inhabits and the one it provably
  cannot.

## 2026-11-07
- **`rangeSlider`/`rangeSliderLive` in MDC2 and MDC3** (MDC3 also the
  `'` siblings reading in a `unit`): the catalogues' two-thumb slider,
  editing a field whose value is a bounded interval `{ start, end, min,
  max, step }` — the one MDC control the vocabularies did not expose
  (the Material Design 3 `md-slider range`, MDC2's `mdc-slider--range`
  with `getValueStart`/`setValueStart`). A field of this shape was
  reachable only as two sliders in a `group`. Law-bench rows on both
  pages; no demo reaches it yet (L14's catalogue-word clause).
- **Number fields, as two-case fields.** `PUI.numeral @n @i` reads a
  text leaf as a field `[ n :: Number, i :: String ]`: the number when
  the text parses (strictly — digits, one point, a leading minus), the
  text itself when it does not, so an invalid entry is model state a
  pane shows and a `provided` withholds on. A leaf typed at a bare
  `Number` was rejected in the same discussion: it keeps the last parse
  while the screen shows "abc", and the next broadcast erases the user's
  text. `filledNumberField`/`outlinedNumberField @l @n @i` in MDC2 and
  MDC3 are it over their text leaves; law-bench rows on both pages. The
  other "closed by rule" cells of the editor grid stay closed: a
  payload-carrying choice edited in place (L14, state the row does not
  hold), a bare payload amended in a dialog or a bare element edited in
  a list (L3, no label-less leaf), a variant at the knot (the deleted
  `cycled`, 2026-10-06).

## 2026-11-06
- **`group @l` in every vocabulary.** The labelled model group, the one
  editor of a field whose value is a record, existed in MDC2 and MDC3
  only, so the other four vocabularies could not nest a sub-record at
  all — the one cell of the field/case × record/variant grid a view
  could reach and had no word for. It is now in Shoelace (an `sl-card`
  with the label in its `header` slot), Fluent and Bootstrap (their
  `card` with a `title3` / `card-title` heading), each stamped
  `role="group"` + `aria-labelledby` as the Material ones are, and HTML
  (a `fieldset` whose `legend` is the label, which the platform names by
  itself — `fieldset` and `legend` join the element oculars). Same
  derivation everywhere, `surface (heading >>> focusField @l w)`. parcel
  gains its four twins and the leaf-law bench a `group` row per page.
- **`×→×` is the view model; the header says so.** `RecordToRecord`'s
  header now opens with what the shape models — the one row the screen
  and the app share, every field typed as the screen represents it, the
  data model never a row but the payload of the folds it enters by and
  the actions it leaves by — and states the shape's two laws, Repetition
  and Answer, apart from the four merge laws it reads at this shape.
  A **validating stage** `p { l :: String } { l' :: Number }` (a parse
  showing its failure and passing only valid rows) was proposed and
  rejected as a stage: withholding in `>>>` reaches nothing, the stages
  after it keep the row they last held and an emitter among them
  replays it. A decision is a classifier handed to a pane, which
  detaches its content instead. What the proposal would reopen is the
  pane that feeds its content the case's payload, dropped 2026-10-13
  when panes stopped narrowing; its honest home would be the knotless
  form, as a row-growing pane beside `introduced`.

## 2026-10-19
- **Fixed: an edit undone by the next edit above it.** Nine apps — crud,
  todo-list, password-generator, espresso-bar, movie-browser (all
  twins), color-mixer, loan-calculator, product-review and signup-form's
  MDC2 twin — had several editors before an emitter under the app's one
  `looped`. The emitter answers a feed with nothing, so an edit never
  reached the loop and the editors above it kept the row of their last
  feed: picking a size after a roast put the roast back. Each now keeps
  its editors in a `( … ) # looped` of their own, as writing.md's app
  shape requires and order-form and flight-booker already did; the rule
  is now stated as a must, and the option-leaves walk checks that a pick
  survives other edits.
- **A selector group knows its options by composition.** The builder
  state carries the group being built: `grouping` builds a group and
  `member` registers an option in it (its element, whether it shows
  itself picked, how it stops), so the radio, segment, tab and stepper
  groups clear, name and scan their options from the members list
  instead of matching class names in the DOM. `clearedByRepress` takes
  only the press snapshot and reads the members; `eachMatching` and
  `Members` are deleted.
- **Introducing editors in every vocabulary.** The `…From` forms the
  MDC3 pilot introduced (2026-10-17) are now in every vocabulary that has
  the editor: MDC2's same five, `textFieldFrom`/`selectFrom` (Shoelace,
  Bootstrap), `textFieldFrom`/`dropdownFrom`/`radioGroupFrom` (Fluent),
  `inputFrom`/`selectFrom` (HTML) — each `introduced @l init` over the
  lens editor, so a knotless form switches design system with its import.
- **No runtime string in markup; `escaped` deleted.** Every caption,
  label, title, icon name and URL that MDC2, MDC3 and the checkout
  steppers interpolated into `staticHTML` (31 sites) is now a
  `staticText` or an attribute on an element built with `el`; the
  constant markup a catalogue documents stays a string. With nothing
  left to escape, `PUI.Web.escaped` is deleted. The steppers take
  `fieldLeaf`, so no demo lifts a focus itself again.
- **`fieldLeaf @l` and `chrome` in `PUI.Web`.** `fieldLeaf @l leaf` is
  `focusField @l` over the leaf with the label stamped as its `name`, the
  line all 81 editors and selectors across the vocabularies had spelled
  out; `chrome w` places a component a leaf's builder does not wire,
  replacing the 75 `_ <- handles …`/`void $ handles …` lines, so
  `handles` is left to the components a builder actually wires.
- **Leaves written once for every vocabulary.** `eventLeaf` moves from
  `PUI.Web` to `PUI`, over any `Interacting` carrier — it only composes
  `replayed` and `occurrencesOn`. Beside it, `statusOver @l face f` (a
  status over its face: `snackbar = statusOver @l snackbarFace`) and
  `confirmedOver`/`acknowledgedOver @l modal` (the two dialogs over a
  vocabulary's `Modal`) replace the definitions each vocabulary had
  copied verbatim; `acknowledgedOver` writes off the modal's release
  on the carrier rather than through a hand-written `toUser`/`fromUser` pair.
- **Selectors are groups over option leaves.** A selector's field holds a
  variant of empty cases, so its options are leaves, one per case, merged
  by the existing `VariantToVariant.do`: `radioButton @"Roast" $
  VariantToVariant.do radio @"Light"; radio @"Dark"`, one leaf per line.
  The merge's owned inputs close the field's row and keep the order
  written (a case written twice is its `DisjointLabels` error); the group
  lifts the block with `focusField @l` and clears its options before
  every feed. Each option is a leaf over the new `PUI.optionWiring` (fed
  its case: show itself picked and echo; picked: emit the case) — `radio`,
  `segment`, `option` (selects and dropdowns), `tab`/`tab'` — in every
  vocabulary that has the group. `…Unpicked` is the group around
  `focusCase @c` over the options; `…Optional` adds the clearing gesture
  as a group-level emission of the none case (`PUI.Web.clearedByRepress`
  for radios and segments, an empty first option for selects, Shoelace's
  clear button). Presentation of one option rides its primed leaf:
  `tab' @"Espresso" { icon: "coffee" }` replaces `tabBar'`'s record keyed
  by every case.
- **Deleted with it:** `Choices`, `choice`, `<+>`, `choiceTags`, `tagged`,
  `taggedTotal`, `PerChoice`/`perChoice`, `clearedOnRepress`, the lifts
  `selectedAt`/`selectedUnpickedAt`/`selectedOptionalAt`, and
  `recordToVariantWiring` (the index-based selector wiring). The option
  list had been a runtime array of tags beside the field's type, linked to
  it only by each vocabulary's signature; an option block is the field's
  cases. Every demo with a selector is converted, checkout's packaged
  stepper among them (a group over `step` leaves whose Back and Next pick
  the neighbouring step), and the leaf-law benches cover every group, the
  clearing gestures included; scripts/smoke/tests/option-leaves*.mjs walk
  espresso-bar (MDC2, MDC3), checkout's stepper and meeting-booker.

## 2026-10-18
- **The leaf protocol moves to `PUI`.** The four wirings
  (`recordToRecordWiring`, `recordToVariantWiring`,
  `variantToRecordWiring`, `variantToVariantWiring`) left `PUI.Web` for
  `PUI`, exported as plumbing beside `folding`. They touch no DOM — every
  platform fact reaches them as a callback — so they are the carrier's,
  not the web layer's. Their record type `Wiring a b` was `PUI`'s
  `Handles a b` under a second name and is deleted; they return `Handles`.
  The vocabularies and checkout's packaged steppers import them from
  `PUI`.
- **More host-neutral plumbing follows it.** `selectedAt`,
  `selectedUnpickedAt`, `selectedOptionalAt` (with their private
  `selectedWith`), `replayed` and `source` moved from `PUI.Web` to `PUI`,
  generalized from `PUI Web` to the carrier they need — `MonadEffect m`
  for the selector lifts, `Functor m` for `replayed`, `Scoped m` for
  `source` (as `ticks`). The browser facts over `source` and the click
  sources `replayed` is applied to stay in `PUI.Web`.
- **`PUI.Web` thinned.** `clickedRow` (one caller) is inlined into
  `eventLeaf`, and `occurrences` (an alias) into its callers as
  `occurrencesOn "click"`; `uniqueId` is the foreign import itself rather
  than an alias over `randomElementId`; the unused `textContent` FFI is
  deleted.
- **The panes and the occurrence sources leave the web layer.** `Hosting`
  gains `attachable` (a pane's content built parked in a suspended scope,
  shown and parked on demand — the DOM fragment juggling is `Web`'s
  instance, a scope flip the `Effect` probe's), so `inCase`, `inCaseAt`,
  `provided`, `providedAt` and their shared `attachedOn` move to `PUI`
  over any `Hosting` carrier; `PUI.Web` re-exports the four public ones,
  so views import them where they did. A new `class Interacting m`
  (`gestured`, `focused`; instances `Web` and the never-firing `Effect`
  probe) carries what `occurrencesOn` and `departures` read, and both move
  to `PUI`. The event name becomes a closed `Gesture` (`Click`,
  `DoubleClick`) instead of a DOM string.

## 2026-10-17
- **The merge laws lose their unit laws.** Law 3 was *Monoid*: both unit
  laws on the nose, symmetry and associativity up to `≈`. It is now
  *Symmetry and associativity*, so a merge is a commutative semigroup on
  labelled rows, and the unit probes are gone: eight laws from
  test/Exhaustive.purs (left and right unit at each shape) and their
  hand-written twins in test/Main.purs, the `(->)` ones included. What a
  unit is still holds by construction and stays stated where it does
  work: the `{}` wire, `lcmap case_ identity`, the carrier's silent
  component as what an absent emitter is, `unshown @l`. Guardrail L5 keeps
  its MUST NOT (no merge carries a unit of its own); the zero-field rule it
  rested on is L6's and keeps its probes.
- **The merge laws revised against the code.** `exact` (law 4) is a trim
  only where the operands own their outputs, `×→×`; at the three shapes
  that share their output row it is the identity — the `+→×` cell had
  said "trim", which the copairing never did. Preservation at `×→+`
  states its repetition half, now checked exhaustively over `joined`
  emitters, the derivation every vocabulary emitter is. Arming (`×→+`'s
  Answer) had been checked as "a script with its feeds removed emits the
  same", which only an emitter that ignores its feeds satisfies — a
  `joined` emitter fails it, since a feed arms the click it replays — and
  is now checked as stated: no emission within a feed's step. The `×→+`
  replay probe in test/Main.purs drives `joined` around a click instead of
  a hand-written replay source. A pointed component (`announce`, a source)
  is stated to release at registration, which the laws permit; "source"
  in the `×→+` header became "emitter", the word now naming the `×→×`
  platform facts; the `clicked` and `toCase` citations went.

- **Neither dialog takes a caption; `confirmed` has a Cancel;
  `acknowledged` tells two more outcomes.** Every `confirmed @l caption` restated its case
  (`@"Book" "Book"`, `@"Refund a customer" "Refund"`, `@"Delete message"
  "Delete"`), so the case captions the confirm button, as an emitter
  draws its label: `confirmed @"Refund a customer" $ body1 $ text
  refundLine`. With the case on the button the dialog has no title of
  its own (its content states what is at stake; the case still names it
  for assistive technology), and it gains a Cancel before the confirm in
  all five vocabularies — before, a decision could be declined only by
  Escape or the scrim, which nothing on screen offered.
  `variantToVariantWiring` takes `cancels` beside `reports`: it closes,
  releases nothing and forgets the row shown; the leaf-law bench clicks
  the cancel first, checks it emits nothing and closes, and that the
  next feed reopens it. `acknowledged @l` keeps its title and loses
  its caption: every call passed "OK" or "Got it", and its one button
  decides nothing — closing it, Escape and the scrim all consume the
  case — so, like `confirmed`'s Cancel, the dismiss is chrome, drawn
  "OK" in all five vocabularies. It gains two demos:
  order-form's rejection `acknowledged @"Total missing" $ …
  staticText "Enter the order's total before submitting it."`, where a
  snackbar flashed the reason, and incident-report's `acknowledged
  @"Report filed" $ … text referenceLine`, the filing returning a
  reference the user must keep — data a five-second snackbar could not
  hold, so the outcome had carried `{}`.
- **Flight-booker confirms before it books.** The dialog Book opens
  stands before any booking, so it is `confirmed @"Book" $ … text
  bookingLine`, "You are going to book a one-way flight on
  27.03.2026.", not `acknowledged @"Book" "OK"` reporting "You have
  booked…" for a booking nothing had made; the confirmed case leaves the
  app, the task modelling no booking beyond the message.
- **The 7GUIs demos meet the task statements.** Read against
  <https://eugenkiss.github.io/7guis/tasks/> (doc/7guis-comparison.md:
  17 → 22 of 23). Temperature-converter starts empty, and each field's
  `settled` rewrites the other only when the two disagree, so a round
  trip writes no float noise back. Flight-booker's options are the
  spec's words (`"one-way flight"`, `"return flight"`), a date the
  calendar lacks is rejected, each date field carries `# attrWith
  "aria-invalid" …` which the page colours red, and Book opens
  `acknowledged @"Book" "OK" $ … text bookedLine`, the message stating
  the selection — its debounced itinerary line, simulated airline,
  progress bar and snackbar, none of which the spec names, are gone.
  Circle-drawer selects the circle nearest the pointer by hover, opens
  a popup "Adjust diameter.." on a right click, adjusts in a frame with
  the live slider, and commits the session as one undo step on Close;
  its `drag` flag became the `mode` field `[ drawing, menu, adjusting {
  before } ]`. Cells has 100 rows, is edited in place on a double
  click, commits when the cell is left, and stores its values,
  re-evaluating only the edited cell and its dependents in dependency
  order (memoised, so a diamond of references is no longer
  exponential); the formula bar and the "Cell X" line are gone. The
  library words this needed: `onHoveredXY @l` and `onContextMenuXY @l`
  beside `onClickedXY` (now primary-button only) in `PUI.Web`;
  `doubleClickTile @l @k` in MDC3 (with `occurrencesOn`, `occurrences`
  at another platform event); `inPlaceInput @l @k` in HTML (with
  `departures`, the occurrence of the focus leaving a container). All
  four new leaves are on the leaf-law benches. Disabled stays absent
  (the panes), by decision. The `lists` dependency went with cells'
  `Data.List`. walkthrough.md reads the new flight-booker; the docs that
  cited its action and snackbar cite payment's.
- **Crud is the benchmark and no more.** The 7GUIs task asks for an
  in-memory list; crud carried a simulated server (`Crud.PeopleServer`,
  300 ms round-trips), a load before the knot, a progress bar per action
  and six outcome snackbars, none of which the spec names. It is now a
  seed (`# looped # with freshCatalogue`) and four silent folds;
  Create, Update and Delete are pure `Catalogue -> Catalogue`. Update
  and Delete exist only while a person is picked (`# providedAt
  @"picked" @"selected"`, the spec's enabled-iff-selected), and the
  prefix filter's `# settled filterChanged` drops a selection it hides,
  closing both CRUD gaps in doc/7guis-comparison.md. The docs that cited
  crud for `load`, `action` and outcome statuses now cite order-form,
  flight-booker, espresso-bar and reorder, which still show them.
- **The 7GUIs demos reviewed as the showcase.** Flight-booker had three
  panes over a three-case `bookingState`, each holding the same display
  over its own copy function (`problemLine`, `oneWayLine`, `returnLine`),
  two of which re-parsed the dates and returned `""` in states their
  pane never showed. The states differed in words, not in what is on
  screen, so they are one line, `text bookingLine`, debounced as before.
  The classifier moved to where structure does differ: the Book button
  is an emitter pane, `# provided @"bookable" bookingState` — the 7GUIs
  task's disabled button by construction — so `submit` is handed only a
  bookable trip, its `"Booking rejected"` outcome and its status are
  gone, and it waits a moment like a real airline so its progress bar
  shows; walkthrough.md reads the new code. Timer ticks every tenth of a
  second (`elapsedTenths`), so its gauge moves rather than jumps.
  Circle-drawer's undo push is a row update (`snapshot`) instead of a
  partial record re-plumbed by hand, crud's two one-line update helpers
  are inlined, cells' and the rest's layout follows the code style
  (signatures, import order). Stale citations went with it:
  `"Order rejected: no total"` (order-form says `"Total missing"`), the
  rule for an action's outcome row on a view line (views write none),
  and counter's MDC3 comments (it has none).
- **The wirings are named by shape.** `editorWiring`, `displayWiring`,
  `selectorWiring` and `statusWiring` (2026-10-11) were named by leaf
  kind while the library already names the shapes. They are now
  `recordToRecordWiring`, `recordToVariantWiring` and
  `variantToRecordWiring`, one per shape a leaf answers in. Two leaf
  kinds shared `×→×`: a display was the editor protocol with nothing to
  report, so `displayWiring` is gone and its nine call sites pass
  `recordToRecordWiring` `held: pure false`, no reports and `read: pure`.
  The selector's wiring was never `×→×`: it answers no feed and reports
  the user's pick, the `×→+` modality at the leaf, with `selectedAt`
  adding the answer because the leaf's `Maybe` loses the field — hence
  `recordToVariantWiring`. Emitters stay `eventLeaf`, a derivation.
- **The `+→+` wiring, and dialogs in every vocabulary with one.** The
  shape had no wiring because no vocabulary had a `+→+` leaf until the
  dialogs, which then assembled the protocol by hand in each Material
  module: `init`'s hooks to open on a feed and close after a report, and
  `witnessed`, a record-output button, to release the content's row.
  `variantToVariantWiring { open, close, content, reports }` (`PUI.Web`)
  is that protocol once — the leaf `a → a`, a case's payload, lifted by
  `focusCase @l` as an editor's `a → a` is by `focusField @l`: a feed
  opens the dialog and feeds its content stage, answering nothing; the
  deciding gesture closes it and releases the content's last answer
  once; a dismiss the platform performs (Escape, scrim) releases
  nothing. Both Material modals are rebuilt on it, `witnessed` is
  deleted (subsumed), and `confirmed`/`acknowledged` join HTML (the
  native `<dialog>`, opened with `showModal`), Shoelace (`<sl-dialog>`)
  and Fluent (`<fluent-dialog>` with `<fluent-dialog-body>`), each a
  face over the one wiring, so a dialog switches design system with the
  import like every other leaf. `PUI.Web` gains `invokeMethod` (an
  element's own show/hide, whatever the catalogue names them) and
  `showModalOnce`. Bootstrap stays without one: its modal is its
  JavaScript plugin, and the vocabulary is CSS-only. The leaf-law bench
  mounts both dialogs on every page that has them.

- **The introducing editor.** A knotless form states each field's
  blank at its editor instead of in a seed: `introduced @l init editor`
  (`PUI`) lifts a lens editor to `{ | b } → { l :: a | b }`, and MDC3
  ships `filledTextFieldFrom`, `debouncedTextFieldFrom`,
  `radioButtonFrom`, `selectFrom` and `checkboxFrom` over it.
  signup-form's MDC3 twin is the first user: no `looped`, no `with`,
  the seed taken apart into five init functions (`blankUsername`,
  `defaultPlan`, …); its MDC2 twin keeps the loop. Piloted on a branch
  first (doc/research-form-editors-pilot.md), the consequences accepted
  by Eryk with the word: the lift **retains** the user's last value and
  re-shows `init` only when it changed (structural equality), a state
  the row does not show, since a bare `lcmap` of an insert re-shows
  every init on any upstream edit; an init is typed at the row the
  editors above it built, so screen order reaches the view model's
  signatures; and an editor under a pane cannot introduce its field
  (flight-booker's Return date failed to type), so a conditional field
  stays model state. A hole init introduces nothing (L18).
  scripts/smoke/tests/introducing-editors.mjs walks the form: an
  upstream edit keeps every value set below it.

- **The dialog is the variant dual of the lens editor.** An editor is
  `focusField @l` over a leaf that shows a field and lets the user
  change it; the library had the dual lift, `focusCase @l`, and no
  vocabulary used it. Now the dialog is that lift: `confirmed @l caption
  content` (MDC2, MDC3) is `focusCase @l` over a modal whose content is
  a stage over the event's payload — opened by case `l`, which it tells
  as its title, the confirm re-emitting the case carrying the row the
  content answered, a cancel consuming it, every other case passing — a
  `+→+` stage between the emitters and the folds, its fold among the
  folds (`confirmed @"Refund a customer" "Refund" $ bodyLarge $ text
  refundLine`, then `fold @"Refund a customer" refundStandard`,
  cashbox). Until today it was a `+→×` leaf folding `f` on confirm,
  the one word that both intercepted and folded, so the fold block held
  a dialog where every other line was a fold. The sibling for the
  message the user must read and can only close is `acknowledged @l
  caption content`: the same modal with a dismiss over `onCase @l`, the
  case **consumed** — `[ l :: a | b ] → [ | b ]`, so what follows is
  typed at the cases that go on (weather's About notice, which folded
  `identity` before). Its content may read the payload, which a status
  may not (2026-10-14): a status is ephemeral, a dialog is not, so the
  2×2 of the `+` side is complete — the event dies or folds, the face
  is gone by itself or dismissed — with told-only snackbars (`kept`),
  `acknowledged`, folding snackbars and `confirmed` in its four cells.
  Inbox's deletion is now one `confirmed @"Delete message"` on the
  button's event and one fold: its `deletion :: [ confirming, silent ]`
  field, `requestDelete`, `keepMessages`, the `dialog` pane and its
  `Delete`/`Keep` buttons are gone (a modal is never a model field), and
  it confirms every deletion, not only the last message's. The bare
  `dialog` ocular stays as a catalogue word.
  `onCase` joins `focusCase` in `Data.Profunctor.Row.VariantToVariant`,
  derived over ecosystem `Choice` with no `Lacks` or `Union`, so nothing
  is stuck on an open event row under holes; `focusCase` itself drops
  its `Union b v2 v1` for the two `Cons` `focusField` has (the
  background is what both rows share, written as one coercion where
  `expand` would restate the constraint). The leaf-law bench mounts both
  dialogs on the MDC2 and MDC3 pages and checks the modal protocol: a
  feed opens and fires nothing, the confirm re-emits the fed case with
  the row shown, the dismiss emits nothing. The `+→+` module header now
  names the dialogs as the shape's leaves that wait beside the
  `action`s, where it said no word passed background cases through a
  `+→+` stage (2026-10-13).

## 2026-10-16

- **Every demo names its shared rows.** With the synonym ban lifted
  (flight-booker first), every view model module now writes its model
  row once as a `type` synonym in business language (`Count`, `Booking`,
  `Order`, `Inbox`, `Stopwatch`, …) and names repeated sub-rows likewise
  (option variants, bounded quantities, element rows, payloads), each
  exported where an exported signature uses it; signatures shrank to
  one line where they fit (about 1900 lines fewer). Views unchanged.

- **A label is only the name.** Labels carried the copy whole, format
  hints and units included (`@"Start date (DD.MM.YYYY)"`, `@"Amount
  (€)"`, decided 2026-10-07 when a `hint:` option was reverted), so a
  developer naming the field in the logic had to recall its
  presentation verbatim (`r."I'd recommend it to a friend"`). A label is
  still the caption it draws (no mapping between copy and identifier),
  but now only the name: a format or an example rides `hint` and a unit
  rides `unit`, on primed siblings — `filledTextField'`/
  `outlinedTextField'` (MDC2, MDC3), `textField'` (Fluent, Shoelace,
  Bootstrap), `slider'`/`sliderLive'` (MDC3; Fluent `slider'`, Shoelace
  and Bootstrap `sliderLive'`; MDC2's slider draws no number, so none).
  Questions and sentences are cut to the name they ask for (`"Todo"`,
  `"Owned for"`, `"Teams link"`, `"Recommend to a friend"`, `"Accept the
  terms of service"`, `"Name"` for `"Your name"`), a fact inside an
  option moved to the model it was already duplicated in (meeting-booker's
  rooms lose their `(4 seats)`, `roomCapacity` keeping the count), a
  reason inside a case became the case (`"Total missing"` for `"Order
  rejected: no total"`), and option labels stopped being shaped for
  one sentence (`"Oat milk"` for `"with oat milk"`, `"Cash"` for
  `"cash"`, `"15 min"` for `"15"` under `"Duration (min)"`), the grammar
  moving to the copy function (espresso-bar's `milkText`, `toLower` on
  read-backs). The plain-HTML flight-booker keeps its hint in its static
  caption, where plain HTML puts captions.

- **A choice is its tag.** Selector options were
  `Array { value :: a, label :: String }`, and every one in the demos was
  built by `choice @l`, whose label was always the case's — the record
  carried the same tag twice, and a hand-written one could caption a
  case with other copy, which L3 forbids. Options are now `Choices w`
  (constructor unexported): the tags in the order written over the
  closed row `w` of `{}` cases, built only by `choice` and `<+>`, so a
  choice's tag is its whole value. Every selector face runs over the
  tags (`Array String`, picks `Maybe String`) and `tagged`/`taggedTotal`
  lift it to the field's variant, so no selector needs `Eq` or
  `withStructuralEq` any more (`clearedOnRepress` neither). The tab
  bar's per-option `icon` (`OptIcon`, unreached) went with the record and
  came back as the primed sibling `tabBar' { icons }` (MDC2/MDC3), the
  icons a record labelled by exactly the choices' cases (`PerChoice`), so
  a missing or extra icon is a compile error. Demo views unchanged; the
  checkout steppers, order-dashboard's `rangePicker` and the law benches
  restate the option type.

- **Registration returns the input leg; routing needs no state.** The
  carrier was `m { toUser, fromUser }`: two closures made apart, so even
  `identity` had to hold the continuation in a `Ref` for `toUser` to
  find, and `Category` demanded `MonadEffect`. It is now
  `PUI m i o = PUI (m ((o -> Effect Unit) -> Effect (i -> Effect Unit)))`:
  registering hands the continuation over and returns the input leg, so
  a feed cannot precede registration. `identity` is `PUI $ pure pure`
  (`Applicative`), `compose` `Apply`, `Profunctor`/`Choice`/`Cochoice`/
  `looped`/`Strong` `Functor` (`Strong`'s pair state and the knot's
  re-entry cell are allocated at registration, once per instantiation),
  `announce`/`seeded`/`with` and the three ungated merges `Applicative`.
  The rest followed the same day: the trace quartet, `Resolving`/
  `resolveFor` and `Retaining` allocate at registration (`Functor`), the
  `×→×` merge's gate too (`Apply`), so `fold`/`reset`/`capture`/`folding`
  and `debounced` are `Applicative`. `MonadEffect` is left to `Scoped`,
  `Hosting` and `handles`, which genuinely build in `m`. Leaves written as two legs adapt through `fromHandles`;
  `handles` gives a component's two legs back, registering on the first
  leg used — a child a leaf feeds but never subscribes to (the HTML
  button's caption) works unsubscribed, as before. Verified beyond the
  stack by a main-vs-branch click walk over all 95 pages; the smoke
  suite alone passed while every HTML button was disabled in an
  intermediate version, since nothing in it clicks through an app.
- **checkout's wizard is a packaged `stepper`.** Its Next and Back were
  two emitters behind two classifiers (`onwardFrom`, `previousOf`)
  folded by two handlers (`stepOnward`, `stepBack`) — four functions
  stating only the order of the steps. They are one selector the demo
  ships beside its views, `Checkout.StepperMDC2`/`StepperMDC3`:
  `stepper @"step" (choice @"Cart" <+> choice @"Shipping" <+> choice
  @"Payment")`, the step order the option list's, built like
  `segmentedButton` (`selectedAt @l` over `selectorWiring`) with a
  Back · dots · Next face. None of the five catalogues has a stepper,
  so it stays app-packaged, not vocabulary. The step cases are their
  copy (`"Cart"`, `"Shipping"`, `"Payment"`); writing.md, vocabulary.md
  and the walkthrough take their two-folds and classifier examples from
  stopwatch and quiz instead.
- **`type` synonyms are allowed in view model modules.** The ban on them
  (filed under "no nominal types") cost every app with a shared row its
  repetition — flight-booker spelled its model row nine times — and
  bought nothing: a synonym is a name for a structure, not a new type,
  so the row stays structural and the view still writes none. `data`
  and `newtype` stay out. scripts/check-determined.mjs now lets the
  compiler decide a closed hint against a signature written through
  the module's synonyms (the verbatim compare stays for the rest, and
  `--write` leaves such a signature alone); scripts/holes.mjs stubs
  values only. flight-booker names `Trip`, `Itinerary`, `Problem`,
  `Date`; writing.md *Types and values*, the walkthrough and the
  `#code-style` note say so.
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
