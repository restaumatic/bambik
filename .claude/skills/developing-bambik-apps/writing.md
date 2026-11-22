# Writing a bambik application

The rules for the two app modules the scaffold ships: the view module
(`src/<Suffix>.purs`, module `<App>.<Suffix>`, named by its design
system) and the view model module beside it (`src/<App>.purs`, module
`<App>`: the app itself). [Code style](#code-style) is the strict
contract and [Formatting](#formatting) its layout; [Stages](#stages) is
the reference from the line you need to the word and the demo that
shows it; [Writing order](#writing-order) is how the two modules get
written. Other files of this skill point here and state no rules.

This file documents no component: what a word does, its signature and
its options are in the library's module headers
([Looking things up](#looking-things-up)). Words appear here only as
examples, each from a demo under `.spago/bambik/<tag>/demo/7guis/`,
`demo/nguis/` or `demo/diner/`. A demo's view module is named by its
design system (`MDC2.purs`, …, `HTML.purs`; modules `Counter.MDC2`, …,
`Counter.HTML`) and its twins share one view model module, the app
itself (`Counter.purs`, module `Counter`), all in one directory, so read
whichever twin matches your design system.

## Terms

- **record** `{ … }` (×) — knowledge: everything at once. **variant**
  `[ … ]` (+) — an event: one case at a time.
- **shape** — what a component takes in and gives out, record or variant
  on each side. There are exactly four, every component has exactly one,
  and each is one arrow of the app's cycle:

  | Shape | Kind | Arrow | Example |
  | --- | --- | --- | --- |
  | ×→× | **editor**, **display**, **stage** | **model** → **view model**: the state as the screen shows it, edits carried back | `filledTextField @"Email"`, `headline4 $ text countLine` |
  | ×→+ | **emitter** | **view model** → **view event**: the state shown, an occurrence emitted | `button @"Count"` |
  | +→+ | **handler** | **view event** → **model event**: what happened turned into what it means | `action @"Charge card" chargeFlaky`, `confirmed @"Book" $ …` |
  | +→× | **status**, **fold** | **model event** → **model**: the event folded into the next state | `snackbar @"Card charged" captured`, `fold @"Count" increment` |

  The row names are positions, not types: the model and the view model
  are one row wherever nothing narrows or derives.
- **stage** — one line of the pipeline: its output is the next line's
  input. Every line passes the row it is fed.
- **merge** — several components over one shared value, written as a
  qualified `do` block ([The pipeline](#the-pipeline)).
- **knot** — the app's one loop, `# looped`, closed with its **seed**,
  the model's value at start (`# looped # with freshCount`); the seed's
  signature is the model row.
- **pane** — a component that exists only while the model is in one
  case (`# provided @"bookable" bookingState`).
- **ocular** — a wrapper that shows content and changes no data: an
  element, a surface, typography (`card`, `headline4`, `div`).
- **copy function** — a pure function in the view model module from the
  row to the words on screen (`countLine`).
- **view model module** — the module beside the view holding the seed
  and every function the view calls, for all four arrows; the view
  determines its signatures up to their tails
  ([Writing order](#writing-order)).

## The pipeline

The app is one pipeline composed with `Semigroupoid.do`
(`import QualifiedDo.Semigroupoid as Semigroupoid`): each line's output
is the next line's input, so **code order is screen order and data
order**. Every line is a stage and passes the row it is fed, so the next
line is fed what the line above shows. Four more qualified `do` blocks
put several components over one value, one per shape (`import
Data.Profunctor.Row.RecordToRecord as RecordToRecord` and siblings;
none is a monad's `do`):

- `RecordToRecord.do` (×→×) — **sources**, each owning the one field it
  produces: facts the platform knows and the app cannot seed
  (`geolocated @"position"`, `online @"connection"`, `colorScheme
  @"scheme"`, `filePicked @"Photo"`). The current time is never one: it
  is no field of the model, a line reads it (`# timestamped @"time"
  clockPeriod`) and an event carries it (`# timestamping @"time"`). A
  source reads nothing, so it is typed at `{}` and a block of sources
  opens the flow; below a line that releases a field it is a compile
  error, and there a source is followed by `# supplied`, which inserts
  its field into the row (incident-report). The rule: **merge what
  writes without reading, sequence everything else** — editors and
  displays read the row, so they are lines, never operands.
- `RecordToVariant.do` (×→+) — a row of emitters over the model, each
  owning the case it emits. The block lists the events that can occur,
  and the fold block after it names exactly the same cases: a case
  written twice, a fold for a case nothing emits and an emitted case
  with no fold are compile errors.
- `VariantToVariant.do` (+→+) — one leaf per case, in the order shown.
  Its one use in an app is a selector's options (`select @"Flight type"
  $ VariantToVariant.do` over `option @"one-way flight"`, `option
  @"return flight"`). Events need no such block.
- `VariantToRecord.do` (+→×) — one fold per case.

**The one runtime rule.** A record merge — and everything built on one —
shows nothing until every field it waits for has a value, then updates
on every change. A pane that stays blank is waiting; the seed is what
gives it a value at start ([When it does not
propagate](#when-it-does-not-propagate)).

## Components

**A component states its label once, as its type argument, and the
label is the copy it draws.** `filledTextField @"First name"` captions
itself "First name" and edits the field `"First name"`; `button
@"Submit order"` draws "Submit order" and emits the case `"Submit
order"`; `snackbar @"Card charged" captured` shows "Card charged" and
nothing more. The model's rows carry the same labels, quoted where they
are human copy (`{ "First name" :: String }`). A quoted label cannot be
a record pun, so bind explicitly (`createPerson { "Name": name, people }
= …`); `r."Name"`, `_."Name"` and `r { "Name" = … }` work as usual in
the view model module.

**The label is only the name.** A format, an example or a unit goes to
the editor's primed sibling (`filledTextField' @"Start date" { hint:
"DD.MM.YYYY" }`, `sliderLive' @"Amount" { unit: "€" }`), and a
question or a sentence is cut to the name it asks for
(`filledTextField' @"Todo" { hint: "What needs to be done?" }`,
`checkbox @"Accept the terms of service"`). A leaf takes no options
record; the few words with optional presentation have a primed sibling
taking it (`button' @"Place order" { icon: "local_cafe" }`). Its field
names belong to the component — copy a demo's call or read the header;
a guessed field fails as `TypesDoNotUnify` on the options record.

- **Editors** are lines, never merge operands; two controls writing one
  field are two lines (tip-calculator).
- **Selectors** are editors in three words: the plain word when the
  field always holds an option, `…Unpicked` when a choice is owed,
  `…Optional` when it may be left unmade. Options are leaves in a
  `VariantToVariant.do` block, named by the selector's leaf (`option`,
  `radio`, `segment`, `tab`); they never read the model. A fact about an
  option (a room's seats) is model data, never part of its name; options
  the model rules out stay drawn and disabled, given as a function from
  the row (`select'`, in its header).
- **Displays** take a copy function, not a label (`headline4 $ text
  countLine`); so does a number drawn as a bar (`linearProgress
  elapsedFraction`).
- **Emitters** emit their own case; two buttons are two cases, each
  with its own fold, and no emitter takes a caption parameter to stand
  for two.
- **Statuses** are ephemeral and read nothing of the payload. What the
  user must keep is a model field shown by a display, or a message the
  user closes (`acknowledged`); mutually exclusive outcomes are sibling
  cases, each owning its teller; a run in progress is shown by the
  action's indicator; a message that stays while a state holds is a
  display under a pane.
- **Chrome** adds nothing to the model. A card around editors is the
  sub-record it edits, `group @"Customer" $ …`, the label its field and
  heading; a plain `card` holds content that edits nothing. The app
  takes no surface: the entry is `body $ …`, never `body $ card $ …`.

**A component's type is its shape, and so is every component the app
packages itself** (checkout's `stepper @"step"`). A signature with a
bare `String`, `Maybe a` or `a` on either side is a smell: that value
belongs in a field of the row, read by a business function.

## Stages

An editor, a display or chrome is a stage as it stands. Everything else
takes a trailing word that says what it is for. What each word does is
its header ([Looking things up](#looking-things-up)); this table is the
index from the line you need to the word and a demo that writes it.

| The line | Write | Demo |
| --- | --- | --- |
| a display or chrome, always there | `headlineSmall $ text orderLine` | order-form |
| an element whose whole face is its decorators | `circle >>> attrAt @"x" "cx" >>> attrWith "fill" circleFill $ identity` | circle-drawer, color-mixer |
| a display shown in one case | `text ticketLine # inCaseAt @"serving" @"display"` | ticket-dispenser |
| an editor that exists in one case | `filledTextField @"Table" # inCaseAt @"Dine in" @"Mode"` | order-form |
| a scrolling list that shows the row just added or changed | `( orderRow # gatheredOf @"dish" tableLines ) # scrolling # revealing` | diner |
| a button that exists in one case | `button' @"Start" { icon: "play_arrow" } # providedAt @"halted" @"phase"`; in a derived state, `# provided @"bookable" bookingState` | stopwatch; flight-booker |
| a button drawn but not pressable while the row says so | `iconButton' @"Dodano porcję" "add" { disabled: isLineSoldOut }` | diner |
| a list of displays rendered from the row | `ul $ ( li $ text lapLine ) # actedOf @"number" lapRows` | stopwatch |
| a list of editors over an array the model stores | `( … ) # actedAt @"id" @"Setlist"` | reorder |
| a list whose elements emit | `tile @"Key entered" @"key" ( … ) # gatheredOf @"key" (const keyPad)`; over a stored array `# gatheredAt @"key" @"cells"` | calculator, tic-tac-toe; cells |
| a filter, a sort or a pick of a stored array, its rows edited in place | `( … iconToggle @"Favorite" … ) # actedWithin @"title" @"movies" visibleMovies`; the picked row's form, `( … ) # actedWithin @"id" @"contacts" pickedContact` | movie-browser, todo-list; contacts |
| a projection that reshapes its rows, its elements edited | `editsOf @l @k proj editor`, folded by `foldInto @l f` | no demo; `PUI` |
| one entity at a time, each shown by its key | `( listItem $ text flightLine ) # dispatchedOf @"code" arrival`; the array of them `# accumulatedOf @"team" goal` | departures, scoreboard |
| a fact the platform knows, owning its field | `geolocated @"position"`, `online @"connection"`, `filePicked @"Photo"`, in `RecordToRecord.do` | here-and-now |
| a line that shows the time, or time passed: the time is not a model field | `( titleMedium $ text clockLine ) # timestamped @"time" clockPeriod`, `clockLine` typed at the line's row with `time` added; on a row of a list, the element's row | punch-clock, incident-report, here-and-now; diner |
| an event stamped with the time it happened | `button @"Punch in" # timestamping @"time"`, folded by `foldInto @"Punch in" punchIn`, the time a field of the event; a beat stamped is `ticks @"Clock ticked" clockPeriod # timestamping @"time"` | punch-clock, incident-report, departures; diner |
| content that waits for the user to confirm | `confirmed @"Refund a customer" $ …`, between the emitters and the folds, its fold among the folds; `confirmed' @l { cancel: "…" }` to caption its Cancel | cashbox, inbox, flight-booker; diner |
| a message the user must close | `( modal @"Total missing" $ … ) # acknowledged` | weather, order-form, incident-report |
| a notice, then the flow goes on | `( modal @"Weather warning" $ … text warningLine ) # continued asForecast`, `asForecast :: Report -> [ "Forecast fetched" :: Report ]` | weather |
| the event path, one line of the body | `( Semigroupoid.do { button @"Count"; fold @"Count" increment } ) # stepped`; several emitters and folds are a `RecordToVariant.do` and a `VariantToRecord.do` inside it, the dialogs between | counter; crud, cashbox |
| an event folded into the model it lands on | `fold @"Add" addTodo`, `addTodo :: Todos -> Todos` | todo-list, counter |
| an event whose payload the fold needs | `foldInto @"Person picked" pick`, `pick :: { event :: Int, model :: Catalogue } -> Catalogue` | crud, inbox, cells |
| a constant replacing the model whole | `reset @"New game" openingPosition` | tic-tac-toe |
| an event line that exists in one case | `( … ) # stepped # inCaseAt @"watching" @"phase"` | ticker |
| an emitter inside a list element, its event carrying that element's row | `( listOfAt' @"Pozycja wydana" @"dish" @"lines" { selected: isPlated } ticketItem ) # joined @"Pozycja wydana"` inside `# gatheredOf @"number" ticketsPage`, folded by `foldInto @"Pozycja wydana" plateDish` | diner |
| a periodic occurrence | `ticks @"Clock ticked" tickPeriod` | stopwatch, timer |
| an invariant between edited fields | `filledTextField @"Celsius" # settled fromCelsius` | temperature-converter |
| an effect on a button's case, in the fold block | `( Semigroupoid.do { indeterminateLinearProgress # action @"Place order" placeOrder; snackbar @"Order placed" (const usualOrder) } )`, the pair owning the case | espresso-bar, order-form |
| an effect on the line after its button | `indeterminateCircularProgress # action @"Charge card" chargeFlaky`, then `snackbar @"Card charged" captured` | payment, ticker |
| an effect whose outcome is the model | `… # action @"Shuffle" shuffleSetlist`, then `capture @"Setlist shuffled"` | reorder, password-generator |
| an effect whose outcome is folded into the model it lands on | `… # action @"Forecast requested" fetchReport`, then `foldInto @"Forecast fetched" rememberReport` | weather, order-form |
| an effect whose outcome is told and keeps the model | `… # action @"Receipt" printReceipt`, then `snackbar @"Receipt printed" kept`; two outcomes, each told on its line (`( modal @"Total missing" $ … ) # acknowledged`, then `snackbar @"Order submitted" kept`) | order-form |
| the load before the knot | `( Semigroupoid.do { indeterminateLinearProgress # load loadOrder; snackbar @"Order loaded" captured } ) # landed` | order-form |
| nothing of the knot on screen until the load lands | `( … ) # looped # awaited` | diner |

Facts the table cannot hold:

- A pane's content, a list's element and `confirmed`'s content release
  the row they are fed. An editor inside a pane edits the model; an
  editor inside a list of displays (`actedOf`) or inside a `tile` is a
  mistake the compiler cannot see — its edits go nowhere. An editor over
  a selection of a stored array is `actedWithin`'s element; one whose
  edits must leave a projection that reshapes its rows is `editsOf`'s.
- Nothing is discarded: a chain whose output is not the model
  (departures' `dispatchedOf` stream) ends the block, and what feeds it
  is a source, not a loop.
- `settled f` runs on every change, not only on the edit it sits on, so
  `f` states something true of every model value, never a reaction to
  an edit: "an estimate belongs to the address it was made for"
  (order-form's `staleDistanceForgotten`), not "forget the estimate when
  the address is edited".
- An action's `Aff` names its outcomes as cases; an `Aff` that throws
  emits nothing and warns in the console, so a failure the user must
  see is an outcome case.
- How sure the business must be that the user read something is a
  business decision: a readout, then a toast, then a banner, then a
  dialog the user must answer. Escalate by choosing the stronger
  component; where outcomes differ in weight, route them apart (cashbox
  confirms outgoing money and posts incoming money straight to the
  balance).

## App shape

An app is one loop through the four shapes, tied once by `# looped #
with seed`:

```purescript
Semigroupoid.do
  headline4 $ text countLine                  -- ×→× a display: shows the row and passes it on
  filledTextField @"Name"                     -- ×→× an editor: edits its field and passes the row on
  ( Semigroupoid.do                           -- the event path, one line of the body
    RecordToVariant.do                        -- ×→+ every emitter and pane
      button @"Create"
      listOf' @"Person picked" …
    VariantToRecord.do                        -- +→× the folds, one per case
      fold @"Create" createPerson             --   the model the event lands on
      foldInto @"Person picked" pick          --   an event whose payload the fold needs
      ( Semigroupoid.do                       --   an effect on a case, folded by its outcome's status
        indeterminateLinearProgress # action @"Place order" placeOrder
        snackbar @"Order placed" (const usualOrder) ) ) # stepped
# looped # with seed
```

- The event path — emitters, dialogs, folds — is one line of the body,
  closed by `# stepped`; an app has **one knot**, and no form keeps a
  `# looped` of its own. A body may hold several event lines, each
  where its buttons are, and displays or editors below them.
- A fold is handed the model as it is when its event lands, not the row
  its button showed; so a fold sets only what its event brought, and an
  edit made while an effect ran is kept.
- `# looped # with seed` closes the knot; `# with seed` closes a flow
  with no loop of its own (potluck). A forgotten seed is a compile error
  at `body` naming the missing fields. A load before the knot is a path
  of its own, closed by `# landed`; its outcome is the model the knot
  loops, so that knot takes no seed (order-form).
- **A form is the exception to the loop.** When nothing in the logic
  writes into a field — no load, reset, fold or normalizer touches it,
  and every editor exists unconditionally — the form needs no knot and
  no seed: each editor is followed by `# introduced @l init`, its label
  written again, `init` a function of the row the editors above it have
  built (signup-form). Displays, panes and the action follow below; edits
  flow down the page only. A field the logic writes, or one under a
  pane, is model state in a `looped` row.

| Shape | Demos |
| --- | --- |
| the smallest loop: display, button, fold | counter |
| a load before the knot, a form, events, actions, statuses | order-form (all four shapes) |
| a loop plus fixed payloads | cashbox, inbox, tic-tac-toe, shopping-cart |
| a fixed grid or canvas fed as data, updated in place | cells, circle-drawer, tic-tac-toe, calculator |
| collections | todo-list, shopping-cart, reorder, potluck, venue, contacts, diner |
| several event lines, each where its buttons are | todo-list, contacts, diner, ticker |
| panes over one classifier | quiz, tic-tac-toe, potluck |
| effects and time | password-generator, stopwatch, timer, weather |
| structure that varies with the data | markdown-previewer |
| no design system at all | restaurant-menu, hello-world |
| a knot fed from upstream, no seed of its own | order-form |
| an action that retries until it succeeds | payment (`chargeFlaky`) |
| a running maximum, a wizard's step, a counter: model fields | auction (`settled raiseTop`), checkout (`stepper @"step"`), ticket-dispenser |
| a reusable sub-form over a sub-record; an event intercepted by a dialog | parcel (`group @"Address" addressForm`), cashbox |
| keyed event streams | departures (`dispatchedOf`), scoreboard (`accumulatedOf`) |

## Conditional visibility

- A component that exists only sometimes is a **pane over a case**,
  never a predicate in the view and never a `Maybe`: `inCase` for
  displays and editors, `provided` for an emitter. A variant the model
  stores is named by case and field (`# providedAt @"halted" @"phase"`,
  `# inCaseAt @"return flight" @"Flight type"`); a derived state has one
  classifier naming every case, each pane naming its own (`# provided
  @"asking" quizPhase`), so two panes over one classifier are never both
  on screen. A case carries a payload only where the business needs
  one; what a pane shows, its read functions read from the model.
- A `Maybe` is a two-case state with unnamed cases: name them (`[
  estimated :: { km, to }, unknown :: {} ]`). `Maybe` stays below the
  UI, converted by a classifier.
- An editor that exists in one mode is `# inCase`, not a pane whose
  edits you fold back by hand. A choice whose cases each carry their
  own fields is a selector field and those fields side by side in the
  model, each field's editor in its case (order-form's `tabBar @"Mode"`,
  then `# inCaseAt @"Dine in" @"Mode"` on the table); what was typed in
  one case is still there after switching back.
- `clWhen` toggles a class for styling, not existence.

## Modals

A dialog is a stage on the event path between the emitters and the
folds, naming the one case it intercepts; every other case passes
through untouched, and a modal is never a model field. The three words
are in the [Stages](#stages) table: `confirmed @l` re-emits its case
with the content's row, folded like any event, its Cancel consuming it;
`( modal @l $ … ) # acknowledged` ends the case there whichever way the
dialog closes; `( modal @l $ … ) # continued exit` lets it go on as the
case `exit` names, and nothing goes on if closed another way. The bare
`dialog` ocular remains for a dialog built by hand: it opens when fed
and closes when its content emits. A drawer feeds its navigation and
its content the same row; the navigation's pick is folded on an event
line of its own (photo-gallery).

## Collections

- **Keyed and kept.** A collection renders one element per row,
  identified by a key field of the row, so rows need an id field. An
  element whose key survives a change is updated in place and keeps its
  focus and local state; a reordered list moves elements with their
  keys. An array the model stores is named, not projected (`gatheredAt
  @"key" @"cells"`, `actedAt @"id" @"Setlist"`, `listOfAt @l @k
  @"messages"`); a fixed catalogue is given through the projection
  argument (`# gatheredOf @"key" (const keyPad)`).
- **Fixed structure** (grids, canvases) is fed as data through
  `actedOf`, each element's attributes computed with `attrWith`.
  **Structure that varies with the data** is `dynamic`, which rebuilds
  per value, with `each` for a constant list built once
  (markdown-previewer).
- **Editing in place and joint choice** are one word, `acted @"id"`:
  the whole array comes back once every element has answered — at once
  for editors, after every choice for selectors still unpicked. An
  element cannot change its own key; add, remove and reorder are
  sibling stages over the enclosing model.
- **A selection edited in place** is `actedWithin`: the rows a view
  model function selects from a stored array — a filter, a sort, the
  one picked row — are the elements, and every edit is written back
  where its key is stored. The function chooses and orders stored rows;
  it cannot reshape one.
- An element whose content reads several things names that reading
  once, in a face function typed at the element row (`attrWith "style"
  cellFace`, tic-tac-toe).

## View module and view model module

- **The view module** (`<Suffix>.purs`, module `<App>.<Suffix>`) exports
  the one entry function and keeps UI functions that span several lines
  (a `dynamic`/`each` builder, a reusable sub-form). It imports the
  design system, the library's words and the view model module. A view
  too long to read as one declaration splits into submodules
  `<App>.<Suffix>.<Part>` in `<Suffix>/<Part>.purs` (diner's
  `Diner.View.Hall`), each signed over the view model's synonyms (`hall
  :: PUI Web Diner Diner`). Components the app draws the same way in
  several places, reading nothing of the model, are the app's own design
  system, `<App>.Design` in `Design.purs`: phrases over the vocabulary's
  words, never a leaf built from a bare element, importing no view model.
- **The view model module** (`<App>.purs`, module `<App>`) exports
  exactly the names the view imports — seed, periods, fixed payloads,
  copy functions, parsers, classifiers, handlers, `Aff` actions — and
  nothing a view line does not type. It imports only the domain
  (`Prelude`, plain data modules, `Effect`, `Aff`, `Data.Variant.Case`),
  never `PUI`, `PUI.Web.*`, a design-system module or the merges. Its
  signatures are what the view reports ([Writing order](#writing-order)).
  A stand-in server is a module of its own beside it, imported like any
  domain module; its module-level state is the one fake in the app,
  named as such.
- The view hands the view model module only arguments called on data
  and never runs an effect at the entry.
- An action owns its button's case; its outcome cases are its `Aff`'s
  (`submitOrder :: Order -> Aff [ "Total missing" :: {}, "Order
  submitted" :: {} ]`), closed by the statuses or folds after it. It is
  opened by its progress indicator, which owns the run's `started` and
  `ended` and nothing else; it stands after the emitters, on the line
  after its button or in the fold block folded by its own statuses. No
  word passes an event around a stage.
- Design-system twins are two view modules over one view model module,
  so anything that would differ between twins is view by definition. An
  app with no business functions (hello-world) is a single view module.

## Code style

The contract for application code. Each rule is strict: code that
breaks one is wrong even if it compiles and runs.

**The anchor invariant.** Every view line names exactly one anchor, in
the anchor's own position, and the anchor says what the line is: a
**field** (the type argument of an editor, selector or group), a
**case** (the type argument of an emitter, pane or status), a **copy
function** (the positional argument of a display), or **nothing**
(chrome; a static's text is a value, not an anchor). So every leaf reads
as a noun phrase — word, anchor, positional arguments:

```purescript
snackbar  @"Card charged" captured             -- the "Card charged" snackbar, folding captured
ticks     @"Clock ticked" tickPeriod           -- the "Clock ticked" source, beating every tickPeriod
button'   @"Sign up" { icon: "person_add" }    -- the record: optional presentation
```

- **A field is named, never picked.** A line that reads a stored field
  names it by its label in the word's own slot (`textAt @"title"`,
  `attrAt @"x" "cx"`, `doubleClickTile @"Cell double-clicked" @"key"`,
  `# providedAt @"halted" @"phase"`, `# gatheredAt @"key" @"cells"`);
  the function form of each `…At` word is for what is derived, named
  in the view model module. No view contains `_.`.
- **A view writes no row.** The model row, a classifier's cases, a
  projection's element row, a payload and an action's outcomes are the
  view model's signatures to state.
- **A record never holds an anchor or a required value**: only optional
  presentation on a primed word, or two same-typed values a positional
  pair could swap (`drawer { title: "Darkroom", subtitle: "…" }`). Two
  value types from the library appear in rows like `Number` does: the
  bounded quantity `{ current, min, max, step }` and the duration `{ ms
  :: Number }`.

### Layout

- **No comments in view code; business code is commented.** Each type
  and function of the view model module carries a `-- |` comment stating
  the rule it implements in the domain's terms, never the screen that
  shows it.
- **The architecture reads off the pipeline**: a load, if any, before the
  knot; inside it, lines in screen order — editors, displays, and each
  event line where its buttons are, its events (×→+), then its dialogs
  and actions (+→+), then its folds and statuses (+→×); the knot closing
  them. No registries, config objects or reflective assembly.
- **A line leads with what is seen, via `$`, and trails with data
  concerns, via `#`**; a decorator rides its element. How lines break,
  indent and import is [Formatting](#formatting).

### Types and values

- **The model is stated once, in the view model module.** The seed's
  signature is the model row; every other row is the signature of the
  function that brings it — a classifier's cases, an action's outcomes,
  a projection's element row. What no signature states is not in the
  model: state only the business needs is derived (inbox computes the
  next id from `messages`) or stored like any other field
  (circle-drawer's undo stacks are in its seed).
- **No nominal types in UI.** A view-model row holds records, variants,
  `String`/`Number`/`Int` and `Array` — nothing else; `{}` for an empty
  payload, never `Unit`. A recursive type (cells' `Expr`) or an
  ecosystem API (`Aff`, `Either`) lives below the UI, entering only
  through business functions.
- **Name a shared row with a `type` synonym**, in business language
  (`Trip`, never `Model` or `State`); export a synonym an exported
  signature uses.
- **No `Maybe` in the model**: name the two cases (`selected :: [ picked
  :: { index :: Int }, none :: {} ]`).
- **No `Boolean` unless a Boolean editor edits it** (`toggleSwitch`,
  `iconToggle`). A flag nobody edits is a phase with two cases (`phase
  :: [ halted :: {}, timing :: {} ]`); a styling test is a named
  predicate (`clWhen isCompleted "todo-done"`).
- **Names say what, in business language.** Role names sit on values
  and functions, never on types (`with plannedTrip`); no lifecycle words
  (`initial`, `default`, `seed`); the entry function is named after the
  app, never `main`.
- **Business values are model data, not UI literals.** A value the
  business owns rides the row (a slider's bounds come from the seed);
  what stays constant — a tick period, a fixed payload, the seed — is a
  named value in the view model module. UI code keeps only presentation:
  labels, icons, styles, structure, layout numbers (`{ columns: 80,
  rows: 3 }`).
- **Copy is a function, not a field.** A display's text comes from a
  copy function in the view model module, unit-testable (`countLine {
  counted: 3 } == "3"`). A whole line is one function, glue included —
  never several leaves with `staticText` between them, never a formatter
  in the view. A field shown as stored names the field (`textAt
  @"title"`). A model field exists because the state needs it, never
  because a display wanted a `String`.
- **Fixed copy is static or constant.** A **static** is on screen
  regardless of data — a heading, a note, a caption — and is a value,
  never a label: `staticText "Hours"`, written first over what it
  introduces (`headline4 $ staticText "Create account"`; inside one
  element, `label $ Semigroupoid.do { staticText "°C "; input @"Celsius"
  "text" }`). A separator is written the same way and shows itself only
  between things (`divider $ slider @"Attendees" # inCaseAt @"chosen"
  @"Room"`). A static inside a pane is still a static (`staticText
  "Error" # inCaseAt @"faulty" @"condition"`) — a copy function that
  ignores its row is a static in the wrong module. A **constant** shows
  only through data — a sentence's glue, a business value in a line —
  and lives in the copy function that reads it. Text that *is* data (a
  parsed markdown run) is `staticText` too.
- **A label is read back, never restated.** A line that shows a case
  label reads it with `caseText` from `Data.Variant.Case`, adapting at
  most its letter case (`"Paying by " <> toLower (caseText
  r."Method")`); grammar a sentence needs lives in its copy function,
  never in the label. A `match` that only echoes its labels is the label
  restated; a map that does real work stays a named copy function. An
  affix the option shows is part of its copy (`radio @"15 min"`).
- **Events carry data, never UI copy** — a reason is a case of its own.

### Business functions

- **The signature is the hint with its tails filled**
  ([Writing order](#writing-order)): the row the stage is fed, closed,
  never narrowed to the fields the body reads, never opened with a
  tail, never coerced at the call site. A name two lines call at
  different rows is the one case for a `forall`: generalize to the
  fields both rows share. Private helpers no view line types are free
  to be general (flight-booker's `dateKey`).
- **Update the row you are given**, never build a literal (`increment m
  = m { counted = m.counted + 1 }`). A preset is a field update, even
  one that reads nothing (`beginTiming sw = sw { phase = .timing {} }`);
  a constant replaces the model only as a whole, by `reset`.
- **One record per business function.** Records that travel together
  are one row. A fold handler is no exception: `fold @l f` hands `f` the
  model (`addTodo :: Todos -> Todos`); `foldInto @l f` hands it one
  record, the payload under `event` and the model under `model` (`pick
  :: { event :: Int, model :: Catalogue } -> Catalogue`); a fixed
  payload is baked into its handler; an effect returns the model under
  its outcome case (`chargeFlaky :: Order -> Aff [ "Card charged" ::
  Order ]`), folded `captured`, or `capture @l` where nothing needs
  telling; an outcome that changes nothing folds `kept`.
- **A status only after an action.** A command straight from an emitter
  folds silently: the screen already shows its result and its case is
  the button's caption. What a status would compute from its payload is
  not copy — a value the user must keep is a model field or what an
  `acknowledged` dialog shows; a reason is a sibling case.
- **A handler carries no field it does not touch.** Group buttons into
  event lines by the fields their handlers touch (circle-drawer keeps
  undo and redo apart from the canvas click). An identity handler means
  the event was never model data: show it with a display instead.
- **Lossy conversions live in the model**: a normalization that loses
  information is `# settled` on the stage (crud's `# settled
  filterChanged`), never hidden inside the component.
- **No dispatcher.** A function that only `match`es event cases has no
  place: each case is its own fold line, its handler a named business
  function.

### Wiring

- **Speak the vocabulary; never import the ecosystem's
  `Data.Profunctor`.** Every reshaping an app needs has a home in a
  word's own argument (`actedOf @l rowsOf`, `settled normalize`, with
  `identity` meaning "the value as it is"). A shape none fits is a gap
  to report, not a reason to reach below the vocabulary.
- **State lives in the model.** No FFI stashes, module-level mutable
  references, DOM reads, window globals or hidden loop channels: a
  running maximum, a wizard's step, a counter are model fields.
- **Lean on the design system; write no custom layout.** A styled flex,
  border or margin wrapper is a smell; an unstyled `div` keeping a
  pane's parts together is fine. Custom style is for data-driven
  graphics only. A surface is stated once (`elevation* $ card $` stacks
  two shadows).

## Writing order

**The view determines the view model up to its tails.** Every view
line names what it needs and the labels it binds, so a view written
with a typed hole for every value it would import compiles to a list of
those values with their types: which share a row, what each returns,
every field and case the lines name — and, as an unknown, the rest of
each row, the **tail**. The tails are the model: many models fit one
view, and the view model module is where one is chosen. Write the view
first and the view model module to that list.

With the watch build running ([building.md](building.md)):

1. **Write the view**, each line naming its anchors. Labels need no
   hole; every other value — a copy function, a handler, a classifier,
   an action, the seed — starts as a typed hole, any name after `?`
   (`text ?countLine`; `fold @"Count" ?increment`; `# looped # with
   ?freshCount`). A hole fails the build by design.
2. **Read the holes.** The compiler reports one declaration's holes per
   build, each at its full inferred type, and a last message lists them
   all with one numbering of the unknowns across them. Counter's three:

   ```
   countLine   :: Record t0 -> String
   increment   :: Record t0 -> Record t0
   freshCount  :: Record t0
   ```

   The view fixed that the three share one row (`t0` everywhere), that
   the copy function returns a `String` and the fold the row; it named
   no field, so the whole row is the tail. A label on a line puts its
   field in the row: temperature-converter's two editors make every
   hint `{ "Celsius" :: String, "Fahrenheit" :: String | t0 }`. Inbox's,
   abridged:

   ```
   openMessage  :: { event :: t0, model :: { messages :: Array { id :: t0 | t1 } | t2 } } -> { … }
   inboxState   :: { messages :: … | t2 } -> [ empty :: t3 | t4 ]
   ```

   — the picked key is the message id's type (`t0` twice), whatever
   that type is; `inboxState` returns a variant with an `empty` case
   because `inCase @"empty" inboxState` says so, and stopwatch's
   `providedAt @"halted" @"phase"` puts `phase :: [ halted :: t2 | t3 ]`
   in its model the same way.
3. **Write the signatures down: the hint with its tails filled.** Fill
   each unknown once, and the same everywhere it appears: counter's `t0`
   is `counted :: Int`, so the model is `{ counted :: Int }` — named
   once, `type Count = { counted :: Int }` — and `countLine :: Count ->
   String`, `increment :: Count -> Count`, `freshCount :: Count`. Close
   every row: a filled tail adds the fields and cases only business
   functions reach, and the result is the row the stage is fed. Do not
   narrow it to the fields the body reads and do not leave a tail open.
   A name reported at two different rows is the one case for a
   `forall`. A row named through a `type` synonym is the same row: the
   check expands it. From then on the compiler checks the view against
   the signatures.
4. **Fill the holes in any order.** An app is one pipeline in one
   declaration, so its holes report together; a view helper's holes
   surface after the entry's. A helper applies one view model function
   per value — composing two (`rgb (mixOf channels)`) hides the type
   between them, so the view model exports the composite
   (color-mixer's `mixedColor`). The seed's value is one business-named
   record (`freshCount`, `mondayMail`).

A design-system twin inverts the loop: its view model module already
exists, so the view is written against known signatures. The app is
finished when no hole is left and it runs in dev mode, checked as
[building.md](building.md) describes: waiting merges are invisible to
the compiler and obvious on screen.

## What the laws guarantee

Every component and every merge obeys a small set of laws. What they
give you while writing:

- **A `do` block is again a component.** A merge of components is a
  component of the same shape, so blocks nest to any depth and each can
  be read as one stage.
- **Order and grouping inside a merge are not observable.** Reordering
  a merge's lines, extracting a sub-block, or adding chrome or a display
  changes screen order and nothing else.
- **Faults are local.** A merge operand gets exactly its part of the
  value and never a sibling's emission, so a misbehaving line is found
  by reading that line. Lines influence each other only through a loop
  you wrote (`looped`).
- **A change arrives whole.** A change to several fields is released
  once, never as a half-updated row. Business functions read consistent
  state.
- **Showing state never fires an event.** Feeding a button, a list or a
  selector the model does not make it emit; only the user does. So the
  loop can feed its emitters freely, and settles instead of spinning.
- **Waiting is safe.** Putting a stage that holds things back —
  `confirmed`, `acted`, a debounce — anywhere only delays what flows; it
  never produces a new or inconsistent value.
- **A blank pane is a missing seed.** A merge that shows nothing is
  waiting for a field nobody has given a value; the watchdog names it
  (below). An event line is never the cause: closed by `# stepped` it
  passes on every row it is fed.
- **An event lands on the model as it is.** A fold is handed the model
  at the moment its event arrives, not the row its button showed, so a
  fold that sets only what its event brought keeps every edit made
  while an effect ran.
- **Design systems are interchangeable.** The laws are about shapes, so
  a twin over another design system behaves the same.

They do not guarantee that your business functions are correct — that
is what the view model module's unit tests are for.

## When it does not compile

Most wiring mistakes surface as `Could not match type` between two rows,
reported where the rows meet rather than where the mistake is: a row
flows down the pipeline until a line cannot take it. Read the two rows
in the message, the one the line wants and the one it got, and look
**upstream** of the reported line for the stage that changed the row.

| The message | Reported at | The mistake | The fix |
| --- | --- | --- | --- |
| the model row against `()` | the whole app expression | the loop has no seed: the entry feeds only `{}` | close the loop with `# with seed` (counter) |
| the model row against `( event :: …, model :: … )` | the event line | a handler and its fold word disagree: `fold @l f` hands `f` the model alone, `foldInto @l f` one record, `{ event, model }` | match the word to the handler: `fold` for `model -> model` (counter's `increment`), `foldInto` for `{ event, model } -> model` (crud's `pick`) |
| `Function …` against `Record` | the line after the event path, or the knot (`looped`) when the path is the body's last line | the event path is not closed: bare folds release a step, `model -> model`, where the next line and the knot take a row | close the path, `( … ) # stepped` (counter) |
| `( "Count" :: … )` against `( "Cout" :: … )` | the fold | one case spelled two ways | spell the case the same on the emitter and its fold |
| `( "Reset" :: … )` against `( … )` | the first line of the emitter block | no fold handles the case | add its fold to the fold block, silent if nothing needs telling (timer's `fold @"Clock ticked" tick`), or remove the emitter |
| `( "Undo" :: … )` against `( … )` | the first line of the fold block | a fold for a case nothing emits | remove the fold, or add the emitter it was written for |
| `Two merge operands own the label "Count"` | the first line of the block | a label written twice in one block: a case two emitters emit, a case two folds handle, a field two sources produce | two buttons are two business actions — give each its own case and its own fold (stopwatch's Start and Stop) |
| `( "E-mail" :: … \| t3 )` against the model row, `while solving Cons "E-mail"` | the editor line | an editor, selector or pane names a field or case the model does not have | spell the label as the model has it, or add the field to the model |
| `( size :: Int … )` against `( … )` | the seed line | a business function names a field the model does not have | copy the signature from the hole its line reports ([Writing order](#writing-order)) |

## When it does not propagate

The compiler proves the wiring, not that data reaches the screen. A
blank pane or a stale readout almost always means a merge is waiting
for a field that has no value. That field belongs to an editor, a
source or the seed — never to a display — so the fix is always a seed
or a missing source. Three aids find it in the browser:

- **The starvation warning**, on by default. A merge still waiting after
  3 s prints one `console.warn` naming the missing fields and the fix,
  and logs the page elements of those fields beside it. Turn it off with
  `window.__bambikNoWarn = true`.
- **The emission trace**: `window.__bambikTrace = true` (or
  `localStorage.setItem("bambik-trace", "true")`) logs every step of
  every update as `console.debug` — enable DevTools' Verbose level. It
  prints the labels your view names.
- **The accessibility tree.** Every labelled component stamps its label
  on its element, so DevTools' accessibility view shows the app in the
  model's own words: groups as sub-records, editors as fields with
  their values, buttons as business cases.

## Looking things up

- **What a word does, its signature and options**: the module headers.
  Browse them all with `npx spago docs --open` in the app, or read the
  source under `.spago/bambik/<tag>/`: `src/PUI.purs` (stages and
  collections), `src/PUI/Web.purs` (text, panes, clicks, attributes),
  `src/PUI/Web/HTML.purs` and `SVG.purs` (elements), and one module per
  design system under `src/PUI/Web/` (`MDC2`, `MDC3`, `Shoelace`,
  `Fluent`, `Bootstrap`) — the same concept keeps the same name across
  them.
- **How it is written in a real app**: the demos under
  `.spago/bambik/<tag>/demo/`.
- **Which word fits a situation**: the [Stages](#stages) table, from
  the line you need to the word and a demo; for a design system's
  editors, emitters, statuses and typography, its module header.

## Formatting

How the code is laid out. These rules are house style — part of the
contract, but about appearance, not meaning.

- **Imports are 100% explicit, `Prelude` included.**
- **A one-line UI function is inlined.** A UI function earns its name
  only by spanning lines.
- **`$` leads, `#` trails.** An element over one leaf applies it with
  `$`, never parentheses (`headline4 $ text countLine`); a chain on a
  wrapped element opens the paren before the wrapper (`( headlineSmall
  $ text wonLine ) # inCase @"won" gameOutcome`). The one exception is
  `static`, whose argument is an ocular: `static (el "input" >>> "type"
  := "checkbox")`. `#` binds tighter than `$`: where a chain must apply
  to a whole wrapped element, open the paren before the wrapper.
- **A decorator rides its element**: composed onto a container with
  `>>>` (`td >>> attrWith "style" cellFace $ …`, `span >>> clWhen
  isCompleted "todo-done" $ textAt @"title"`) or trailing a finished
  leaf with `#` (`… # tooltip "Members get 10% off"`); never leading.
- **Closing parens and `#` chains never start a line.** A trailing
  chain stays at the end of the component's last line, nested closers
  cascading onto it (`fold @"Count" increment ) # stepped`). The
  exception is the knot's closer, `# looped # with seed` / `# with
  seed`, on its own line.
- **A long signature breaks** at 100 columns: the name on its own line,
  `::` and each `->` leading a line beneath it, a record or variant too
  long for its line one field per line under its bracket (cells).
- **Two-space indentation.** A block's lines sit two columns deeper than
  its opener, `( Semigroupoid.do` included; a closer returns to its
  opener's column. No four-space steps, no alignment to a mid-line token.
