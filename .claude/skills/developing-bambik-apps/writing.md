# Writing a bambik application

The rules below govern the two app modules the scaffold ships: the view
module (`src/<Suffix>.purs`, module `<App>.<Suffix>`, named by its design
system) and the view model module beside it (`src/<App>.purs`, module
`<App>`: the app itself), whose signatures the view determines up to
their tails ([Writing order](#writing-order)). [Code style](#code-style) is the strict
contract; the sections before it are the shapes that contract is
written in. Other files of this skill point here and state no rules.

This file never documents a component. What a word does, its signature
and its options are in the library's module headers — see
[Looking things up](#looking-things-up). Words appear here only as
examples, each from a demo you can open under
`.spago/bambik/<tag>/demo/7guis/` or `demo/nguis/`. A view module is
named by its design system (`MDC2.purs`, …, `HTML.purs`, the modules
`Counter.MDC2`, …, `Counter.HTML`), and the twins share one view model
module, the app itself (`Counter.purs`, module `Counter`), all in the
app's one directory (`demo/7guis/counter/`), so read whichever twin
matches your design system.

## Terms

- **record** `{ … }` (written ×) — knowledge: everything at once.
  **variant** `[ … ]` (written +) — an event: one case at a time.
- **shape** — what a component takes in and gives out, record or
  variant on each side. There are exactly four, and every component has
  exactly one:

  | Shape | Kind | Examples |
  | --- | --- | --- |
  | ×→× | **editor**, **display**, **stage** | `filledTextField @"Email"`, `headline4 $ text countLine` |
  | ×→+ | **emitter** | `button @"Count"` |
  | +→× | **status** | `snackbar @"Flight booked" identity` |
  | +→+ | **handler** | `action @"Create" createPerson` |

- **stage** — one step of the pipeline: its output is the next step's
  input.
- **merge** — several components over one shared value, written as a
  qualified `do` block (below).
- **seed** — the model's value at start, on the line that closes the
  loop (`# looped # with freshCount`); its signature is the model row. A pane
  stays blank until the fields it waits for have values; a seed gives
  them one.
- **pane** — a component that exists only while the model is in one
  case (`# inCase @"problem" bookingState`).
- **anchor** — the one model symbol a view line names: a field, a case,
  a copy function, or nothing (see [Code style](#code-style)).
- **view model module** — the module beside the view holding the seed
  and every function the view calls; the view determines its
  signatures up to their tails ([Writing order](#writing-order)).
- **copy function** — a pure function in the view model module from the
  row to the words on screen (`countLine`).
- **chrome** — headings, cards and other parts that show no data.
- **hole** — a stand-in for a value not written yet
  ([Writing order](#writing-order)).

## The pipeline

The app is one pipeline composed with `Semigroupoid.do`
(`import QualifiedDo.Semigroupoid as Semigroupoid`): each line's output
is the next line's input, so code order is screen order *and* data
order. Every line is a **stage**: an editor, a display, a static, a pane,
a list, an event ensemble, a fold. A line that **shows data** — a
display, a static, a pane of displays, a list of displays — changes no
data: it releases the row it was fed, so the next line is fed what it
shows. It is written in its place on the screen and nothing else
(counter's `headline4 $ text countLine` above its `button @"Count"`;
tip-calculator's totals after its last slider; stopwatch's lap list after
the folds). Four more qualified `do` blocks put several components over
one value, one per shape:

- `RecordToRecord.do` (×→×) — components over one record side by
  side, each owning the fields it releases: **sources** — facts the
  platform knows and the app cannot seed, `geolocated @"position"`,
  `online @"connection"`, `clock @"time" clockPeriod`, `colorScheme
  @"scheme"`, the capture `filePicked @"Photo"` — each owning its one
  field, merged into the row the lines below read (here-and-now,
  incident-report). Every editor owns the whole row, so editors never
  merge, and a display owns the row it releases, so two displays
  compose ([Components](#components)).
- `RecordToVariant.do` (×→+) — a row of buttons over the model
  (cashbox, stopwatch).
- `VariantToVariant.do` (+→+) — one handler per event case. No demo
  needs the block: an action handles its button's case inside the fold
  block ([Business functions](#business-functions)).
- `VariantToRecord.do` (+→×) — one fold or status per case (crud's
  folds; flight-booker's two snackbars).

Import the four from `Data.Profunctor.Row.RecordToRecord` and its
siblings (`import Data.Profunctor.Row.RecordToRecord as
RecordToRecord`). None of the five is a monad's `do`.

**The one runtime rule.** A record merge — and everything built on
one — shows nothing until every field it waits for has a value, then
updates on every change. A pane that stays blank is waiting; the seed
is what gives it a value at start, and
[When it does not propagate](#when-it-does-not-propagate) shows how to
find which field is missing.

## Components

**A component states its label once, as its type argument, and the
label is the copy it draws.** `filledTextField @"First name"`
captions itself "First name" and edits the field `"First name"`;
`button @"Submit order"` draws "Submit order" and emits the case
`"Submit order"`. This keeps the code as close to the running UI as
possible: the words on the screen are the words in the code, so a line
is found from the screen and the screen from a line. Labels are human
copy, so they are usually quoted, and the model's rows carry the same
quoted labels (`{ "First name" :: String }`). A quoted label cannot be a record pun,
so bind explicitly:

```purescript
createPerson { "Name": name, "Surname": surname, people } = …
```

In the view model module, field access (`r."Name"`), accessor sections
(`_."Name"`) and update syntax (`r { "Name" = … }`) work as usual; a
view names a field only by its label (see
[The anchor invariant](#code-style)). The label is the name the
screen shows and nothing more, so the developer thinks the word the
user reads: a format, an example or a unit is not the name and goes to
the editor's primed sibling (`filledTextField' @"Start date" { hint:
"DD.MM.YYYY" }`, `sliderLive' @"Amount" { unit: "€" }`), and a
question or a sentence is cut to the name it asks for
(`filledTextField' @"Todo" { hint: "What needs to be done?" }`,
`toggleSwitch @"Teams link"`, `checkbox @"Accept the terms of
service"`, never `"I accept…"` or `"Your name"`). A leaf takes no
options record: the label is its caption. The few words with optional
presentation have a primed sibling taking it (`button' @"Place order" {
icon: "local_cafe" }`, `listOf' … { selected: isSelected } …`, the
hint and the unit).

How each kind takes its business meaning:

- **Editors** edit the field their label names, and each one is a whole
  pipeline stage: fed the whole row, it edits its field and passes the
  rest on. A form is editors written as successive lines, never merge
  operands. Two controls writing one field are two lines in a row
  (tip-calculator's slider and range input). Editors live inside a loop,
  `# looped`, so every editor sees its siblings' latest values; a form
  inside an app's loop of events keeps a `# looped` of its own
  (order-form).
- **Selectors** are editors, and the word depends on what the model
  holds. The plain word when the field always holds an option; the
  `…Unpicked` word when a choice is owed but not yet made; the
  `…Optional` word when the user may leave it unmade. Options are
  `choice @"…"` values in the order written (meeting-booker:
  `dropdownUnpicked @"Room" @"chosen" (choice @"Focus pod" <+> …)`). A
  fact about an option (a room's seats) is model data, never part of
  its name.
- **Displays** take a copy function, not a label:
  `headline4 $ text countLine` (counter). A number shown as a
  bar or gauge takes a function too, and keeps its label only as the
  accessible name (`linearProgress elapsedFraction`, timer).
  See *Copy is a function* in [Types and values](#types-and-values).
- **Emitters** emit their own case. Whatever the business decides about
  it is decided where the case is consumed — the fold's handler, the
  action's outcome row. Two buttons are two business cases, each
  with its own fold (stopwatch's `fold @"Start" beginTiming`,
  `fold @"Stop" haltTiming`); no emitter takes a caption
  parameter to stand for two.
- **Statuses** are labelled with the case they tell, and the label is
  their whole copy: `snackbar @"Flight booked" identity` shows the words
  "Flight booked". A status takes no copy function — it is ephemeral,
  so it names what happened and nothing more; what the user must keep
  is a model field shown by a display. Mutually exclusive outcomes
  are sibling statuses in one `VariantToRecord.do`, each owning its
  case, so an action names its outcomes as the words the user reads
  (`Aff [ "Order submitted" :: {}, "Total missing" :: {} ]`);
  a run in progress is shown by the action's indicator, never by
  a status. A message that stays while a state holds is a display
  under a pane (`( banner $ staticText "Inbox zero!" ) # inCase @"empty"
  inboxState`, inbox), not a status.
- **Chrome** (`card`, `topAppBar`, typography, dialogs) wraps other
  components and adds nothing to the model; code order is screen order.
  A card around editors is a sub-record: write it as the labelled group
  `group @"Customer" $ …` (order-form), whose label is the sub-record's
  field and heading. A plain `card` holds only content that edits
  nothing (order-form's summary). The app itself takes no surface: the
  entry is `body $ …`, never `body $ card $ …`.

**A component's type is its shape, and so is every component the app
packages itself** (order-dashboard's `statTile "Orders placed"
ordersCount`; checkout's `stepper @"step"`, a selector editing the
stored step through Back and Next). A signature with a bare `String`, `Maybe a` or `a` on
either side is a smell: that value belongs in a field of the row, read
by a business function.

A primed word's options are a record whose field names belong to the
component (`button'`'s `icon`, `listOf'`'s `selected`), so copy a
demo's call or read the header instead of guessing — a guessed field
fails as `TypesDoNotUnify` on the options record.

## Stages

An editor, a display or chrome is a stage as it stands. Everything else
takes a trailing word that says what it is for:

| The line | Write | Demo |
| --- | --- | --- |
| a display or chrome, always there | `headlineSmall $ text orderLine` | order-form |
| an element whose whole face is its decorators | `circle >>> attrAt @"x" "cx" >>> attrWith "fill" circleFill $ identity` — the wire, decorated | circle-drawer, color-mixer |
| a display shown in one case | `text ticketLine # inCaseAt @"serving" @"display"` | ticket-dispenser |
| an editor that exists in one case | `filledTextField @"Table" # inCaseAt @"Dine in" @"Mode"` | order-form |
| a button that exists in one case | `button' @"Start" { icon: "play_arrow" } # providedAt @"halted" @"phase"` | stopwatch |
| a list of displays rendered from the row | `ul $ ( li $ text lapLine ) # foreach @"number" lapRows` | stopwatch |
| a list of editors over an array the model stores | `( … ) # actedAt @"id" @"Setlist"` | reorder |
| a list whose elements emit | `tile @"Key entered" @"key" ( … ) # anyOf @"key" (const keyPad)`; over a stored array `# anyOfAt @"key" @"cells"` | calculator, tic-tac-toe; cells |
| a projected list whose elements are edited | `( listItem $ … iconToggle @"Favorite" … ) # anyEdit @"Favorite changed" @"title" visibleMovies # joined @"Favorite changed"` — the element stays an editor, a user's edit leaves as the case, the fold writes it back by key | movie-browser |
| a list element echoing the row it shows | `( listItem $ text flightLine ) # dispatched …` | departures, scoreboard |
| a fact the platform knows, owning its field | `geolocated @"position"`, `clock @"time" clockPeriod`, `filePicked @"Photo"`, merged in `RecordToRecord.do` | here-and-now, incident-report |
| content that waits for the user to confirm | `confirmed @"Refund a customer" "Refund" $ …` — a `+→+` stage between the emitters and the folds: opened by its case, which it tells as its title, the confirm re-emitting the case with the row shown, which its fold then takes (`fold @"Refund a customer" refundStandard`); a cancel consumes it | cashbox, inbox |
| a message the user must close | `acknowledged @"About this dashboard" "Got it" $ …` — the `+→+` stage consuming its case: the row less it goes on to the folds | weather |
| an event folded into the model | `fold @"Add" addTodo`, one per case, merged in `VariantToRecord.do` | todo-list |
| an event carrying something of its own, joined with the model | `listOf @"Todo toggled" … # joined @"Todo toggled"` | todo-list, cells, inbox |
| an action whose outcome is told and keeps the model | `( Semigroupoid.do { button' @"Receipt" { icon: "file" }; indeterminateLinearProgress # action @"Receipt" printReceipt } ) # joined @"Receipt printed"` in the event ensemble, its status folding `kept`; two outcomes `# joined2 @"Order submitted" @"Order rejected: no total"` | order-form |
| a periodic occurrence | `ticks @"Clock ticked" tickPeriod` | stopwatch, timer |
| an invariant between edited fields | `filledTextField @"Celsius" # settled fromCelsius` | temperature-converter |
| an effect on a button's case, folded by its own outcomes | `( Semigroupoid.do { indeterminateLinearProgress # action @"Create" createPerson; VariantToRecord.do { snackbar @"Person created" identity; … } } )`, the pair owning the case in the fold block, the statuses owning the action's outcomes | crud |
| an effect whose outcome is the model | `( Semigroupoid.do { indeterminateCircularProgress # action @"Forecast requested" fetchReport; capture @"Forecast fetched" } )` | weather |
| an effect whose outcomes are told | `indeterminateLinearProgress # action @"Book" submit`, then `snackbar @"Flight booked" identity` and `snackbar @"Booking rejected" identity` | flight-booker |
| the load before the knot | `indeterminateLinearProgress # load loadPeopleCatalogue`, then `capture @"People loaded"` — run by `body`'s one feed of `{}` | crud, order-form |
| a fold told by a status | `snackbar @"Person created" identity`, `snackbar @"Quote fetched" recordQuote`, `snackbar @"Order submitted" kept` — a status takes its fold like `fold @l f`, tells its case by its label, and stands only after an action whose outcome it is | crud, ticker, order-form |

A pane's content, a list's element and `confirmed`'s content release
the row they are fed: a display does so by itself. An editor inside a
pane edits the model (`inCase`); an editor inside a list of displays
(`foreach`) or inside a `tile` is a mistake — its edits go nowhere —
and `npm run check-view-model` rejects the first; an editor whose edits
must leave a projected list is `anyEdit`'s element. Nothing is discarded: a
chain whose output is not the model — departures' `dispatched` stream —
ends the block, and what feeds it is a source (`clock`), not a loop.

A display's policy is a business decision about how sure the business
must be that the user read it: a readout, then a toast, then a banner,
then a dialog the user must answer. Escalate by choosing the stronger
component; where outcomes differ in weight, route them apart (cashbox
sends outgoing money through a confirmation and posts incoming money
straight to the balance).

`settled f` runs on every change, not only on the edit it sits on, so
`f` states something true of every model value, never a reaction to an
edit. Order-form's "an estimate belongs to the address it was made for"
(`staleDistanceForgotten`) is such an invariant: editing the address
drops the estimate as a consequence. "Forget the estimate when the
address is edited" is not.

## App shape

An app is one loop through the four shapes, tied once by `# looped # with seed`:

```purescript
Semigroupoid.do
  headline4 $ text countLine                  -- ×→× a display: shows the row and passes it on
  RecordToVariant.do                          -- ×→+ the event ensemble: every emitter and pane
    button @"Add"
    listOf @"Todo toggled" … # joined @"Todo toggled"  --   an event with a payload of its own, joined with the model
  VariantToRecord.do                          -- +→× the folds, one per case
    fold @"Add" addTodo
    fold @"Todo toggled" toggleTodo
    ( Semigroupoid.do                         --   an effect on a case, folded by its own outcomes
      indeterminateLinearProgress # action @"Create" createPerson
      VariantToRecord.do
        snackbar @"Person created" identity
        snackbar @"Person not created" identity )
# looped # with seed
```

**A form is the exception to the loop.** When nothing in the logic
writes into a field — no load fills it, no reset clears it, no fold or
normalizer touches it, and every editor exists unconditionally — the
form needs no knot and no seed: each editor states its own blank in its
**introducing form**, `{ | b } → { l :: a | b }`, its init a function
of the row the editors above it have built (signup-form's MDC3 view,
`debouncedTextFieldFrom @"Username" blankUsername usernameSettleTime`,
`radioButtonFrom @"Plan" defaultPlan …`, with `defaultPlan :: {
"Username" :: String } -> Plan`). Displays, panes and the action follow
below, reading the row the editors built; edits flow down the page
only. A field the logic writes, or one under a pane (`inCaseAt`), is
model state: the lens editor in a `looped` row.

A fold is one line per event case, `fold @l f` (`fold @"Count"
increment`): the case is its label, `f` of the payload the whole next
model. A fold with something to tell is a **status**, which takes its
fold the same way: `snackbar @l f`. Every `+→×`
line folds; nothing stands in the block releasing nothing.
The status names the case — its label is the event's, human copy like
`"Person created"` — and that label is all it shows: it tells the
**event**, never the fold's result, and reads nothing of the payload. A
status therefore stands after an action whose outcome row names what
happened (crud's action returns the model under `"Person created"`, and
the status's fold is `identity`). A command straight from an
emitter has nothing to tell that the displays do not already show, and a
heartbeat nobody needs told about has nothing at all: both are bare
folds (`fold @"Count" increment`, `fold @"Clock ticked" tick`). A fold is memoryless: an emitter's payload is the row it was fed, an event
with something of its own arrives `# joined @l` with that row as
`{ event, model }`, and an effect returns the model. So every handler has
the model in hand and the loop's memory stays at the emitters and in
the knot. Each fold releases the whole next model, and the `+→×` merge
forwards each release as it comes — its output row is shared, where the
`×→×` merge's operands own their fields. The seed and every later model enter at the top, so everything that
must be on screen at mount — displays, editors, the list a pick comes
from — stands before the ensemble; a stage after the fold is fed only by
events. Editors before the ensemble flow forward only; an editor whose
edit must reach its siblings keeps its own normalizer (`# settled`,
temperature-converter) or stands in a loop of its own: a form of several
editors is `( … ) # looped` inside the app's loop (order-form). An event
whose effect takes time is joined with the model when its outcome lands,
not when it fired: `joined @l` wraps the emitter and its action together
(`( Semigroupoid.do { button …; … # action @"Estimate distance" estimateDistance } )
# joined @"Distance estimated"`), and the fold sets only what the effect
produced on that model (`setDistance`), so an edit made while it ran is
kept (order-form). A drawer feeds its nav and its content the same row;
its nav is the list, `joined`, and its fold, a record stage of its own
(photo-gallery). `# with seed` closes a
flow with no loop of its own. Both close the app to what `body` accepts; a forgotten seed is a
compile error at `body` naming the missing fields.

| Shape | Demos |
| --- | --- |
| the smallest loop: display, button, fold | counter |
| the loop with each stage's shape spelled out in comments | counter (MDC3) |
| a load action feeding a looped form, events, actions, statuses | order-form (all four shapes), crud |
| a loop plus fixed payloads | cashbox, inbox, tic-tac-toe, shopping-cart |
| a fixed grid or canvas fed as data, updated in place | cells, circle-drawer, tic-tac-toe, calculator |
| collections | todo-list, shopping-cart, reorder, potluck |
| panes over one classifier | quiz, flight-booker, potluck |
| effects and time | password-generator, stopwatch, timer, weather |
| structure that varies with the data | markdown-previewer |
| no design system at all | restaurant-menu, hello-world |
| a knot fed from upstream, no seed of its own | order-form and crud (`looped` after the load action, which `body`'s one feed of `{}` runs) |
| an action that retries until it succeeds | payment (`chargeFlaky`) |
| a running maximum, a wizard's step, a counter: model fields | auction (`settled raiseTop`), checkout (`stepper @"step"`, its own selector), ticket-dispenser (`issue`) |
| a reusable sub-form over a sub-record; an event intercepted by a dialog | parcel (`group @"Address" addressForm`), cashbox (`confirmed @"Refund a customer" "Refund" refundStandard $ …`) |
| an effect's outcome folded into the model it lands on | order-form (`… # joined @"Distance estimated"`, `fold @"Distance estimated" setDistance`) |
| keyed event streams | departures (`dispatched`), scoreboard (`accumulated`) |

## Conditional visibility

A component that exists only sometimes is a **pane over a case**, never
a predicate in the view and never a `Maybe`. The argument is a business
function that classifies the model into a variant; the pane exists while
the variant is at the named case, and its content is fed the model, as
every stage is: `inCase` for displays and editors, `provided` for an
emitter.

When the state is stored in the model as a variant, the pane names the
case and then the field, both by label — `providedAt`, `inCaseAt`
(stopwatch's `# providedAt @"halted" @"phase"`, ticket-dispenser's
`text ticketLine # inCaseAt @"serving" @"display"`, flight-booker's
`# inCaseAt @"return" @"Flight type"`) — typed by the model row the seed's
signature states. When it is derived, one classifier derives it, its
signature naming every case — flight-booker's `bookingState`, potluck's
`menuState`, inbox's `messageView`, quiz's `quizPhase` — and each
pane names only its own case (`# provided @"reading" messageView`). Two panes over one classifier
can never both be on screen. A case carries a payload only where the
business needs one; what a pane shows, its read functions read from the
model (inbox's `subjectLine` finds the open message itself).

A `Maybe` a pane depends on is a two-case state with unnamed cases.
Name them: order-form's distance is
`[ estimated :: { km, to }, unknown :: {} ]`. `Maybe` stays below the
UI — a lookup, an `Aff` result — and a classifier converts it (inbox's
`messageView` turns `find`'s `Maybe` into `reading`/`browsing`).

An editor that exists only in one mode is `# inCase`, not a pane whose
edits you fold back by hand. Order-form's fulfillment fields,
flight-booker's return date (`# inCaseAt @"return" @"Flight type"`) and
meeting-booker's attendees slider (`# inCaseAt @"chosen" @"Room"`) are the
examples. A choice whose cases each carry their own fields is a
selector field and those fields side by side in the model, each field's
editor existing in its case (order-form's Fulfillment: `tabBar @"Mode"`,
then `# inCaseAt @"Dine in" @"Mode"` on the table): what was typed in
one case is still there after switching away and back, because it is a
model field. Where the business needs only the chosen case, a view model
function reads it from that record.

`clWhen` toggles a class for styling, not existence.

## Modals

A dialog is a stage on the event path, between the emitters and the
folds, and it names the one case it intercepts: `confirmed @"Delete
message" "Delete" $ bodyMedium $ text deletionLine` (inbox) opens on
that case, shows its payload to the content — a display or editors over
the row the button replayed — and re-emits the case carrying the
content's row when the user confirms, so the fold block folds it like
any event (`fold @"Delete message" deleteOpened`); a cancel consumes it
and no fold runs. `acknowledged @"About this dashboard" "Got it" $ …`
(weather) is the same dialog with a dismiss: the case is consumed either
way, so what follows is typed at the cases that go on. Every other case
passes through both untouched, and a modal is never a model field — no
`confirming` state, no `Delete`/`Keep` buttons. The bare `dialog` ocular
remains for a dialog built by hand: it opens when fed and closes when
its content emits.

A drawer's navigation is the first stage and its content the second, so
the pick reaches the content directly (photo-gallery).

## Collections

- **Keyed and kept.** `foreach @"key" rowsOf` and `listOf @l @k` render
  one element per row, identified by a key field of the row; an array
  the model stores is named instead of projected (`anyOfAt @"key"
  @"cells"`, `actedAt @"id" @"Setlist"`, `listOfAt @l @k @"messages"`). An element
  whose key survives a change is updated in place and keeps its focus
  and local state; a reordered list moves elements with their keys. Rows
  therefore need an id field — an array of bare strings cannot be
  edited in place.
- **Fixed structure** (grids, canvases): feed the structure as data
  through `foreach` and compute each element's attributes with
  `attrWith` (cells, circle-drawer). **Structure that varies with the
  data** (markdown blocks): `dynamic`, which rebuilds per value, with
  `each` for a constant list, built once (markdown-previewer).
- **Editing in place and joint choice** are one word, `acted @"id"`: the
  whole array comes back once every element has answered — at once for
  editors, which answer every feed (reorder), and after every choice for
  selectors still unpicked (potluck). An element cannot change its own
  key. Add, remove and reorder are sibling stages over the enclosing
  model, not part of the element.
- A fixed catalogue is given through the word's own projection
  argument, `# anyOf @"key" (const keyPad)` (calculator).
- An element whose content reads several things names that reading
  once, in a face function, typed at the element row and closed like
  every other function a view line names
  (`attrWith "style" cellFace`, `cellFace :: { key :: String, text ::
  String, status :: [ … ] } -> String`, cells).

## View module and view model module

Every function belongs to one of two classes, in two modules with a
one-way dependency:

- **The view module** (`<Suffix>.purs`, module `<App>.<Suffix>`) exports the one entry function and
  keeps UI-wiring functions that span several lines (a `dynamic`/`each`
  builder, a reusable sub-form like parcel's `addressForm`). It imports
  the design system, the library's words and the view model module.
- **The view model module** (`<App>.purs`, module `<App>`) exports business
  functions and named business values: seed models, tick periods, fixed
  payloads, copy functions, parsers, `Aff` actions — exactly the names
  the view imports, nothing a view line does not type (a helper only
  other business functions call is private). It imports only the
  domain — `Prelude`, plain data modules, `Effect`, `Aff`,
  `Data.Variant.Case` — never `PUI`, `PUI.Web.*`, a design-system module
  or the merges. The one exception is temporary: a stub `hole` while
  the view model is not written ([Writing order](#writing-order)).

**The view determines the view model module up to its tails.** A value
lives there exactly when a view line calls it, and its signature is
what that line demands: compiled with a typed hole for each such value,
the view reports every one at the type it gives it — which values share
a row, what each returns, every field and case a line names — and the
rest of each row as an unknown, the **tail**, which only the model
fills. The module's signatures are that list with the tails filled
([Writing order](#writing-order)). Nothing is designed into the view
model module that no view line asks for, and what the view leaves
unknown is the model's to choose.

The view hands the view model module **only arguments called on
data**: a copy function, a handler, a classifier, an action, the seed,
a period. It never runs one of its effects at the entry and never
applies a component built there. A stand-in server is a module of its
own beside the view model module (crud's `PeopleServer`), as a real
server sits behind its client library: the view model module imports it
like any domain module, the view never does, and its module-level state
is the one fake in the app, named as such.

**An action owns its button's case; its outcomes are its `Aff`'s.**
Its outcome cases are typed by its `Aff` (`createPerson :: model -> Aff
[ "Person created" :: model, "Person not created" :: model ]`, crud),
and the line names only the case it handles: the action is typed at
exactly those outcomes, and what tells them closes them — the statuses
after it, owning their cases, or among emitters the `joined @l` that
joins it (`joined2 @l1 @l2` for two outcomes). With the `Aff` still a
hole, those followers are what type it. It is **opened by
its progress indicator**: the slot owns the run's `started` and `ended`
and nothing else — a status that tells an outcome is never the slot, and
the types forbid it. An action lives in the fold block, **folded by its
own outcome statuses** after it, the pair owning the button's case there
through the action's label:

```purescript
( Semigroupoid.do
  indeterminateLinearProgress # action @"Create" createPerson
  VariantToRecord.do
    snackbar @"Person created" identity
    snackbar @"Person not created" identity )
```

So the action line reads as a noun phrase like every leaf — word, case,
function — and nothing places it: no `atCase` under the button's case,
no widening beside emitters. An outcome that is told but changes nothing
is `kept`: the action is `joined` with its button in the event
ensemble, so its outcome arrives as `{ event, model }`, and its status
folds `kept` in the fold block (order-form's receipt; its submit, of two
outcomes, `# joined2 @"Order submitted" @"Total missing"`).
Where nothing loops after the action, it needs no join: its `Aff`
returns the model under each outcome and the statuses fold `identity`
(flight-booker). The load before the knot is `load`
(`indeterminateLinearProgress # load loadPeopleCatalogue`), the `×→+`
action `body`'s one feed of `{}` runs, its outcomes typed by its `Aff`
and named by the capture or status after it. No word passes an event *around* a
stage: every event reaches the fold block on its own.

Design-system twins are two view modules over the same view model module, so
anything that would differ between twins is view by definition. An app
with no business functions (hello-world) is a single view module.

## Code style

The contract for application code. Each rule is strict: code that
breaks one is wrong even if it compiles and runs.

**The anchor invariant.** Every view line names exactly one anchor, in
the anchor's own position, and the anchor says what the line is:

- a **field** — the type argument of an editor, selector or group: the
  label is the model field the line edits (`filledTextField @"First
  name"`, `group @"Customer" $ …`);
- a **case** — the type argument of an emitter, pane or status
  (`button @"Submit order"`, `# inCase @"problem" bookingState`,
  `snackbar @"Flight booked" identity`);
- a **copy function** — the positional argument of a display
  (`text balanceLine`, `imagePane developedShot`);
- **nothing** — chrome (`card`, `h1 >>> cl "restaurant-name" $ staticText
  "Osteria Yoneda"`, `topAppBar "Espresso Bar"`). A static's text is a
  value, not an anchor: it needs no data to be seen.

**A field is named, never picked.** A line that reads a field the model
stores names it by its label, in the word's own slot, never with an
accessor: a display shows it (`textAt @"title"`, todo-list), a decorator
sets it (`attrAt @"x" "cx"`, circle-drawer), a click reports it
(`tile @"Cell picked" @"key"`, cells), a pane is in one of its cases
(`# providedAt @"halted" @"phase"`, stopwatch), a collection repeats it
(`# anyOfAt @"key" @"cells"`, cells; `listOfAt' @"Message opened"
@"id" @"messages" { … }`, inbox). Each `…At` word is the sibling of a word
that takes a function, and the function form is for what is derived —
a copy function, a classifier, a projection, a read deeper than one
field (weather's `text cityLine`) — named in the view model module. So
no view contains `_.`.

A view **writes no row**. The model row, a classifier's cases, a
projection's element row and a payload are the view model's signatures
to state; written on a view line they would be a second copy of those
signatures (see *The model is stated once* in
[Types and values](#types-and-values)), an action's outcomes included —
they are its `Aff`'s, closed by what tells them.

So every leaf reads as a noun phrase — word, anchor, positional
arguments:

```purescript
snackbar  @"Flight booked" identity            -- the "Flight booked" snackbar, folding identity
confirmed @"Refund a customer" "Refund"        -- the "Refund a customer" dialog, its case the title; the caption a value
button'   @"Sign up" { icon: "person_add" }    -- the record: optional presentation
```

**A record never holds an anchor or a required value.** A record stays
only for optional presentation (`{ icon }`, `{ selected }`, on a primed word) or for
two same-typed values a positional pair could swap
(`drawer { title: "Darkroom", subtitle: "photos drawn on the spot" }`).
If you must open a record to learn what the line is about, the line
breaks the invariant. Two value types from the library appear in
application rows like `Number` does: the bounded quantity
`{ current, min, max, step }` and the duration `{ ms :: Number }`.

Reading the view is then reading the model: an editor line says which
field, an emitter or pane line which case, a display line where its
text is computed, a chrome line nothing.

### Layout

- **No comments.** Code reads on its own.
- **Imports are 100% explicit, `Prelude` included.** Add and remove the
  names each change touches.
- **A one-line UI function is inlined.** A named function whose whole
  body is one pipeline expression is indirection: write the expression
  where it is used (`snackbar @"Order submitted" kept` sits in
  the status merge). A UI function earns its name only by spanning
  lines.
- **A line leads with what is seen, via `$`, and trails with data
  concerns, via `#`.** No data word leads a line. An element over one
  leaf applies it with `$`, never with parentheses (`headline4 $ text
  countLine`, `card $ body1 $ text summaryLine`); a chain on the wrapped
  element opens the paren before the wrapper (`( headlineSmall $ text
  wonLine ) # inCase @"won" gameOutcome`, tic-tac-toe). The one
  exception is `static`, whose argument is an ocular — polymorphic, so
  `$` cannot pass it: `static (el "input" >>> "type" := "checkbox")`
  (reorder).
- **A decorator rides its element.** `cl`, `clWhen`, `attrWith` and
  `tooltip` compose onto a container with `>>>`
  (`td >>> attrWith "style" cellFace $ …`) or trail a finished leaf with
  `#` (`checkbox @"Loyalty member" … # tooltip "Members get 10% off"`);
  a decorated element over a leaf composes the decorator onto the element
  (`span >>> clWhen isCompleted "todo-done" $ textAt @"title"`,
  todo-list); never lead with one.
- **Closing parens and `#` chains never start a line.** A trailing
  chain stays on one line at the end of the component's last line, and
  nested closers cascade onto that same line, each spaced from the chain
  it closes over: `… choice @"Pavlova") ) # acted @"name"` (potluck). The
  exception is a seed closer, `) # looped # with seed` / `) # with seed`,
  on its own line. An action's outcome row too long for its line
  starts a line of its own, two columns deeper than the line it belongs
  to, and its closer carries the rest of the line (flight-booker's
  action).
  `#` binds tighter than `$`: where a chain must apply to a whole
  wrapped element (a `foreach` multiplying a card), open the paren
  before the wrapper.
- **A long signature breaks.** A view model signature that does not fit
  100 columns puts the name on a line of its own, `::` and each `->`
  leading a line beneath it, and a record or variant too long for its
  line one field per line under its opening bracket (crud). Only the
  whitespace, and the synonyms naming its rows, differ from the hint
  with its tails filled.
- **Two-space indentation.** A block's lines sit two columns deeper
  than its opener, `( Semigroupoid.do` included; a closer returns to its
  opener's column. No four-space steps, no alignment to a token
  mid-line.
- **The architecture reads off the pipeline**, in this order: load →
  form (×→×) → live summary → events (×→+) → each action with its
  statuses (+→+ then +→×), closed by the seed. No registries, config
  objects or reflective assembly.

### Types and values

- **The model is stated once, in the view model module.** The seed's
  signature is the model row (`freshCount :: Count`, `type Count =
  { counted :: Int }`), and
  every other row is the signature of the function that brings it: a
  classifier's cases (`quizPhase :: … -> [ asking :: {}, finished :: {} ]`,
  quiz), an action's outcomes, a projection's element row
  (`lapRows :: … -> Array Lap`, `type Lap = { number :: Int, tenths ::
  Int }`, stopwatch).
  The view writes none of them (`# looped # with freshCount`, `# provided
  @"asking" quizPhase`, `# foreach @"number" lapRows`), and every
  editor, selector, list and field label it names is checked against
  them — so a stored field is read by its label (`listOfAt … @"messages"`,
  `# providedAt @"halted" @"phase"`). The view states only what is its
  own: the labels it names — an action's outcomes among them where its
  followers name them (`snackbar @"Flight booked" identity`, `joined
  @"Receipt printed"`) — and an option list, closed by `<+>` (`choice @"one-way" <+> choice
  @"return"`).

  What no signature states is not in the model. State only the business
  needs is derived (inbox computes the next message id from
  `messages`) or stored like any other field (circle-drawer's undo
  stacks are state, so they are in its seed's row).
- **No nominal types in UI.** No `data` or `newtype` for anything a
  component shows, emits or is configured with. A view-model row holds
  records, variants, `String`/`Number`/`Int` and `Array` — nothing
  else; `{}` for an empty payload, never `Unit`.
- **Name a shared row with a `type` synonym.** A synonym is a name for a
  structure, not a new type, so it keeps the row structural: flight-booker
  writes its model row once, `type Trip = { … }`, and every signature
  says `Trip`. Export a synonym an exported signature uses (the compiler
  requires it); no view imports it, since the view writes no row. Name
  it in business language (`Trip`, `Itinerary`), never by its role
  (`Model`, `State`).
- **No `Maybe` in the model.** Name the two cases the business already
  uses: `selected :: [ picked :: { index :: Int }, none :: {} ]`,
  `approval :: [ approved :: { attempt :: Int }, pending :: {} ]`.
- **No `Boolean` unless a Boolean editor edits it** (a `toggleSwitch`
  over `"Teams link"`). A flag nobody edits is a phase with
  two states — `status :: [ active :: {}, completed :: {} ]`,
  `drag :: [ adjusting :: {}, settled :: {} ]` — and a styling test over
  it is a named predicate (`clWhen isCompleted "todo-done"`).
- **Nominal types live below the UI**: a recursive type (cells' formula
  AST) or an ecosystem API (`Aff`, `Either`), entering the UI only
  through business functions.
- **Names say what, in business language.** Role names sit on values
  and functions, never on types (`with plannedTrip`, `with emptyCanvas`).
  Never lifecycle words: no `initial`, `default` or `seed` as a name, no
  entry function called `main`.
- **Business values are model data, not UI literals.** If the business
  owns a value, it rides the row: a slider edits a bounded quantity
  whose bounds come from the seed and may change at runtime. What stays
  a constant — a tick period, a fixed payload, the seed — is a named
  value in the view model module (`tickPeriod`, `smallestLoan`,
  `roomTemperature`). UI code keeps only presentation: labels, icons,
  styles, structure, layout numbers (`{ columns: 80, rows: 3 }`).
- **Copy is a function, not a field.** A display's text comes from a
  copy function in the view model module, named on the view line and
  unit-testable (`countLine { counted: 3 } == "3"`). A whole line is one
  function, glue included — never several leaves with `staticText`
  between them, never a formatter in the view:

  ```purescript
  headlineSmall $ text balanceLine
  ```

  with `balanceLine :: Till -> String`, `type Till = { balance ::
  Number }`. A
  display that shows one field as it is stored names the field
  (`textAt @"title"`). A number drawn as a bar is a function too — a
  fraction is derived from state, like a sentence. A model field exists
  because the app's state needs it, never because a display wanted a
  `String`.
- **Fixed copy is static or constant.** A **static** is on screen before
  and regardless of any data — a heading, a note, a checkbox's caption —
  and is a value, never a label, since it names no field and no case:
  `staticText "Hours"`. A static that introduces what follows — a
  heading over its section, a caption before its field — is written
  first, a line like any display (signup-form's `headline4 $ staticText
  "Create account"`; inside one element, a block of two lines: `label $
  Semigroupoid.do { staticText "°C "; input @"Celsius" "text" }`,
  temperature-converter-html). A **separator** is written the same way,
  leading over what it introduces (`divider $ slider @"Attendees" #
  inCaseAt @"chosen" @"Room"`, meeting-booker), and shows itself only
  between things: while something is rendered before it and what it
  introduces renders something — never above an absent pane or an empty
  list. A static inside a pane is still a static: the pane decides when
  it shows, the text reads nothing (`staticText "Error" # inCaseAt
  @"faulty" @"condition"`, calculator; `caption1 $ staticText "Seats
  taken"`, meeting-booker) — a copy function that ignores its row is a
  static written in the wrong module. A **constant** is fixed copy that
  shows only through data — a sentence's glue, or a line carrying a
  business value — and lives in the copy function that reads it (`text
  refundLine`, cashbox: the refund amount belongs to the view model, not
  the view). Text that *is* data (a parsed markdown run) is
  `staticText` too.
- **A label is read back, never restated.** A case label is the copy
  its option draws (`choice @"Oat milk"`, `choice @"Cash"`); a line
  that shows it reads it back with `caseText` from `Data.Variant.Case`,
  adapting at most its letter case (order-form's `payingLine r =
  "Paying by " <> toLower (caseText r."Method")`). A label is never
  shaped to fit a sentence elsewhere — `"with oat milk"`, `"cash"` in
  lower case for one line — since a second sentence could not share
  it: grammar a sentence needs lives in its copy function
  (espresso-bar's `milkText`). A `match` that only echoes its case
  labels is the label restated. A map that does real work —
  shortening, glyphs, per-case sentences — stays a named copy function.
  An affix the option shows is part of its copy (`choice @"15 min"`).
- **Events carry data, never UI copy.** An emission carries the order,
  the outcome, the reason — a reason as a case of its own, since the
  status telling it shows its label and nothing else.

### Business functions

- **The signature is the hint with its tails filled.** Every business
  function is typed as the view reports it, each unknown filled once
  and the same everywhere it appears: the row the stage is fed, closed
  (`Record t0 -> String` reported, `countLine :: Count -> String`
  written, `type Count = { counted :: Int }`). Never narrowed to the
  fields the body reads, never opened with a tail, never coerced at the
  call site. Which fields the body uses is the body's business; the
  compiler checks the body against the row it is given. A name two lines
  call at different rows gets two hints, and that is the one case for a
  `forall`: generalize to the fields both rows share, with a tail. A
  row the signature names through a `type` synonym is the same row
  (`problemLine :: Trip -> String`, flight-booker): the check expands it.
  Private helpers no view line types are free to be as general as they
  like (flight-booker's `parse`, `formatDate`).
- **Update the row you are given.** A function returning the row it
  takes — a handler, a `settled` normalizer, a periodic step — updates
  it (`increment m = m { counted = m.counted + 1 }`), never builds a
  literal.
- **A preset is a field update**, even one that reads nothing:
  `beginTiming sw = sw { phase = .timing {} }` (stopwatch),
  `fold @"Reset" restarted` (timer). A constant
  replaces the model only as a whole, by a reset: `reset @"New
  game" openingPosition` (tic-tac-toe).
- **One record per business function.** Records that travel together
  are one row; let field names carry the roles positional arguments
  lose. A fold handler is no exception: a joined event arrives as one
  record, the payload under `event` and the model under `model`:

  ```purescript
  fold @"Todo toggled" toggleTodo
  toggleTodo :: { event :: Int, model :: { todos :: …, … } } -> { todos :: …, … }
  toggleTodo { event: i, model: m@{ todos } } = m { todos = … }
  ```

  A button replaying the row it is fed is handled by the row update
  itself (`"Add": addTodo`, `addTodo :: model -> model`); an event with
  a payload of its own is `# joined @l` on its line and handled by one
  function of one record (`toggleTodo :: { event :: Int, model :: … } ->
  …`); a fixed payload is baked into its handler (cashbox's
  `refundStandard`); an effect returns the model under its outcome case
  (`createPerson :: model -> Aff [ "Person created" :: model, "Person not
  created" :: model ]`), so each outcome's fold is `snackbar @"Person
  created" identity` (the payload becomes the model;
  `capture @l` where nothing needs telling); an outcome that changes
  nothing folds `kept` over its joined event; a constant replacing the
  model is `reset @"New game" openingPosition`, or `(const x)` on a
  status. A status shows its case label, never the handler's result.
- **A status only after an action.** A fold opens with a visible
  status where an action's outcome row names what the status tells
  (crud's `snackbar @"Person created" identity`, the action returning
  the model under that case). A command straight from an
  emitter folds silently, whatever it does, since the screen already
  shows its result and its case is the button's caption (counter's
  count, cashbox's till, inbox's list): `fold @"Todo toggled"
  toggleTodo`. What a status would compute from its payload is not
  copy: a value the user must keep is a model field shown by a display
  (payment's `text statusLine` carries the attempt), a reason is a
  sibling case (`"Total missing"`).
- **A handler carries no field it does not touch.** Group buttons into
  stages by the fields their handlers touch (circle-drawer keeps undo and
  redo apart from the canvas click). An identity handler means the
  event was never model data: show it with a display stage instead.
- **Lossy conversions live in the model.** An editor round-trips its
  field; a normalization that loses information is `# settled` on the
  stage (cells' formula field `# settled commit`), never hidden inside
  the component.
- **No dispatcher.** A function that only `match`es event cases has no
  place: each case is its own `fold @l f` line (`status @l line f`
  where an action's outcome is told), `f` and `line` named business
  functions in the view model module.

### Wiring

- **Speak the vocabulary; never import the ecosystem's
  `Data.Profunctor`.** The words of `PUI`, `PUI.Web`, the design-system
  module and the merge blocks of `Data.Profunctor.Row.*` are vocabulary;
  raw `lcmap`/`rmap`/`dimap` are not. Every reshaping an app needs has
  a home in a word's own argument — `foreach @l rowsOf`,
  `dispatched @k @a arrival`, `settled normalize`, with `identity`
  meaning "the value as it is". A shape none fits is a gap to report,
  not a reason to reach below the vocabulary.
- **State lives in the model.** No FFI stashes, no module-level mutable
  references, no reading the DOM back, no window globals, and no hidden
  loop channel: a running maximum, a wizard's step, a counter are model
  fields the folds and normalizers update (auction, checkout,
  ticket-dispenser).
- **Lean on the design system; write no custom layout.** Stock
  components, surfaces and typography carry the design language; a
  styled flex, border or margin wrapper is a smell. An unstyled `div`
  keeping a pane's parts together is fine (meeting-booker). Custom style
  is for data-driven graphics only — an SVG canvas, a colour swatch. A
  surface is stated once (`elevation* $ card $` stacks two shadows).

## Writing order

**The view determines the view model up to its tails.** Every view
line names what it needs and the labels it binds, so a view written
with a typed hole for every value it would import compiles to a list of
those values with their types: which share a row, what each returns,
every field and case the lines name — and, as an unknown, the rest of
each row, the **tail**. The tails are the model: many models fit one
view, and the view model module is where one is chosen. Write the view
first and the view model module to that list, filling the tails. Until it exists a **hole** stands in for each
missing value, and there are two:

| Hole | Written | Compiles? | Gives you |
| --- | --- | --- | --- |
| typed hole (the compiler's) | `?countLine` — any name after `?` | **no**, by design | the value's full inferred type, reported at the hole |
| runtime hole | `hole` (`import PUI.Web (hole)`), a value of every type | **yes** | a view that builds and runs; it throws only when data reaches it |

Use `?name` to learn what a missing value must be and `hole` to see the
view run before it exists. One `?name` fails the build, so to run the
view, turn every remaining `?name` into `hole` or its definition. With
the watch build running ([building.md](building.md)):

1. **Write the view**, each line naming its anchors
   ([Code style](#code-style)). Labels never need a hole, because writing
   them is writing the view's part of the model; the view writes no row.
   Every other value — a copy function, a
   handler, a classifier, an action, the seed — starts as a typed hole
   (`text ?countLine`, `# looped # with ?freshCount`).
2. **Read the holes.** With several holes, a last message lists them
   all, placed on the declaration's name, with one numbering of the
   unknowns across them. Counter's three:

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
   that type is; `inboxState` returns a variant with an `empty` case,
   because `inCase @"empty" inboxState` says so, and stopwatch's
   `providedAt @"halted" @"phase"` puts `phase :: [ halted :: t2 | t3 ]`
   in its model the same way.
3. **Write the signatures down: the hint with its tails filled.** Fill
   each unknown once, and the same everywhere it appears: counter's `t0`
   is `counted :: Int`, so the model is `{ counted :: Int }` — named
   once, `type Count = { counted :: Int }` — and `countLine :: Count ->
   String`, `increment :: Count -> Count`, `freshCount :: Count`. Close
   every row: a filled tail adds the fields
   and cases only business functions reach, and the result is the row
   the stage is fed, its fields in the compiler's order. Do not narrow it
   to the fields the body reads and do not leave a tail open: which
   fields the body uses is the body's business, and the compiler checks
   the body against the row it is given. A name reported at two
   different rows, because two lines call it, is the one case for a
   `forall`: generalize to the fields both rows share, with a tail. A
  row the signature names through a `type` synonym is the same row
  (`problemLine :: Trip -> String`, flight-booker): the check expands it. The
   hole's message goes away, and from then on the compiler checks the
   view against the signatures.
4. **Fill the holes in any order.** The compiler reports one
   declaration's holes per build, and an app is one pipeline in one
   declaration, so its holes report together; a view helper's holes
   (markdown-previewer's `documentView`) surface after the entry's. A
   helper applies one view model function per value — composing two
   (`rgb (mixOf channels)`) hides the type between them, so the view
   model exports the composite (color-mixer's `mixedColor`). The seed's
   signature is the model row, and its value is one business-named
   record (`freshCount`, `mondayMail`).
5. **Run the view on holes, at any point.** Replace the typed holes
   with `hole` — inline (`text hole`, `# with hole`) or as view model exports
   stubbed without signatures (`countLine = hole`, the view importing
   the names it will keep). The view builds and shows its initial UI —
   chrome, editors, buttons — before any business function exists. A
   seed, a period or an action that is still a hole counts as absent:
   the pane it feeds stays blank rather than failing. While the seed is
   a hole, input has nothing to join and reaches no hole; the starvation
   warning may report that, which is expected while holes remain. Once
   the seed is real, input reaches the next unwritten function and
   throws: **`bambik: a hole was reached` in the console names the next
   function to write.** A view that throws at mount applies a view model
   value instead of handing it data — see
   [View module and view model module](#view-module-and-view-model-module).
6. **Finish with no hole left.** A runtime hole compiles, so nothing
   stops one from shipping: the app is done only when
   `grep -rnw hole src/` comes back empty, and then only once it runs
   ([Finish by running it](#finish-by-running-it)).

A design-system twin inverts the loop: its view model module already exists,
so the view is written against known signatures.

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
- **A blank pane is a missing seed — or a line after an event.** A
  merge that shows nothing is waiting for a field nobody has given a
  value; the watchdog names it (below). A line that shows nothing until
  the first click stands after the loop's emitters and folds: that pair
  is one stage that releases on events, not on the seed, so what must
  show at mount stands before it ([App shape](#app-shape)).
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
| the model row against `( event :: …, model :: … )`, `while matching label "Start"` | the fold of that case | an emitter inside a pane is `# joined`, but it already replays the model: `joined` is for an emitter fed something narrower, a list element or a canvas | drop the `# joined` and take the model in the handler (stopwatch) |
| `( "Cout" :: … )` against `( "Count" :: … )` | the fold | one case spelled two ways | spell the case the same on the emitter and its fold |
| `( "Reset" :: … )` against `( … )` | the emitter | no fold handles the case | add its fold to the fold block, silent if nothing needs telling (timer's `fold @"Clock ticked" tick`), or remove the emitter |
| `( size :: Int … )` against `( … )` | the seed line | a business function names a field the model does not have | copy the signature from the hole its line reports ([Writing order](#writing-order)) |

## When it does not propagate

The compiler proves the wiring, not that data reaches the screen. A
blank pane or a stale readout almost always means a merge is waiting
for a field that has no value. That field belongs to an editor, a
source or the seed — never to a display — so the fix is always a seed
or a missing source, never something on the display. Three aids find
it in the browser:

- **The starvation warning**, on by default. A merge still waiting after
  3 s prints one `console.warn` naming the missing fields and the fix,
  and logs the page elements of those fields beside it, so clicking one
  in DevTools shows where it is. Turn it off with
  `window.__bambikNoWarn = true`.
- **The emission trace**: `window.__bambikTrace = true` (or
  `localStorage.setItem("bambik-trace", "true")`) logs every step of
  every update as `console.debug` — enable DevTools' Verbose level. It
  prints the labels your view names, one more reason to name cases
  rather than build variants inline.
- **The accessibility tree.** Every labelled component stamps its label
  on its element, so DevTools' accessibility view shows the app in the
  model's own words: groups as sub-records, editors as fields with
  their values, buttons as business cases.

## Finish by running it

A module that compiles is not a delivered change. Every change ends with
the app running in dev mode, checked in a browser as
[building.md](building.md) describes, and its URL handed back to the
developer: waiting merges are invisible to the compiler and obvious on
screen.

## Looking things up

- **What a word does, its signature and options**: the module headers.
  Browse them all with `npx spago docs --open` in the app, or read the
  source under `.spago/bambik/<tag>/`: `src/PUI.purs` (stages and
  collections), `src/PUI/Web.purs` (text, panes, clicks, attributes,
  `hole`), `src/PUI/Web/HTML.purs` and `SVG.purs` (elements), and one
  module per design system under `src/PUI/Web/` (`MDC2`, `MDC3`,
  `Shoelace`, `Fluent`, `Bootstrap`) — the same concept keeps the same
  name across them.
- **How it is written in a real app**: the demos under
  `.spago/bambik/<tag>/demo/`.
- **Which word fits a situation**: [vocabulary.md](vocabulary.md).
- **One demo read line by line**: [walkthrough.md](walkthrough.md).
