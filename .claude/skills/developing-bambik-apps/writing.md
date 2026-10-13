# Writing a bambik application

The rules below govern the two app modules the scaffold ships: the view
module (`src/<Module>.purs`) and the view model module beside it
(`src/<Module>ViewModel.purs`), whose signatures the view determines
([Writing order](#writing-order)). [Code style](#code-style) is the strict
contract; the sections before it are the shapes that contract is
written in. Other files of this skill point here and state no rules.

This file never documents a component. What a word does, its signature
and its options are in the library's module headers — see
[Looking things up](#looking-things-up). Words appear here only as
examples, each from a demo you can open under
`.spago/bambik/<tag>/demo/7guis/` or `demo/nguis/`. A demo directory's
suffix names its design system (`counter-mdc2`, `counter-mdc3`, …,
`counter-html`); the siblings share one view model module in the
unsuffixed directory (`counter/CounterViewModel.purs`), so read
whichever twin matches your design system.

## Terms

- **record** `{ … }` (written ×) — knowledge: everything at once.
  **variant** `[ … ]` (written +) — an event: one case at a time.
- **shape** — what a component takes in and gives out, record or
  variant on each side. There are exactly four, and every component has
  exactly one:

  | Shape | Kind | Examples |
  | --- | --- | --- |
  | ×→× | **editor**, **display**, **stage** | `filledTextField @"Email"`, `headline4 (text countLine)` |
  | ×→+ | **emitter** | `button @"Count"` |
  | +→× | **status** | `snackbar @"booked" bookedLine` |
  | +→+ | **handler** | a backend action |

- **stage** — one step of the pipeline: its output is the next step's
  input.
- **merge** — several components over one shared value, written as a
  qualified `do` block (below).
- **seed** — the model's value at start, on the line that declares the
  model row (`# looped @( counted :: Int ) # with freshCount`). A pane
  stays blank until the fields it waits for have values; a seed gives
  them one.
- **pane** — a component that exists only while the model is in one
  case (`# pane @"estimated" distanceOf`).
- **anchor** — the one model symbol a view line names: a field, a case,
  a copy function, or nothing (see [Code style](#code-style)).
- **view model module** — the module beside the view holding the seed
  and every function the view calls; the view determines its
  signatures ([Writing order](#writing-order)).
- **copy function** — a pure function in the view model module from the
  row to the words on screen (`countLine`).
- **chrome** — headings, cards and other parts that show no data.
- **hole** — a stand-in for a value not written yet
  ([Writing order](#writing-order)).

## The pipeline

The app is one pipeline composed with `Semigroupoid.do`
(`import QualifiedDo.Semigroupoid as Semigroupoid`): each line's output
is the next line's input, so code order is screen order *and* data
order. A **stage** (an editor, an event ensemble, a fold) is a line as it
stands. A line that **shows data** — a display, a static, a pane, a list
of displays: it outputs `{}` — changes no data, so it is not a stage but a
decorator over the stage it stands before, written `prefixed d $ stage`
(counter's `prefixed (headline4 (text countLine)) $ button @"Count"`);
several in a row are one `RecordToRecord.do` group under one `prefixed`
(tip-calculator). Displays **after** a stage whose row must flow on —
tip-calculator's totals after the last slider, stopwatch's laps after the
folds — trail it: `slider @"Split between" # suffixed ( RecordToRecord.do
… )`. A display that ends a block, or the whole content of a pane, is
written plainly: nothing follows it, so there is nothing to decorate. Four more qualified `do` blocks put several components over one
value, one per shape:

- `RecordToRecord.do` (×→×) — chrome and displays reading one record
  together (espresso-bar's caffeine readout, shopping-cart's rows).
  Never an editor: editors are pipeline stages ([Components](#components)).
- `RecordToVariant.do` (×→+) — a row of buttons over the model
  (cashbox, stopwatch).
- `VariantToVariant.do` (+→+) — one handler per event case: backend
  actions (crud).
- `VariantToRecord.do` (+→×) — one status per outcome (flight-booker's
  two snackbars).

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
[The anchor invariant](#code-style)). Put punctuation and units on
the label (`filledTextField @"Start date (DD.MM.YYYY)"`,
`sliderLive @"Amount (€)"`); where a symbol is the conventional
caption, write the symbol (`@celsius`). A leaf takes no options record:
the label is its caption. The few words with optional presentation have
a `…With` sibling taking it (`buttonWith @"Place order" { icon:
"local_cafe" }`, `listOfWith … { selected: isSelected } …`).

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
  `dropdownUnpicked @"Room" @"chosen" (choice @"Focus pod (4 seats)" <+> …)`).
- **Displays** take a copy function, not a label:
  `headline4 (text countLine)` (counter). A number shown as a
  bar or gauge takes a function too, and keeps its label only as the
  accessible name (`linearProgress elapsedFraction`, timer).
  See *Copy is a function* in [Types and values](#types-and-values).
- **Emitters** emit their own case. Whatever the business decides about
  it is decided where the case is consumed — the fold's handler, the
  status's copy function. When two buttons would share one handler,
  keep two business cases and apply the handler in each case's fold
  (checkout's `unshown @"Next" # fold stepTo`,
  `unshown @"Back" # fold stepTo`).
- **Statuses** are labelled with the case they show and take its copy
  function: `snackbar @"booked" bookedLine`. Mutually exclusive outcomes
  are sibling statuses in one `VariantToRecord.do`, each owning its
  case. A status that must also let the event flow on is
  `# observed` (payment's retry toast).
- **Chrome** (`card`, `topAppBar`, typography, dialogs) wraps other
  components and adds nothing to the model; code order is screen order.
  A card around editors is a sub-record: write it as the labelled group
  `group @"Customer" $ …` (order-form), whose label is the sub-record's
  field and heading. A plain `card` holds only content that edits
  nothing (order-form's summary). The app itself takes no surface: the
  entry is `body $ …`, never `body $ card $ …`.

**A component's type is its shape, and so is every component the app
packages itself** (order-dashboard's `statTile "Orders placed"
ordersCount`). A signature with a bare `String`, `Maybe a` or `a` on
either side is a smell: that value belongs in a field of the row, read
by a business function.

A `…With` word's options are a record whose field names belong to the
component (`buttonWith`'s `icon`, `listOfWith`'s `selected`), so copy a
demo's call or read the header instead of guessing — a guessed field
fails as `TypesDoNotUnify` on the options record.

## Stages

An editor is a stage, and a display or chrome is a line that shows data,
as they stand. Everything else takes a trailing word that says what it is
for:

| The line | Write | Demo |
| --- | --- | --- |
| a display or chrome, always there | `headlineSmall $ text orderLine` | order-form |
| a display shown in one case | `text distanceLine # pane @"estimated" distanceOf` | order-form |
| an editor that exists in one case | `filledTextField @"Table" # inCaseAt @"Dine in" @"Mode"` | order-form |
| a button that exists in one case | `buttonWith @"Start" { icon: "play_arrow" } # providedAt @"halted" @"phase"` | stopwatch |
| a list rendered from the row | `ul $ (li $ text lapLine) # foreach @"number" lapRows` | stopwatch |
| a display before a stage | `prefixed (headline4 (text countLine)) $ button @"Count"` | counter |
| several displays before a stage | `prefixed ( RecordToRecord.do … ) $ slider @"Split between"` | tip-calculator, weather |
| displays after a stage whose row flows on | `slider @"Split between" # suffixed ( RecordToRecord.do … )` | tip-calculator, stopwatch |
| a list element echoing the row it shows | `prefixed (listItem $ text flightLine) identity # dispatched …` | departures, scoreboard |
| content that waits for the user to confirm | `confirmed "Refund" "Refund the customer?" $ …` | cashbox |
| an event folded into the model | `unshown @"Add" # fold addTodo`, one per case, each opened by its status, merged in `VariantToRecord.do` | todo-list |
| an event carrying something of its own, joined with the model | `listOf @"Todo toggled" … # joined @"Todo toggled"` | todo-list, cells, inbox |
| a periodic occurrence | `unshown @"Clock ticked" # ticks tickPeriod` | stopwatch, timer |
| an invariant between edited fields | `filledTextField @celsius # settled fromCelsius` | temperature-converter |
| effects on the buttons' cases, the other events passing | `( VariantToVariant.do { indeterminateLinearProgress # action createPerson # atCase @"Create"; … } ) # subChoice`, every action typed at the block's outcome row, every outcome folded once by its status | crud |
| an effect whose outcome is the model | `indeterminateCircularProgress # action fetchReport # atCase @"Forecast requested"` then `unshown @"Forecast fetched" # fold identity` | weather |
| an effect opened by its outcome statuses | `( VariantToRecord.do { indeterminateLinearProgress; snackbar @"Flight booked" bookedLine; snackbar @"Booking rejected" rejectedLine } ) # action @( … ) submit # atCase @"Book"` | flight-booker |
| a fold nothing needs telling about | `unshown @"Todo toggled" # fold toggleTodo`, `unshown @"Clock ticked" # fold tick` | todo-list, timer, stopwatch |

Content of the panes and of `confirmed` must output `{}`. An
assembly that emits something you mean to drop is dropped **in
writing**, with `# muted` (scoreboard's summary list).

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

An app is one loop through the four shapes, tied once by `# looped @( … ) # with seed`:

```purescript
Semigroupoid.do
  prefixed (headline4 (text countLine)) $     -- ×→× a display, decorating the stage it stands before
    RecordToVariant.do                        -- ×→+ the event ensemble: every emitter and pane
    button @"Add"
    listOf @"Todo toggled" … # joined @"Todo toggled"   --   an event with a payload of its own, joined with the model
  VariantToRecord.do                          -- +→× the folds, one per case, each opened by its status
    unshown @"Add" # fold addTodo
    unshown @"Todo toggled" # fold toggleTodo
    ( Semigroupoid.do                         --   an effect on a case, folded by its own outcomes
      indeterminateLinearProgress # action createPerson
      VariantToRecord.do
        snackbar @"Person created" personCreatedLine # fold identity
        snackbar @"Person not created" personNotCreatedLine # fold identity ) # atCase @"Create"
# looped @( … ) # with seed
```

A fold opens with its status, `snackbar @l line # fold f`, as an effect
opens with its progress indicator (`indeterminateLinearProgress # action
f`): every line, whatever its shape, starts with a UI component.
The status names the case — its label is the event's, human copy like
`"Person created"` — and shows the **event**, never the fold's result:
it is fed the occurrence as it arrives, so its copy function is typed at
the payload and everything it says must be in the event. A status with
copy therefore stands after a `+→+` block that returned what it tells
(crud's action returns the model, so `personCreatedLine :: model ->
String`). A command straight from an emitter has nothing to tell that
the displays do not already show, and a heartbeat nobody needs told
about has nothing at all: both open with the case unshown, `unshown @l`
(`unshown @"Count" # fold increment`). A fold is memoryless: a replaying emitter's payload is the row it was fed, an event
with something of its own arrives `# joined @l` with that row as
`{ event, model }`, and an effect returns the model. So every handler has
the model in hand and the loop's memory stays at the emitters and in
the knot. Each fold releases the whole next model, and the `+→×` merge
forwards each release as it comes — its output row is shared, where the
`×→×` merge's operands own their fields — and a status with nothing to
fold (an outcome outside any loop, flight-booker) stands alone in the
block, releasing nothing and so fitting any row. The seed and every later model enter at the top, so everything that
must be on screen at mount — displays, editors, the list a pick comes
from — stands before the ensemble; a stage after the fold is fed only by
events. Editors before the ensemble flow forward only; an editor whose
edit must reach its siblings keeps its own normalizer (`# settled`,
temperature-converter) or stands in a loop of its own: a form of several
editors is `( … ) # looped` inside the app's loop (order-form). An event
whose effect takes time is joined with the model when its outcome lands,
not when it fired: `joined` wraps the emitter and its action together
(`( Semigroupoid.do { button …; … # action estimateDistance # atCase … } )
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
| panes over one classifier | quiz, checkout |
| effects and time | password-generator, stopwatch, timer, weather |
| structure that varies with the data | markdown-previewer |
| no design system at all | restaurant-menu, helloworld |
| a knot fed from upstream, no seed of its own | order-form and crud (`looped` after the load action, which `body`'s one feed of `{}` runs) |
| an action that retries until it succeeds, narrated on its way in | payment (`chargeFlaky`, `# observed`) |
| state that used to be a hidden loop, now a model field | auction (`settled raiseTop`), checkout (`stepTo`), ticket-dispenser (`issue`) |
| a reusable sub-form over a sub-record; routing some events | parcel (`group @"Address" addressForm`), cashbox (`subChoice`) |
| an effect's outcome folded into the model it lands on | order-form (`… # joined @"Distance estimated"`, `fold setDistance`) |
| keyed event streams | departures (`dispatched`), scoreboard (`accumulated`) |

## Conditional visibility

A component that exists only sometimes is a **pane over a case**, never
a predicate in the view and never a `Maybe`. The argument is a business
function that classifies the model into a variant; the pane exists while
the variant is at the named case and is given that case's payload:
`pane` for a display, `inCase` for an editor, `provided` for an
emitter.

When the state is stored in the model as a variant, the pane names the
case and then the field, both by label — `providedAt`, `paneAt`,
`inCaseAt` (stopwatch's `# providedAt @"halted" @"phase"`,
flight-booker's `# inCaseAt @"return" @"Flight type"`) — typed by the
model row the seed line declares. When it is derived, one classifier
derives it, naming every case and giving each case exactly what its
pane shows — checkout's `checkoutStep`, calculator's `readout`, inbox's
`messageView` — and the classifier's first pane declares those cases
(`# provided @"reading" @( reading :: { sender :: String, subject :: String, body :: String }, browsing :: {} ) messageView`);
its later panes name only their case. Two panes over one classifier
can never both be on screen.

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

A dialog opens when it is fed and closes when one of its buttons emits.
Feed it only in the state that asks for it and put the deciding buttons
inside: inbox's `dialog "Delete the last message?" $ RecordToVariant.do
…` under `# provided @"confirming" deletionPane`. For a confirmation step
inside a flow, `confirmed` (cashbox).

A drawer's navigation is the first stage and its content the second, so
the pick reaches the content directly (photo-gallery).

## Collections

- **Keyed and kept.** `foreach @"key" rowsOf` and `listOf @l @k` render
  one element per row, identified by a key field of the row; an array
  the model stores is named instead of projected (`foreachAt @"key"
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
  argument, `# foreach @"key" (const keyPad)` (calculator).
- An element whose content reads several things names that reading
  once, in a face function, typed at the element row and closed like
  every other function a view line names
  (`attrWith "style" cellFace`, `cellFace :: { key :: String, text ::
  String, status :: [ … ] } -> String`, cells).

## View module and view model module

Every function belongs to one of two classes, in two modules with a
one-way dependency:

- **The view module** (`<App>.purs`) exports the one entry function and
  keeps UI-wiring functions that span several lines (a `dynamic`/`each`
  builder, a reusable sub-form like parcel's `addressForm`). It imports
  the design system, the library's words and the view model module.
- **The view model module** (`<App>ViewModel.purs`) exports business
  functions and named business values: seed models, tick periods, fixed
  payloads, copy functions, parsers, `Aff` actions — exactly the names
  the view imports, nothing a view line does not type (a helper only
  other business functions call is private). It imports only the
  domain — `Prelude`, plain data modules, `Effect`, `Aff`,
  `Data.Variant.Case` — never `PUI`, `PUI.Web.*`, a design-system module
  or the merges. The one exception is temporary: a stub `hole` while
  the view model is not written ([Writing order](#writing-order)).

**The view determines the view model module.** A value lives there
exactly when a view line calls it, and its signature is what that line
demands: compiled with a typed hole for each such value, the view
reports every one at a concrete type, and the module's signatures
are that list, verbatim
([Writing order](#writing-order)). Nothing is designed into the view
model module that no view line asks for.

The view hands the view model module **only arguments called on
data**: a copy function, a handler, a classifier, an action, the seed,
a period. It never runs one of its effects at the entry and never
applies a component built there. A stand-in server is a module of its
own beside the view model module (crud's `PeopleServer`), as a real
server sits behind its client library: the view model module imports it
like any domain module, the view never does, and its module-level state
is the one fake in the app, named as such.

**An action's outcome cases are named by its `Aff`** (`createPerson ::
model -> Aff [ "Person created" :: model, "Person not created" :: model ]`,
crud) and **opened by its statuses**: a progress indicator for
`started`/`ended`, a status per outcome case it shows, or both in one
`VariantToRecord.do`. The actions of a loop stand in one
`VariantToVariant.do`, one line per button's case, and **every action is
typed at the block's outcome row** — the union of all their outcomes,
as a display's function is typed at the whole model though it reads one
field (`createPerson :: model -> Aff [ "Person created" :: model, "Person
deleted" :: model, … ]`, the six cases of crud's three actions, each
`Aff` returning its own). The block's outputs are one row (an equality,
like the `×→×` merge's inputs), so the fold block after it fixes the row
and nothing names an outcome on an action line; events the actions do
not handle pass `# subChoice`. Where only statuses follow (flight-booker;
order-form's submit and receipt, whose snackbars sit in the fold block
beside the folds), `@( … )` declares the payloads nothing downstream
fixes.

Design-system twins are two view modules over the same view model module, so
anything that would differ between twins is view by definition. An app
with no business functions (helloworld) is a single view module.

## Code style

The contract for application code. Each rule is strict: code that
breaks one is wrong even if it compiles and runs.

**The anchor invariant.** Every view line names exactly one anchor, in
the anchor's own position, and the anchor says what the line is:

- a **field** — the type argument of an editor, selector or group: the
  label is the model field the line edits (`filledTextField @"First
  name"`, `group @"Customer" $ …`);
- a **case** — the type argument of an emitter, pane or status
  (`button @"Submit order"`, `# pane @"estimated" distanceOf`,
  `snackbar @"booked" bookedLine`);
- a **copy function** — the positional argument of a display
  (`text balanceLine`, `imagePane developedShot`);
- **nothing** — chrome (`card`, `h1 >>> cl "restaurant-name" $ staticText
  "Osteria Yoneda"`, `topAppBar "Espresso Bar"`). A static's text is a
  value, not an anchor: it needs no data to be seen.

**A field is named, never picked.** A line that reads a field the model
stores names it by its label, in the word's own slot, never with an
accessor: a display shows it (`textAt @"title"`, todo-list), a decorator
sets it (`attrAt @"x" "cx"`, circle-drawer), a click reports it
(`clicked @"Cell picked" @"key"`, cells), a pane is in one of its cases
(`# providedAt @"halted" @"phase"`, stopwatch), a collection repeats it
(`# foreachAt @"key" @"cells"`, cells; `listOfAt @"Message opened"
@"id" @"messages"`, inbox). Each `…At` word is the sibling of a word
that takes a function, and the function form is for what is derived —
a copy function, a classifier, a projection, a read deeper than one
field (weather's `text cityLine`) — named in the view model module. So
no view contains `_.`.

A **declared row** is not an anchor either. It is a visible type
argument stating a shape the compiler could not otherwise know: the
model row where the model first appears, and a derived row where it is introduced
(see *The model is declared once* in
[Types and values](#types-and-values)). It names no field and no case.

So every leaf reads as a noun phrase — word, anchor, positional
arguments:

```purescript
snackbar  @"booked"  bookedLine              -- the booked snackbar, saying bookedLine
confirmed "Refund"   "Refund the customer?"   -- the title is static: a value
buttonWith    @"Sign up" { icon: "person_add" }  -- the record: optional presentation
```

**A record never holds an anchor or a required value.** A record stays
only for optional presentation (`{ icon }`, `{ selected }`, on a `…With` word) or for
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
  where it is used (`snackbar @"orderSubmitted" submittedLine` sits in
  the status merge). A UI function earns its name only by spanning
  lines.
- **A line leads with what is seen, via `$`, and trails with data
  concerns, via `#`.** No data word leads a line.
- **A decorator rides its element.** `cl`, `clWhen`, `attrWith`,
  `tooltip` and `tooltipWith` compose onto a container with `>>>`
  (`td >>> attrWith "style" cellFace $ …`) or trail a finished leaf with
  `#` (`span (textAt @"title") # clWhen isCompleted "todo-done"`,
  `checkbox @"Loyalty" … # tooltipWith loyaltyNote`); never lead with
  one.
- **Closing parens and `#` chains never start a line.** A trailing
  chain stays on one line at the end of the component's last line, and
  nested closers cascade onto that same line, each spaced from the chain
  it closes over: `… choice @"Pavlova") ) # acted @"name"` (potluck). The
  exception is a seed closer, `) # looped @( … ) # with seed` /
  `) # with @{ … } seed`, on its own
  line — or, when the model row is long, `) # looped` on its own line,
  the row's fields one per line beneath it and `) # with seed` closing
  the last (inbox). Every declared row too long for its line breaks the
  same way: the row starts a line of its own, two columns deeper than
  the line it belongs to, one field per line, and its closer carries the
  rest of the line (flight-booker's first pane and its action, crud's
  list).
  `#` binds tighter than `$`: where a chain must apply to a whole
  wrapped element (a `foreach` multiplying a card), open the paren
  before the wrapper.
- **A long signature breaks.** A view model signature that does not fit
  100 columns puts the name on a line of its own, `::` and each `->`
  leading a line beneath it, and a record or variant too long for its
  line one field per line under its opening bracket (crud). Only the
  whitespace differs from the hint.
- **Two-space indentation.** A block's lines sit two columns deeper
  than its opener, `( Semigroupoid.do` included; a closer returns to its
  opener's column. No four-space steps, no alignment to a token
  mid-line.
- **The architecture reads off the pipeline**, in this order: load →
  form (×→×) → live summary → events (×→+) → each action with its
  statuses (+→+ then +→×), closed by the seed. No registries, config
  objects or reflective assembly.

### Types and values

- **The model is declared once, where it first appears; every derived
  row where it is introduced.** For a loop closed with a model that is
  the seed line (`# looped @( counted :: Int ) # with freshCount`; inbox's two
  fields, one per line). A load action before the knot
  emits its outcome as an event folded into the loop (`unshown @"People
  loaded" # fold identity`, crud and order-form), so the
  knot declares the model like any other. Every editor, selector, list and field label is checked
  against it — so a stored field is read by its label
  (`listOfAt … @"messages"`, `# providedAt @"halted" @"phase"`). A
  **derived row** is a shape no model field holds, and the line that
  introduces it declares it after its anchor:
  - a classifier's cases, on its first pane
    (`# pane @"cart" @( cart :: { item :: String }, shipping :: { address :: String }, payment :: { card :: String } ) checkoutStep`);
  - an action's outcome, where no status or fold fixes it (`# action @( "Flight booked" :: …, "Booking rejected" :: String ) submit`, flight-booker);
  - a projection's element row
    (`# foreach @"name" @( name :: String, mix :: … ) (const palette)`,
    `# foreach @"number" @( number :: Int, tenths :: Int ) lapRows`);
  - a seed with no loop of its own (`# with @{ "Guests" :: Array { … } } invitation`);
  - an option list, closed by `<+>` (`choice @"one-way" <+> choice @"return"`).

  What no line declares is not in the model. State only the business
  needs is derived (inbox computes the next message id from
  `messages`) or declared like any other field (circle-drawer's undo
  stacks are state, so they are on its seed line). Every typed hole then
  reports a concrete type with nothing unknown; the view model module's
  signatures are those types verbatim, and the compiler checks that they
  agree.
- **No nominal types in UI.** No `data`, `newtype` or `type` synonym for
  anything a component shows, emits or is configured with. A view-model
  row holds records, variants, `String`/`Number`/`Int` and `Array` —
  nothing else; `{}` for an empty payload, never `Unit`.
- **No `Maybe` in the model.** Name the two cases the business already
  uses: `selected :: [ picked :: { index :: Int }, none :: {} ]`,
  `approval :: [ approved :: { attempt :: Int }, pending :: {} ]`.
- **No `Boolean` unless a Boolean editor edits it** (a `toggleSwitch`
  over `"Include a Teams link"`). A flag nobody edits is a phase with
  two states — `status :: [ active :: {}, completed :: {} ]`,
  `drag :: [ adjusting :: {}, settled :: {} ]` — and a styling test over
  it is a named predicate (`clWhen isCompleted "todo-done"`).
- **Nominal types live below the UI**: a recursive type (cells' formula
  AST) or an ecosystem API (`Aff`, `Either`), entering the UI only
  through business functions. The price is spelling a shared row out in
  every signature that uses it (flight-booker's itinerary); pay it — the
  shape is the interface.
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
  headlineSmall (text balanceLine)
  ```

  with `balanceLine :: { balance :: Number } -> String`. A
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
  first: in a block it is a line like any display (signup-form's
  `headline4 $ staticText "Create account"`); inside one element, where
  there is no block, it is `prefixed` over what it introduces
  (`label $ prefixed (staticText "°C ") $ input @"celsius" "text"`,
  temperature-converter-html). A **separator** is written the same way,
  leading over what it introduces (`divider $ slider @"Attendees" #
  inCaseAt @"chosen" @"Room"`, meeting-booker), and shows itself only
  between things: while something is rendered before it and what it
  introduces renders something — never above an absent pane or an empty
  list. A **constant** shows only through data — a sentence's glue,
  a pane's message — and lives in a copy function: `text faultLine #
  pane @"faulty" readout` (calculator), `# tooltipWith loyaltyNote`
  (espresso-bar). Text that *is* data (a parsed markdown run) is
  `staticText` too.
- **A label is read back, never restated.** A case label is the copy it
  draws, so write it as the exact copy the line needs
  (`choice @"with oat milk"`, `choice @"cash"`) and read it back with
  `caseText` from `Data.Variant.Case` (order-form's
  `payingLine r = "Paying by " <> caseText r."Method"`). A `match` that
  only echoes its case labels is the label restated. A map that does
  real work — shortening, glyphs, per-case sentences — stays a named
  copy function. An affix shared by every case is not part of the copy:
  put it in the caption (`@"Duration (min)"` over `choice @"15"`) or in
  the line's glue.
- **Events carry data, never UI copy.** An emission carries the order,
  the outcome, the reason; the status's copy function turns it into a
  sentence.

### Business functions

- **The signature is the hint, verbatim.** Every business function is
  typed exactly as the view reports it: the row the stage is fed, closed
  (`countLine :: { counted :: Int } -> String`). Never narrowed to the
  fields the body reads, never opened with a tail, never coerced at the
  call site. Which fields the body uses is the body's business; the
  compiler checks the body against the row it is given. A name two lines
  call at different rows gets two hints, and that is the one case for a
  `forall`: generalize to the fields both rows share, with a tail.
  Private helpers no view line types are free to be as general as they
  like (flight-booker's `parse`, `formatDate`).
- **Update the row you are given.** A function returning the row it
  takes — a handler, a `settled` normalizer, a periodic step — updates
  it (`increment m = m { counted = m.counted + 1 }`), never builds a
  literal.
- **A preset is a field update**, even one that reads nothing:
  `beginTiming { model: sw } = sw { phase = .timing {} }` (stopwatch),
  `unshown @"Reset" # fold restarted` (timer). A constant
  replaces the model only as a whole, by a reset: `unshown @"New
  game" # reset openingPosition` (tic-tac-toe).
- **One record per business function.** Records that travel together
  are one row; let field names carry the roles positional arguments
  lose. A fold handler is no exception: a joined event arrives as one
  record, the payload under `event` and the model under `model`:

  ```purescript
  unshown @"Todo toggled" # fold toggleTodo
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
  created" personCreatedLine # fold identity`; a constant replacing the
  model is `unshown @"New game" # reset openingPosition`. A status's copy function takes the event's
  payload, never the handler's result.
- **A status only after a `+→+` block.** A fold opens with a visible
  status where a `+→+` block returned what the status tells, an
  action's outcome (crud's `snackbar @"Person created"
  personCreatedLine # fold identity`). A command straight from an
  emitter folds silently, whatever it does, since the screen already
  shows its result (counter's count, cashbox's till, inbox's list):
  `unshown @"Todo toggled" # fold toggleTodo`.
- **A handler carries no field it does not touch.** Group buttons into
  stages by the fields their handlers touch (circle-drawer keeps undo and
  redo apart from the canvas click). An identity handler means the
  event was never model data: show it with a display stage instead.
- **Lossy conversions live in the model.** An editor round-trips its
  field; a normalization that loses information is `# settled` on the
  stage (cells' formula field `# settled commit`), never hidden inside
  the component.
- **No dispatcher.** A function that only `match`es event cases has no
  place: each case is its own `status @l line # fold f` line, `f` and
  `line` named business functions in the view model module.

### Wiring

- **Speak the vocabulary; never import the ecosystem's
  `Data.Profunctor`.** The merges and the knot you import from
  `Data.Profunctor.Row.*` are vocabulary; raw `lcmap`/`rmap`/`dimap`
  are not. Every reshaping an app needs has a home in a word's own
  argument — `foreach @l rowsOf`, `toCase @l payloadOf`,
  `settled normalize`, with `identity`
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

**The view determines the view model.** Every view line names what it
needs and states the piece of the model it binds, so a view written
with a typed hole for every value it would import compiles to a list of
those values with their types: the view model module's signatures, with
nothing unknown. Write the view first and the view model
module to that list. Until it exists a **hole** stands in for each
missing value, and there are two:

| Hole | Written | Compiles? | Gives you |
| --- | --- | --- | --- |
| typed hole (the compiler's) | `?countLine` — any name after `?` | **no**, by design | the value's full inferred type, reported at the hole |
| runtime hole | `hole` (`import PUI.Web (hole)`), a value of every type | **yes** | a view that builds and runs; it throws only when data reaches it |

Use `?name` to learn what a missing value must be and `hole` to see the
view run before it exists. One `?name` fails the build, so to run the
view, turn every remaining `?name` into `hole` or its definition. With
the watch build running ([building.md](building.md)):

1. **Write the view**, each line naming its anchors, the seed line
   declaring the model row and each derived row declared where it is
   introduced ([Types and values](#types-and-values)). Labels and
   declared rows never need a hole, because writing them
   is writing the model. Every other value — a copy function, a
   handler, a classifier, an action, the seed — starts as a typed hole
   (`text ?countLine`, `# looped @( counted :: Int ) # with ?start`).
2. **Read the holes.** Every hole reports a concrete type built from
   the pieces the lines state. With several holes, a last message lists
   them all, placed on the declaration's name: the view model module's
   signatures, ready to write down. Three of inbox's 17, abridged and
   joined onto one line each:

   ```
   openMessage  :: Int -> { deletion :: …, messages :: Array { … } } -> { deletion :: …, messages :: Array { … } }
   highlighted  :: { body :: String, id :: Int, sender :: String, status :: …, subject :: String } -> Boolean
   mondayMail   :: { deletion :: …, messages :: Array { … } }
   ```

   Nothing is unknown. An unknown anywhere (`Record t1`,
   `[ reading :: … | t2 ]`, a tail `| t0`) names a missing declaration:
   the model row where the model first appears, or a derived row where
   it is introduced. Each `?name` is its own hole, so a function used on
   several lines (`bookingState` on three panes) is declared on its
   first; once written, the compiler types its other uses.
3. **Write the signatures down, verbatim.** The hint is the signature:
   the row the stage is fed, closed, its fields in the compiler's order
   (`highlighted :: { body :: String, id :: Int, sender :: String, status :: …, subject :: String } -> Boolean`
   over the list's element row). Do not narrow it to the fields the body
   reads and do not open it with a tail: which fields the body uses is
   the body's business, and the compiler checks the body against the
   row it is given. A name reported at two different rows, because two
   lines call it, is the one case for a `forall`: generalize to the
   fields both rows share, with a tail. The hole's message goes away,
   and from then on the compiler checks the function against the
   declared rows.
4. **Fill the holes in any order.** The compiler reports one
   declaration's holes per build, and an app is one pipeline in one
   declaration, so its holes report together; a view helper's holes
   (markdown-previewer's `documentView`) surface after the entry's. A
   helper applies one view model function per value — composing two
   (`rgb (mixOf channels)`) hides the type between them, so the view
   model exports the composite (color-mixer's `mixedColor`). The seed's
   signature is the model row its line declares, and its value is one
   business-named record (`freshCount`, `mondayMail`).
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
- **A change arrives whole.** A change to several fields renders once,
  every field fresh — never a half-updated row. Business functions read
  consistent state.
- **Showing state never fires an event.** Feeding a button, a list or a
  selector the model does not make it emit; only the user does. So the
  loop can feed its emitters freely, and settles instead of spinning.
- **Waiting is safe.** Putting a stage that holds things back —
  `confirmed`, `acted`, a debounce — anywhere only delays what flows; it
  never produces a new or inconsistent value.
- **A blank pane is a missing seed.** A merge that shows nothing is
  waiting for a field nobody has given a value; the watchdog names it
  (below).
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
| the model row against `()`, `while matching label "Count"` | the status or fold of that case | a display stands as a line of its own before the emitter, so it passes on `{}` and the emitter replays it | make it `prefixed (…) $` over the line after it (counter) |
| `()` against `( event :: …, model :: … )`, `while matching label "Start"` | the fold of that case | an emitter inside a pane lacks `# joined @"Start"`, so it replays the pane's payload instead of the model | add `# joined` to the emitter (stopwatch) |
| `( "Cout" :: … )` against `( "Count" :: … )` | the fold | one case spelled two ways | spell the case the same on the emitter and its fold |
| `( "Reset" :: … )` against `( … )` | the emitter | no fold handles the case | add its fold to the fold block, silent if nothing needs telling (timer's `unshown @"Clock ticked" # fold tick`), or remove the emitter |
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
