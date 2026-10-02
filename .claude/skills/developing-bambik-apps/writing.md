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
unsuffixed directory (`inbox/InboxViewModel.purs`; demos not yet swept
still name theirs `<Demo>Logic`, as `counter/CounterLogic.purs`), so read
whichever twin matches your design system.

## Terms

- **record** `{ … }` (written ×) — knowledge: everything at once.
  **variant** `[ … ]` (written +) — an event: one case at a time.
- **shape** — what a component takes in and gives out, record or
  variant on each side. There are exactly four, and every component has
  exactly one:

  | Shape | Kind | Examples |
  | --- | --- | --- |
  | ×→× | **editor**, **display**, **stage** | `filledTextField @"Email" {}`, `text countLine # shown` |
  | ×→+ | **emitter** | `button @"Count" {}` |
  | +→× | **status** | `snackbar @"booked" bookedLine` |
  | +→+ | **handler** | a backend action |

- **stage** — one step of the pipeline: its output is the next step's
  input.
- **merge** — several components over one shared value, written as a
  qualified `do` block (below).
- **seed** — the model's value at start, on the line that declares the
  model row (`# mvu @( count :: Int ) freshCount`). A pane
  stays blank until the fields it waits for have values; a seed gives
  them one.
- **pane** — a component that exists only while the model is in one
  case (`# shownWhen @"estimated" distanceOf`).
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
order. Four more qualified `do` blocks put several components over one
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
label is the copy it draws.** `filledTextField @"First name" {}`
captions itself "First name" and edits the field `"First name"`;
`button @"Submit order" {}` draws "Submit order" and emits the case
`"Submit order"`. Labels are human copy, so they are usually quoted,
and the model's rows carry the same quoted labels
(`{ "First name" :: String }`). A quoted label cannot be a record pun,
so bind explicitly:

```purescript
createPerson { "Name": name, "Surname": surname, people } = …
```

Field access (`r."Name"`), accessor sections (`_."Name"`) and update
syntax (`r { "Name" = … }`) work as usual. Put punctuation and units on
the label (`filledTextField @"Start date (DD.MM.YYYY)" {}`,
`sliderLive @"Amount (€)" {}`); where a symbol is the conventional
caption, write the symbol (`@"°C"`). A caption config (`floatingLabel:`,
`label:`) is only for copy the label cannot be — localized wording,
passed from the app's copy table — and, on a button, hiding the caption
of a glyph-only face (see the component's header).

How each kind takes its business meaning:

- **Editors** edit the field their label names, and each one is a whole
  pipeline stage: fed the whole row, it edits its field and passes the
  rest on. A form is editors written as successive lines, never merge
  operands. Two controls writing one field are two lines in a row
  (tip-calculator's slider and range input). Editors live inside a loop
  — `mvu`, `looped` or `bracketed` — so every editor sees its siblings'
  latest values; a flow without a loop of its own wraps its form in
  `# looped` (order-form).
- **Selectors** are editors, and the word depends on what the model
  holds. The plain word when the field always holds an option; the
  `…Unpicked` word when a choice is owed but not yet made; the
  `…Optional` word when the user may leave it unmade. Options are
  `choice @"…"` values in the order written (meeting-booker:
  `dropdownUnpicked @"Room" @"chosen" {} (choice @"Focus pod (4 seats)" <+> …)`).
- **Displays** take a copy function, not a label:
  `headline4 (text countLine) # shown` (counter). A number shown as a
  bar or gauge takes a function too, and keeps its label only as the
  accessible name (`linearProgress @"Elapsed" elapsedFraction`, timer).
  See *Copy is a function* in [Types and values](#types-and-values).
- **Emitters** emit their own case. Whatever the business decides about
  it is decided where the case is consumed — the fold's handler, the
  status's copy function. When two buttons feed one loop case, keep two
  business cases and introduce the loop case from each (checkout's
  `button @"Next" {} # toCase @"next" goneOn`).
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
packages itself** (order-dashboard's `statTile @"Orders placed"
ordersCount`). A signature with a bare `String`, `Maybe a` or `a` on
either side is a smell: that value belongs in a field of the row, read
by a business function.

Component configs are records whose field names belong to each
component (`filledTextField`'s `floatingLabel`, `button`'s `icon`), so
copy a demo's call or read the header instead of guessing — a guessed
field fails as `TypesDoNotUnify` on the config record.

## Stages

An editor is a stage as it stands. Everything else becomes a stage with
a trailing word that says what it is for:

| The line | Write | Demo |
| --- | --- | --- |
| a display or chrome, always there | `(headlineSmall $ text orderLine) # shown` | order-form |
| a display shown in one case | `text distanceLine # shownWhen @"estimated" distanceOf` | order-form |
| an editor that exists in one case | `filledTextField @"Table" {} # inCase @"Dine in" selection` | order-form |
| a button that exists in one case | `button @"Start" {…} # provided @"halted" _.phase` | stopwatch |
| a list rendered from the row | `ul $ (li $ text lapLine) # shownEach @"number" lapRows` | stopwatch |
| content that waits for the user to confirm | `confirmed @"Refund" @"Refund the customer?" $ …` | cashbox |
| a button stepping the model | `button @"Count" {} # applied increment` | counter |
| events folded into the model | `# updated (match { "Start": const beginTiming, … })` | stopwatch |
| an invariant between edited fields | `filledTextField @"°C" {} # settled fromCelsius` | temperature-converter |
| a periodic step | `every tickPeriod tick` | stopwatch, timer |
| buttons replaying the row they are fed | `(RecordToVariant.do …) # armed` | order-form |
| an effect run on a button's case | `indeterminateLinearProgress @"Submitting order" # action submitOrder # atCase @"Submit order"` | order-form |
| an effect with no progress indicator | `blank # action rotateAction # atCase @"Rotate"` | reorder |

Content inside `shown`, the panes and `confirmed` must output `{}`. An
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

The pipeline ends with its seed: `# mvu seed` for a model that loops
through its own editors and buttons, `# with seed` for a flow with no
loop of its own. Both close the app to what `body` accepts; a forgotten
seed is a compile error at `body` naming the missing fields.

| Shape | Demos |
| --- | --- |
| the smallest model-view-update loop | counter |
| a load action feeding a looped form, events, actions, statuses | order-form (all four shapes), crud |
| a loop plus fixed payloads | cashbox, inbox, tic-tac-toe, shopping-cart |
| a fixed grid or canvas fed as data, updated in place | cells, circle-drawer, tic-tac-toe, calculator |
| collections | todo-list, shopping-cart, reorder, potluck |
| panes over one classifier | quiz, checkout |
| effects and time | password-generator, stopwatch, timer, weather |
| structure that varies with the data | markdown-previewer |
| no design system at all | restaurant-menu, helloworld |
| one state-loop each | auction (`feedback`), checkout (`folding`), payment (`iterate`), ticket-dispenser (`unfolding`) |
| a reusable sub-form; routing some events | parcel (`subStrong`), cashbox (`subChoice`) |
| keyed event streams | departures (`dispatched`), scoreboard (`accumulated`) |

## Conditional visibility

A component that exists only sometimes is a **pane over a case**, never
a predicate in the view and never a `Maybe`. The argument is a business
function that classifies the model into a variant; the pane exists while
the variant is at the named case and is given that case's payload:
`shownWhen` for a display, `inCase` for an editor, `provided` for an
emitter.

When the state is stored in the model as a variant, the pane reads the
field with an accessor (inbox's `# provided @"confirming" _.deletion`,
flight-booker's `# inCase @"return" _."Flight type"`), typed by the
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
flight-booker's return date (`# inCase @"return" _."Flight type"`) and
meeting-booker's attendees slider (`# inCase @"chosen" _."Room"`) are the
examples; a variant field edited through several such panes is wrapped
in `# bracketed @"Mode" …` (order-form).

`clWhen` toggles a class for styling, not existence.

## Modals

A dialog opens when it is fed and closes when one of its buttons emits.
Feed it only in the state that asks for it and put the deciding buttons
inside: inbox's `dialog @"Delete the last message?" $ RecordToVariant.do
…` under `# provided @"confirming" _.deletion`. For a confirmation step
inside a flow, `confirmed` (cashbox).

A drawer's navigation is the first stage and its content the second, so
the pick reaches the content directly (photo-gallery).

## Collections

- **Keyed and kept.** `foreach @"key" rowsOf` and `listOf @l @k` render
  one element per row, identified by a key field of the row. An element
  whose key survives a change is updated in place and keeps its focus
  and local state; a reordered list moves elements with their keys. Rows
  therefore need an id field — an array of bare strings cannot be
  edited in place.
- **Fixed structure** (grids, canvases): feed the structure as data
  through `foreach` and compute each element's attributes with
  `attrWith` (cells, circle-drawer). **Structure that varies with the
  data** (markdown blocks): `dynamic`/`each`, which rebuild per value
  (markdown-previewer).
- **Editing in place**: `edited @"id"` folds each element's edit back
  into the array (reorder). An element cannot change its own key. Add,
  remove and reorder are sibling stages over the enclosing model, not
  part of the element.
- **Joint choice**: `acted @"name"` gathers every element's choice
  before emitting (potluck).
- A fixed catalogue is given through the word's own projection
  argument, `# foreach @"key" (const keyPad)` (calculator).
- An element whose content reads several things names that reading
  once, in a face function over an open row
  (`attrWith "style" cellFace`, cells).

## View module and view model module

Every function belongs to one of two classes, in two modules with a
one-way dependency:

- **The view module** (`<App>.purs`) exports the one entry function and
  keeps UI-wiring functions that span several lines (a `dynamic`/`each`
  builder, a reusable sub-form like parcel's `addressForm`). It imports
  the design system, the library's words and the view model module.
- **The view model module** (`<App>ViewModel.purs`) exports business
  functions and named business values: seed models, tick periods, fixed
  payloads, copy functions, parsers, `Aff` actions. It imports only the
  domain — `Prelude`, plain data modules, `Effect`, `Aff`,
  `Data.Variant.Case` — never `PUI`, `PUI.Web.*`, a design-system module
  or the merges. The one exception is temporary: a stub `hole` while
  the view model is not written ([Writing order](#writing-order)).

**The view determines the view model module.** A value lives there
exactly when a view line calls it, and its signature is what that line
demands: compiled with a typed hole for each such value, the view
reports every one at a concrete type, and the module's signatures
restate that list with the footprints narrowed
([Writing order](#writing-order)). Nothing is designed into the view
model module that no view line asks for.

The view hands the view model module **only arguments called on
data**: a copy function, a handler, a classifier, an action, the seed,
a period. It never runs one of its effects at the entry and never
applies a component built there: an optic the view uses is assembled on
the view line from view model functions (ticket-dispenser's `reelE issue
nextTicket identity`). A stand-in server keeps its state in the view
model module, as a real server would (crud's catalogue).

**Name each action's outcome cases where the action is**: a
single-outcome action's line names its case
(`… # action createPerson # atCase @"Create" # toCase @"created" identity`,
crud); a multi-outcome action is followed directly by its own statuses
(order-form's submit). Do not merge two actions before their outcomes
are named.

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
  name" {}`, `group @"Customer" $ …`);
- a **case** — the type argument of an emitter, pane or status
  (`button @"Submit order" {}`, `# shownWhen @"estimated" distanceOf`,
  `snackbar @"booked" bookedLine`);
- a **copy function or accessor** — the positional argument of a
  display (`text balanceLine`, `text _.title`, `imagePane developedShot`);
- **nothing** — chrome (`card`, `h1 >>> cl "restaurant-name" $ staticText
  @"Osteria Yoneda"`, `topAppBar @"Espresso Bar"`). A static's type argument is its own
  text, not an anchor: it needs no data to be seen.

A **declared row** is not an anchor either. It is a visible type
argument stating a shape the compiler could not otherwise know: the
model row on the seed line, and a derived row where it is introduced
(see *The model is declared once* in
[Types and values](#types-and-values)). It names no field and no case.

So every leaf reads as a noun phrase — word, anchor, positional
arguments:

```purescript
snackbar  @"booked"  bookedLine              -- the booked snackbar, saying bookedLine
confirmed @"Refund"  @"Refund the customer?"  -- the title is static: a type
button    @"Sign up" { icon: "person_add" }  -- the record: optional presentation
```

**A record never holds an anchor or a required value.** A record stays
only for optional presentation (`{ icon }`, `{ floatingLabel }`) or for
two same-typed values a positional pair could swap
(`drawer @( title :: "Darkroom", subtitle :: "photos drawn on the spot" )`).
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
  concerns, via `#`.** No data word leads a line; an emitter's fixed
  payload trails too (`button @"Take a deposit" { icon: "savings" } # with
  customerDeposit`, cashbox), and
  `# with {}` is written inline.
- **A decorator rides its element.** `cl`, `clWhen`, `attrWith`,
  `tooltip` and `tooltipWith` compose onto a container with `>>>`
  (`td >>> attrWith "style" cellFace $ …`) or trail a finished leaf with
  `#` (`span (text _.title) # clWhen isCompleted "todo-done"`,
  `checkbox @"Loyalty" … # tooltipWith loyaltyNote`); never lead with
  one.
- **Closing parens and `#` chains never start a line.** A trailing
  chain stays on one line at the end of the component's last line, and
  nested closers cascade onto that same line, each spaced from the chain
  it closes over: `… # shown ) # feedback @"top" @Number noBids`. The
  exception is a seed closer, `) # mvu @( … ) seed` / `) # with @( … ) seed`,
  on its own line — or, when the model row is long, `) # mvu` on its own
  line, the row's fields one per line beneath it and the seed last
  (inbox).
  `#` binds tighter than `$`: where a chain must apply to a whole
  wrapped element (a `foreach` multiplying a card), open the paren
  before the wrapper.
- **Two-space indentation.** A block's lines sit two columns deeper
  than its opener, `( Semigroupoid.do` included; a closer returns to its
  opener's column. No four-space steps, no alignment to a token
  mid-line.
- **The architecture reads off the pipeline**, in this order: load →
  form (×→×) → live summary → events (×→+) → each action with its
  statuses (+→+ then +→×), closed by the seed. No registries, config
  objects or reflective assembly.

### Types and values

- **The model is declared once, on the seed line; every derived row
  where it is introduced.** The seed line states the whole model
  (`# mvu @( count :: Int ) freshCount`; inbox's two fields, one per
  line), and every editor, selector, list and accessor is checked
  against it — so a stored field is read with a plain accessor
  (`listOf … _.messages`, `# provided @"confirming" _.deletion`). A
  **derived row** is a shape no model field holds, and the line that
  introduces it declares it after its anchor:
  - a classifier's cases, on its first pane
    (`# shownWhen @"cart" @( cart :: { item :: String }, shipping :: { address :: String }, payment :: { card :: String } ) checkoutStep`);
  - an action's outcome (`# action @[ generated :: String ] samplePassword`);
  - a projection's element row
    (`# foreach @"name" @( name :: String, mix :: … ) (const palette)`,
    `# shownEach @"number" @( number :: Int, tenths :: Int ) lapRows`);
  - a fixed payload (`# with @( amount :: Number ) courierFee`);
  - a trace form's state (`# feedback @"top" @Number noBids`,
    `# folding @"next" @"step" @[ cart :: {}, shipping :: {}, payment :: {} ] cartStep`),
    a bracketed editor's state, a Reel's two types;
  - an option list, closed by `<+>` (`choice @"one-way" <+> choice @"return"`).

  What no line declares is not in the model. State only the business
  needs is derived (inbox computes the next message id from
  `messages`) or declared like any other field (circle-drawer's undo
  stacks are state, so they are on its seed line). Every typed hole then
  reports a concrete type with nothing unknown; the view model module's
  signatures restate them with footprints narrowed to open rows, and the
  compiler checks that they agree.
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
  and functions, never on types (`mvu plannedTrip`, `with emptyCanvas`).
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
  unit-testable (`countLine { count: 3 } == "3"`). A whole line is one
  function, glue included — never several leaves with `staticText`
  between them, never a formatter in the view:

  ```purescript
  headlineSmall (text balanceLine) # shown
  ```

  with `balanceLine :: forall r. { balance :: Number | r } -> String`. A
  display that shows one field verbatim takes the accessor
  (`text _.title`). A number drawn as a bar is a function too — a
  fraction is derived from state, like a sentence. A model field exists
  because the app's state needs it, never because a display wanted a
  `String`.
- **Fixed copy is static or constant.** A **static** is on screen before
  and regardless of any data — a heading, a note, a checkbox's caption —
  and is a type: `staticText @"Hours"`, `tooltip @"…"`. A **constant**
  shows only through data — a sentence's glue, a pane's message — and
  lives in a copy function: `text faultLine # shownWhen @"faulty"
  readout` (calculator), `# tooltipWith loyaltyNote` (espresso-bar).
  Text that *is* data (a parsed markdown run) is `staticString`.
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

- **Footprints as open rows.** Every business function states what it
  reads and writes as an open row:
  `countLine :: forall r. { count :: Int | r } -> String`. Never the
  whole model, never a closed row, never a coercion at the call site.
  The compiler checks the footprint against the row the stage is fed,
  and the function touches exactly the fields it names.
- **Update the row you are given.** A function returning the row it
  takes — a handler, a `settled` normalizer, a periodic step — updates
  it (`increment m = m { count = m.count + 1 }`), never builds a
  literal.
- **A preset is a field update**, even one that reads nothing:
  `beginTiming sw = sw { phase = .timing {} }` (stopwatch),
  `button @"Reset" {} # applied restarted` (timer). A constant replaces
  the model only as a whole: `button @"New game" {…} # with
  openingPosition # updated (match { "New game": const })`
  (tic-tac-toe).
- **One record per business function.** Records that travel together
  are one row; let field names carry the roles positional arguments
  lose. The exception is a fold handler, which takes the event's payload
  and the state separately, each an open row:

  ```purescript
  # updated (match { refunded: applyRefund })
  applyRefund :: forall r1 r2. { amount :: Number | r1 } -> { balance :: Number | r2 } -> { balance :: Number | r2 }
  applyRefund { amount } till = till { balance = till.balance - amount }
  ```

  A button with no payload of its own, stepping the model it is fed, is
  `# applied f` (`button @"Add" {} # applied addTodo`); inside a
  `match`, `const f` ignores the payload (`"Start": const beginTiming`)
  and `const <<< f` applies `f` to it (espresso-bar's
  `"The usual": const <<< theUsual`). Scalar and array payloads (a key,
  a fetched list) are positional.
- **A handler carries no field it does not touch.** Group buttons into
  stages by the fields their handlers touch (circle-drawer keeps undo and
  redo apart from the canvas click). An identity handler means the
  event was never model data: show it with a display stage instead.
- **Lossy conversions live in the model.** An editor round-trips its
  field; a normalization that loses information is `# settled` on the
  stage (cells' formula field `# settled commit`), never hidden inside
  the component.
- **Inline a dispatcher.** A named function that only `match`es cases is
  written inline at the `updated` stage, each branch a named business
  function in the view model module.

### Wiring

- **Speak the vocabulary; never import the ecosystem's
  `Data.Profunctor`.** The merges and state-loops you import from
  `Data.Profunctor.Row.*` are vocabulary; raw `lcmap`/`rmap`/`dimap`
  are not. Every reshaping an app needs has a home in a word's own
  argument — `foreach @l rowsOf`, `toCase @l payloadOf`,
  `settled normalize`, `bracketed @l stateOf caseOf`, with `identity`
  meaning "the value as it is". A shape none fits is a gap to report,
  not a reason to reach below the vocabulary.
- **State lives in the model or in the state-loops.** No FFI stashes,
  no module-level mutable references, no reading the DOM back, no window
  globals. A component's private state is threaded by `feedback`,
  `folding` or `unfolding`.
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
   introduced ([Types and values](#types-and-values)). Labels,
   accessors and declared rows never need a hole, because writing them
   is writing the model. Every other value — a copy function, a
   handler, a classifier, an action, the seed — starts as a typed hole
   (`text ?countLine`, `# mvu @( count :: Int ) ?start`).
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
   the model row on the seed line, or a derived row where it is
   introduced. Each `?name` is its own hole, so a function used on
   several lines (`bookingState` on three panes) is declared on its
   first; once written, the compiler types its other uses.
3. **Decide the footprint.** A reported type takes the whole model,
   not what the function needs. Keep the fields the function reads and
   writes, open the row with a tail, and write it in the view model
   module (`sortBySender :: forall r. { messages :: Array { … } | r } -> { messages :: Array { … } | r }`;
   `highlighted :: forall r. { status :: … | r } -> Boolean` over the
   list's element row). The hole's message goes away, and from then on
   the compiler checks the footprint against the declared rows.
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
   with `hole` — inline (`text hole`, `# mvu hole`) or as view model exports
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
  you wrote (`mvu`, `looped`).
- **A change arrives whole.** A change to several fields renders once,
  every field fresh — never a half-updated row. Business functions read
  consistent state.
- **Showing state never fires an event.** Feeding a button, a list or a
  selector the model does not make it emit; only the user does. So
  `updated`, `applied` and `mvu` can feed emitters freely, and a loop
  settles instead of spinning.
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
