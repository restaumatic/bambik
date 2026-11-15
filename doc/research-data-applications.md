# Research: applications over data (branch model-data-manipulation, 2026-11-14)

**Status.** An experiment in the worktree, uncommitted, on top of the
pilot that was already there (the view ending at its events, the folds,
knot and seed in the app module — `edits`, `told`). Green: `spago build`,
`spago test`, `npm run bundle-demos`, `npm run smoke`, `npm run
check-view-model`, `npm run check-determined`. Not synced: writing.md,
guardrails, CLAUDE.md and the CHANGELOG say nothing of it yet
([Not synced](#not-synced)).

## The question

Take the applications whose whole state is **data**: one value in
memory, built from records and variants with primitives at the leaves
and keyed arrays between them, which the screen reads and writes. How
does such an application sit in the four row shapes and the keyed
square?

## The answer

The data's constructors are the shapes' tensors, so every position in
the data already has a kind, and a UI over the data is typed by two
decisions the shapes were made to state:

| | it writes **the next state** (`×` out) | it writes **an operation** (`+` out) |
| --- | --- | --- |
| it reads **the state** (`×` in) | `×→×` — in place: editors, `group @l`, `acted`, `actedAt`, `actedWithin` | `×→+` — commands: emitters, `gathered…`, `listOf`, `edits @l` |
| it reads **a change** (`+` in) | `+→×` — a change applied: `fold`, `capture`, `accumulated…` | `+→+` — a change interpreted: `action`, `confirmed`, `dispatched…` |

The top row is the UI: it is fed the data whole and answers either with
the data edited or with what the user did. The bottom row is where a
change lands — the UI's own operations, folded by the data's
transitions, or changes that arrive from outside, one entity at a time
(departures, scoreboard). The keyed square is the same table one level
down, a table's rows where the row shapes have a document's fields.

Everything else in this note is that table read in one direction:

1. **Reading is a function.** The data is fed whole; each line reads
   what it wants. A query is a function of the data, nothing more.
2. **Writing in place is an optic.** The editor composes along the
   data's own structure, and the way back is carried inside the
   composition.
3. **Writing by operation is an emitter and a fold.** The way back is
   carried by the event, and `joined` is what puts it there.
4. **The two are one thing at two times.** An edit in place is the
   operation that carries the next state, folded by `capture`
   ([the law](#the-law-between-them)).
5. **Between a derived view and a stored array there was a missing
   rung**, the selection written back by key. It is one lens setter
   away from the two forms that existed, and it is now a word:
   `actedWithin` ([below](#the-view-update-problem-and-actedwithin)).

## The data and its constructors

```
data  ::=  primitive  |  { l :: data, … }  |  [ c :: data, … ]  |  Array { k :: key | row }
```

| Constructor | Tensor | Reads | Written in place | Written by operation |
| --- | --- | --- | --- | --- |
| a primitive in a field | — | `textAt @l`, a copy function | the leaf editor `filledTextField @l` | `# edits @l`, then `capture` |
| a record in a field | `×` | a copy function reading deeper | `group @l form` — the field lens | — |
| a variant in a field, cases bare | `+` | `inCaseAt @c @l`, `caseText` | a selector, `segmentedButton @l` over option leaves | an emitter and a fold |
| a variant in a field, cases with payloads | `+` | `inCaseAt @c @l`, a classifier, `match` in a copy function | **not as a sum** ([A variant in data](#a-variant-in-data)) | a transition: the fold moves the case and supplies the payload |
| a keyed array in a field | the `×`-power over the keys | `actedOf @k query`, `actedAt @k @t` | `acted @k`, `actedAt @k @t`, `actedWithin @k @t select` | `gatheredOf`/`listOf` picks, row buttons, `edits` in a row |
| one row of it | the `+`-power | — | — | fed one at a time: `dispatchedOf`, `accumulatedOf` |

Only `actedWithin` is new in this table. Its point is that it has no
empty row: a data type is read off as a UI line by line,
its labels the captions (L3), which is the converse of L18 — there the
view fixes the model up to its tails, here a given data type fixes the
default view. venue is that reading done once, as deep as its data
nests: a record of a primitive, a choice, a sub-record, an array of
records each holding a choice with its fields, and a sub-record holding
a choice and an array.

## Reading is a function

Every reading word takes a function of the row it is fed, so a query
is written where queries belong, in the app module, and tested there:

| What the screen shows | The query | The word |
| --- | --- | --- |
| a sentence | `tableLine :: Diner -> String` | `text tableLine` |
| the rows that qualify | `menuCard :: Diner -> Array Dish` (dishes with portions left) | `listOf … menuCard` |
| rows joined across two arrays | `tableLines :: Diner -> Array TableLine` (a line with its dish's price) | `# gatheredOf @"dish" tableLines` |
| an aggregate | `takingsLine`, `bills` (each ready order with its total) | `text`, `# gatheredOf @"number" bills` |
| which state the data is in | `tableState :: Diner -> [ idle :: {}, taking :: {} ]` | `# provided @"taking" tableState` |

No optic is involved in reading: the path into the data is function
composition, and a join is a `find`. What the words add is identity —
rows keep their elements across feeds by their key — and existence, a
pane over a classifier.

## Writing in place: the UI is an optic

An editor lifted by `focusField @l` is a whole-row citizen: it retains
the row it was fed and re-attaches it to every edit. `group @l` is the
same lens over a sub-record's form, `acted @k` the container action
over a keyed array. They compose as the data nests, and the composite
is the path:

```purescript
group @"Delivery" $ Semigroupoid.do
  segmentedButton @"Service" $ …
  ( list $
    ( listItem $ Semigroupoid.do
      labelLarge $ text zoneLine
      sliderLive' @"Fee" { unit: "€" } ) # actedAt @"zone" @"zones" ) # inCaseAt @"Own couriers" @"Service"
```

A fee dragged in the third zone leaves as the whole venue: the slider
re-attaches its zone row, `actedAt` its array and the `Delivery`
record, `group` the venue. Nothing names the path on the way back,
because each level kept its context — `Strong`'s state channel, the
container action's retained array.

A UI built only this way is `PUI m Data Data`, and its context is the
bare knot: `# looped # with trattoria` (venue; parcel and movie-browser
are the same shape). No event, no fold.

In place reaches exactly as far as the data's **shape stays**: the
same fields, the same case, the same keys. Three things are outside it
by construction, and each is an operation: a row added, removed or
moved; a case changed to one that carries a payload; and anything the
screen shows that the data does not store as shown.

## Writing by operation: the emitter and the fold

An emitter answers with what the user did, the fold says what that
does to the data. A fold is a **transaction**: a pure function
`payload -> data -> data`, releasing the whole next state at once — a
feed is one step, so a transaction that takes a portion off a dish and
puts a line on an order is never seen half done (tested on diner's
context, without a browser).

The context the lens kept inside has to travel in the event instead,
and `joined` is the word that puts it there: it is `first` around its
source, the fed row leaving beside the payload. One `joined` per
level is the path, written out:

```purescript
( div $
  ( card $ Semigroupoid.do
    titleMedium $ text ticketLine
    RecordToVariant.do
      ( listOfAt' @"Line plated" @"dish" @"lines" { selected: isPlated } $ text itemLine ) # joined @"Line plated"
      button' @"Order ready" { icon: "room_service" } # provided @"plated" ticketState ) # gatheredOf @"number" cooking ) # joined @("Line plated" + "Order ready")
```

```purescript
plateLine :: { event :: { event :: String, model :: Order }, model :: Diner } -> Diner
```

The line's key, the order it is in, the data it lands on: the fold's
argument is the zipper the lens composition would have held. So the
two disciplines differ in **who holds the path** — the composition, or
the event — and in nothing else. Where the nesting of `{ event, model
}` grows unwelcome, the query materializes the path instead and the
row carries its parents' keys (L9: identity is data); diner's `Bill`
carries its order's number that way.

### The law between them

```
edits @l e >>> capture @l  ⊑  e
```

An editor `e` and the pair on the left emit the same on every edit of
the user, each the next state; `e` also answers every feed, the pair
answers none (test/Main.purs, *In place is the operation that carries
the next state*). In place is the special case of an operation whose
payload is the whole next state and whose fold is the identity —
state-based where a command is operation-based. This is why an app in
the context form ([below](#the-data-context)) can keep an in-place form
as one line: `( … ) # edits @"Contact edited"`, folded `capture
@"Contact edited"`.

## The view-update problem, and `actedWithin`

Model → view model is, for an editor, a lens: the getter is what the
screen reads, the setter what an edit means. Lenses were first proposed
as an answer to this question in databases — *given a view, what does
writing through it do to the base?* — and for a keyed collection the
vocabulary held two of its three classical answers:

| Word | Getter | Setter | The view |
| --- | --- | --- | --- |
| `actedOf @k query` | `query` | `const`: the fed value kept | read-only: a derived array has no base |
| `actedAt @k @t` | the field `t` | `t` replaced whole | the base table itself |
| **`actedWithin @k @t select`** | `select`, rows of `t` | each row landed at its key in `t` | a selection of the base: filtered, sorted, picked |
| `( row # edits @l ) # gatheredOf @k query`, `joined`, `fold @l put` | `query` | the app's `put`, as an operation per row | a view with its own trigger |

The three forms that write nothing or write canonically are **one lens
over `acted`**, told apart by their setter:

```purescript
actedOf     f      w = lens f      const  (acted @l w)
actedAt            w = prop (Proxy @t)    (acted @l w)
actedWithin select w = lens select landed (acted @l w)
```

`landed` replaces, in `t`, the rows carrying the released keys and
keeps the rest of the row fed. The word is derived (the ecosystem's
`lens`, nothing new in the carrier), its laws are in
`Data.Profunctor.Keyed.Acting` and value-tested on the probe carrier:

- **wire** — `actedWithin @l @t select identity ≈ identity`: GetPut,
  exact, since a selection never reshapes a row (the element is typed
  at the stored row).
- **landing** — a release replaces the row of its key and nothing
  else; the key is the fed one, unforgeable.
- **agreement** — `actedWithin @l @t (get t) ≈ actedAt @l @t`.

PutGet is the selection's to keep, and it is the classical caveat: an
edit that takes its row out of the selection lands, and the next feed
drops the element (a todo ticked under *Active* leaves the list). The
one precondition the type cannot state is that `select` picks rows
**of `t`**; a row it invents has no place to land and is not written.

What it bought, on three demos:

- **movie-browser**: favourites are toggled through the category and
  tag filter in place. Gone: the `Listing` projection row, the
  `"Favorite changed"` case, its `joined`, its fold, `markFavorite`,
  and the inner `# looped` around the filters. The app is `×→×` from
  top to bottom.
- **todo-list**: a todo is ticked through the visibility filter
  (`iconToggle @"Done"` in the row); `toggleTodo` and its case are
  gone, and `status :: [ active, completed ]` became the Boolean its
  editor edits.
- **contacts**: master and detail. The form **is** the picked row:
  `pickedContact :: Directory -> Array Contact` returns none or one, so
  the form exists while something is picked, edits the stored contact
  as it is typed (the list beside it reads the same array and follows),
  and a new pick is a new key, so a new element that holds nothing of
  the old one. `Maybe` needs no word here: at most one is an array.

  ```purescript
  div $
    ( Semigroupoid.do
      segmentedButton @"Kind" $ …
      filledTextField @"Name"
      filledTextField @"Phone"
      filledTextField @"Tax number" # inCaseAt @"Company" @"Kind" ) # actedWithin @"id" @"contacts" pickedContact # edits @"Contact edited"
  ```

### At most one is an array

Since 2026-10-13 a pane names its case and feeds its content the whole
row, so a read under a case finds the case again: inbox's reading pane
had four copy functions over one helper, `openMessageLine`, each with
a `maybe ""` for the case the pane had already excluded. A query that
returns none or one row, under a keyed word, is the pane that narrows,
for every kind of content:

| The content | The word | Demo |
| --- | --- | --- |
| displays | `actedOf @k oneOrNone` | scoreboard's summary line |
| displays and an emitter | `gatheredOf @k oneOrNone` | inbox's reading pane |
| editors | `actedWithin @k @t oneOrNone` | contacts' form |

The content is typed at the row and exists while the row does. inbox
now reads `headlineSmall $ textAt @"subject"`, `text fromLine` with
`fromLine :: Message -> String`, and its Delete button replays the
message it stands in, so `deleteMessage` deletes by id; the classifier
`messageView`, `subjectLine`, `bodyLine` and the `Maybe` helper are
gone. The difference from a pane is identity: a pane parks one content
and shows it again, a selection builds an element per key and cancels
the leaver's scope, which is what a detail of *this* row wants.

On the real DOM (scripts/smoke/tests/updatable-selection.mjs): the
element of a surviving key keeps its node, its focus and the text
being typed while the list is re-rendered from the edit; a row that
leaves the selection takes its element; an edit made under one filter
is there under another.

**`editsOf` is deleted.** A projection that only narrows a row is
never needed for editing — an element is typed at the stored row and
reads the fields it names — so movie-browser, its one demo, left it
for `actedWithin`. What remains outside a selection is a projection
that **joins**: a cart line showing its product's price beside a
quantity editor reads another array, so its row is not a stored row
and no setter is canonical. That case needs no word of its own since
the pilot's `edits`: `editsOf @l @k query editor` was `gatheredOf @k
query (edits @l editor)`, a composition of two exported words that
nothing reached, and it goes on subsumption grounds (L14). The
composition is law-tested (test/Main.purs, *The editing list*); no
demo writes it yet.

For a single field the same ladder already stood, with the middle rung
deliberately closed: `text f` reads, an editor writes its field, and an
editor bracketed by a lossy `dimap` is refused (the round-trip
contract; a normalization is `# settled`). The collection's middle
rung is open because a selection loses nothing.

## A variant in data

A variant stored in a field is a value, and what the vocabulary does
with it depends on whether its cases carry payloads:

- **Bare cases** are a choice, edited in place by a selector
  (venue's `"Cuisine"`, `"Trading"`, `"Service"`; diner's `"Table"`).
- **Cases with payloads are a lifecycle**, written by transitions.
  diner's order is `stage :: [ taking, cooking, ready, paid :: { total
  :: Int } ]`: no editor shows it; each station reads the orders in its
  case (`cooking`, `bills`), and a fold moves an order on, the event
  bringing the payload (`settle` stores the total the pass showed).
  Exclusivity is by construction and the screens follow from it — an
  order is on exactly one station.
- **A choice whose alternatives the user edits is not stored as a
  sum.** This is the 2026-10-11 decision (`bracketed`), and the data
  reading confirms it rather than reopening it: a sum holds one case,
  so an editor over it loses the other cases' fields on every switch.
  The model holds the selector and every case's fields side by side
  (venue's `Delivery`: `"Service"`, `"Partner name"`, `zones`), each
  edited `# inCaseAt @c @"Service"`; the walk confirms a fee set for
  own couriers is still there after a trip through *Partner*. Where a
  consumer wants the sum, it is a function of that record
  (`deliveryLine` reads it with one `match`).

Two costs, both already known. A pane names only its case and feeds
its content the whole row (2026-10-13), so a read under a case finds
the case again: `takingsLine` matches all four stages to read one
total (for rows of an array there is a way round,
[At most one is an array](#at-most-one-is-an-array)). And data that
arrives **given** as sums, with identifier labels rather than copy — a
backend's JSON — is not the model: it is converted where it enters and
leaves (`load`, `action`), and the model in between is the product with
labels the screen reads.

## Keys and references

A row's identity is one of its fields (L9), so normalized data needs
nothing new: a line stores its dish's key, a query joins it, and an
operation writes by key. shopping-cart was the one demo whose
reference data lived outside the model (`const productCatalogue`); it
now holds `products` with their stock beside `order`, whose lines
refer to products by name, and adding a unit moves a unit of stock
with it.

**A reference is written by a pick.** `listOf @"Dish ordered" @"name"
menuCard` emits the key, the fold stores it. There is no editor of a
field holding a key: a selector's options are the cases of a variant,
closed in the view, and a list of options that is data has no leaf.
See [Open](#open).

## The data context

The pilot's form fits data applications without change, and names
what they are made of. The app module holds the **schema** (the seed's
signature), the **queries** (read functions), the **transactions**
(folds) and one word that closes a view over them:

```purescript
opened :: forall m. Applicative m => PUI m Diner Event -> PUI m {} Diner
opened view =
  Semigroupoid.do
    view
    VariantToRecord.do
      capture @"Table chosen"
      fold @"Dish ordered" orderDish
      …
  # looped # with dinnerService
```

A view is then exactly a UI over the data, `PUI m Diner Event`: it
reads the data and writes operations, and `Event` is the data's write
interface, stated once. Three consequences showed in the demos:

- **Several UIs over one value are a merge.** diner is three stations
  — floor, kitchen, pass — each a block of displays and emitters, merged
  `RecordToVariant.do` and closed once (`# opened`). An order sent
  from the floor is a ticket in the kitchen because both read `orders`.
- **Screen order is free again.** With edits as occurrences an editor
  may stand below an emitter: contacts' list is above its form,
  todo-list's Add is between its field and its list. In main's form
  every editor precedes the event ensemble.
- **The uniform statement.** A context is a transition `PUI m out
  Data`, a seed and the knot. For an in-place UI the transition is the
  wire, which is why its context is the bare `# looped # with seed`;
  by [the law](#the-law-between-them) nothing else distinguishes the
  two.

The database vocabulary transfers one to one, which is the practical
form of this note:

| Database | Here |
| --- | --- |
| the database, one consistent state | the row at the knot |
| schema | the seed's signature |
| query, view | a copy function, a projection, a classifier |
| transaction | a fold, `payload -> data -> data`, released whole |
| constraint | a `settled` normalizer: true of every value |
| primary key | the field a keyed word names (`@"id"`) |
| foreign key | a key stored in another row: joined by a query, written by a pick |
| read-only view, updatable view, base table | `actedOf`, `actedWithin`, `actedAt` |
| change feed, replica | `dispatched…`, `accumulated…` (`+` in) |

## What was built

Library: `actedWithin` (`Data.Profunctor.Keyed.Acting`, re-exported by
`PUI`), its three laws tested; the in-place/operation law tested;
diner's context driven on the probe carrier (one feed per transaction,
each a consistent state, no browser);
scripts/smoke/tests/updatable-selection.mjs. `editsOf` deleted. From
the pilot: `edits` (its precondition now stated), `told`.

One carrier fix, found by contacts (`PUI.Web`'s `Hosting` instance): an
element that is a bare run of siblings with a pane among them left the
pane's content behind when its key left, and would have left it in
place when its key moved. The run was recorded at build, and a pane
attaches its content later, between two placeholders of the run. The
run is now read when it is moved or removed, first node to last. No
demo had the combination — every element with a pane was wrapped in
one node — and the smoke test now holds it (it failed before the fix).

| Demo | What it is about |
| --- | --- |
| **venue** (new; MDC3, MDC2) | a document edited in place as deep as it nests — `group`, `acted`, `actedAt` inside a pane inside a `group`, a choice with its fields beside it — and a summary card reading the same value through queries; `Venue → Venue`, no event |
| **contacts** (new; MDC3, MDC2) | master and detail over one stored array: the form is the picked row (`actedWithin`), New and Delete are operations; context form |
| **diner** (new; MDC3, MDC2) | one value, three UIs: queries that filter, join and aggregate; seven operations, one of them carrying a two-level path (`joined` in `joined`); a stage variant as lifecycle with a payload on its last case; stock moving with orders |
| movie-browser (MDC2, MDC3) | in place through the filter; four-shape loop reduced to `×→×` |
| todo-list (MDC2, MDC3) | ticked in place through the visibility filter, Add and Clear as operations; context form; seeded with data |
| shopping-cart (MDC2, MDC3) | the shelf and its stock moved into the data; lines refer to products by key; context form |
| inbox (MDC2, MDC3) | the reading pane is the selection of the open message (`gatheredOf @"id" openMessages`), its lines typed at the message |

Each was walked in headless Chrome (the walks are not committed: the
smoke suite keeps only what the laws cannot reach). 105 demos on 46
pages mount clean.

## Costs

1. **The selection's precondition is unchecked.** `select` must return
   rows of `t`. The type pins the row but not where it came from.
2. **`edits` tells an answer from an edit by when it comes.** It drops
   what a stage emits inside its feed, which is every leaf's answer.
   A stage under `debounced` answers after its window, and that answer
   leaves as an edit; folded and looped it would repeat for ever. No
   demo combines the two, and the headers of `edits` and `unanswering`
   now state the precondition, but nothing enforces it.
3. **An in-place island names a field and a case.** `filledTextField'
   @"Todo" { … } # edits @"Todo typed"` breaks one anchor per line; the
   case exists only to be captured. Islands may share one case; the
   demos name each.
4. **A display has no place after the last emitter** of its block in
   the context form: every station of diner is displays first, or a
   display directly before an emitter in a nested block. A read-only
   UI's honest type over the data is `PUI m Data [ | v ]`, writing
   nothing; no word gives a display that type.
5. **The app module imports `PUI`** for its context word, and its
   folds and seed become private — against writing.md as it stands.
6. **Paths in events nest**: `{ event :: { event :: String, model ::
   Order }, model :: Diner }`. Honest, and the alternative is stated
   above, but two levels is where it stops being pleasant.
7. **A collection needs an element of its own.** A keyed word appends
   its elements to the element it was built in, so contacts' form is
   wrapped in a bare `div`; without it a form picked later is appended
   after everything else (on the demo page, outside the demo — found on
   the first walk, invisible to the compiler).
8. **Whole re-feed.** Every change re-feeds the whole value and every
   query recomputes. Feed-idempotence and keyed reconciliation keep
   the DOM work proportional to the change; the query work is
   proportional to the data. cells is the precedent for when that
   matters.

## Open

- **A reference editor.** A field holding a key, its options a keyed
  array from the data: the selector's group over option leaves, with
  `acting` supplying the leaves at runtime where the `+→+` merge
  supplies them from the view. It cannot be an emitter with a fold
  beside the loop (`updated`, deleted 2026-10-11), so it is a leaf
  family per vocabulary, with copy from a function where a static
  option has its label. Not attempted.
- **A display in an emitter block** (cost 4). The derivation would be
  the display followed by the `×→+` unit; whether that is a word or a
  reason to prefer the knot-step form is undecided.
- **The updatable join has no demo.** `( row # edits @l ) # gatheredOf
  @k query` with a fold that writes back by key is tested on the probe
  carrier only; a cart line's quantity edited beside its joined price
  would be the demo.
- **Data arriving as changes** was read, not exercised: departures and
  scoreboard are the `+`-in column, fed by a clock where a data
  application would have a server's feed.

## Not synced

writing.md (the app shape, the view-model module's imports, the stage
table's row for `editsOf`, now deleted, a row for `actedWithin`), guardrails (L3's
keyed forms, L13's note on editors in a derived list), CLAUDE.md (the
demo tables, the keyed bullet) and vocabulary.md; the CHANGELOG has
its entry (2026-11-14). The pilot was not synced either; both wait for a decision
on the context form.
