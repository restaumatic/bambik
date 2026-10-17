# Flight-booker, line by line

The 7GUIs flight booker: a one-way/return selector, one or two date fields
coloured red while they hold no valid date, a Book button there only for a
bookable trip, and a message stating what was booked. It is a small demo
that uses three of the four shapes — editors (`×→×`), an emitter (`×→+`)
and a dialog (`+→+`); the fourth, the fold (`+→×`), is counter's — so once
it reads plainly, the larger demos are the same moves repeated. The view
is `demo/7guis/flight-booker/MDC2.purs` (module `FlightBooker.MDC2`), the
view model module `demo/7guis/flight-booker/FlightBooker.purs`; in an app
both are under `.spago/bambik/<tag>/`. The rules the lines follow are in
[writing.md](writing.md); what each component does is in its module header
(`npx spago docs --open`).

## The view

```purescript
module FlightBooker.MDC2 (flightBooker) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import FlightBooker (bookedLine, bookingState, plannedTrip, returnDateInvalid, startDateInvalid)
import PUI (looped, with)
import PUI.Web ((<+>), attrWith, choice, inCaseAt, provided, text)
import PUI.Web.MDC2 (acknowledged, body, body1, button', filledTextField', select)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type" (choice @"one-way flight" <+> choice @"return flight")
      filledTextField' @"Start date" { hint: "DD.MM.YYYY" } # attrWith "aria-invalid" startDateInvalid
      filledTextField' @"Return date" { hint: "DD.MM.YYYY" } # attrWith "aria-invalid" returnDateInvalid # inCaseAt @"return flight" @"Flight type"
    ) # looped # with plannedTrip
    button' @"Book" { icon: "flight_takeoff" } # provided @"bookable" bookingState
    acknowledged @"Book" "OK" $ body1 $ text bookedLine
```

**The imports.** `PUI` for the words that shape data flow (`looped`,
`with`); `PUI.Web` for the words every design system shares (`choice` and
`<+>`, the panes `inCaseAt` and `provided`, the `attrWith` decorator, the
`text` leaf); and `PUI.Web.MDC2` for the design system, its `body`
included. The MDC3 twin differs in its module and entry name, that one
vocabulary import, and the typography it pulls from it (`bodyMedium` for
`body1`); the view model module is shared verbatim.
`QualifiedDo.Semigroupoid as Semigroupoid` gives `Semigroupoid.do`:
stages in sequence, not a monad.

**`body $ Semigroupoid.do`.** Mount at the document body, dressed for
Material 2, applied with `$` (layout), never `#` (data flow). The outer
`Semigroupoid.do` has three stages, and data flows top to bottom as the
code reads: the form emits the model on every edit → the button, there
only for a bookable trip, turns it into an event → the dialog tells the
user what the event booked. Code order is DOM order and data order
(writing.md *The pipeline*).

**Stage 1 — the form.** An inner `Semigroupoid.do` of three editors, closed
with `# looped # with plannedTrip`; the model row is `plannedTrip`'s
signature, in the view model module.

- `select @"Flight type" (choice @"one-way flight" <+> choice @"return flight")` —
  the type argument is both the caption and the model field, so this
  edits `{ "Flight type" :: [ "one-way flight" :: {}, "return flight" :: {} ] }`.
  Each `choice @l` states an option's copy once, as its case, and `<+>`
  joins the options in writing order while closing their row. A trip
  always has a type, so the plain `select` fits: the field holds the
  variant itself.
- `filledTextField' @"Start date" { hint: "DD.MM.YYYY" }` — the label is
  the field's name, drawn as its caption; the format is not the name, so
  it rides the primed sibling's `hint`, drawn under the field.
- `# attrWith "aria-invalid" startDateInvalid` — a decorator: on every
  feed it sets the field's `aria-invalid` from the model, `"true"` while
  the text is no valid date. The page's stylesheet colours an invalid
  field red, and assistive technology announces it. The decorator keeps
  the shape it decorates, so the editor is still the stage.
- `filledTextField' @"Return date" … # inCaseAt @"return flight" @"Flight type"`
  — the editor pane: this field exists only while the stored
  `"Flight type"` is at case `return flight`, and the model passes
  straight through otherwise — bambik's form of the task's disabled
  field. The pane names the case and then the field, both by label; the
  model row types it (writing.md *Conditional visibility*).

Each editor is fed the whole record and emits it with its own field
changed. `looped # with plannedTrip` supplies the starting record and
loops each change back to the top, so all three editors see every edit;
it also closes the app's input to `{}`, which `body` requires (writing.md
*App shape*).

**Stage 2 — the button, `button' @"Book" { icon: "flight_takeoff" } #
provided @"bookable" bookingState`.** The first shape change, `×→+`: fed
the model, the button emits case `"Book"` carrying the model on click
(its case is its caption; `icon` is presentation config). `provided` is
the emitter pane: the button exists only while the classifier
`bookingState` yields case `bookable`, so a trip with an unreadable date
or a return before its start cannot be booked — the task's disabled
button, by construction rather than by a flag. The pane names only its
case; the classifier's signature states the rest (writing.md
*Conditional visibility*).

**Stage 3 — the dialog, `acknowledged @"Book" "OK" $ body1 $ text
bookedLine`.** `+→+`: the dialog owns the button's case. A `"Book"`
event opens it, titled by the case and showing its content fed the
event's payload — the trip — and the user's `"OK"` closes it, the case
consumed, so nothing flows on. Its content is a stage like any other:
`body1 $ text bookedLine`, a display whose whole sentence, "You have
booked a one-way flight on 27.03.2026.", is one pure function in the view
model module. The message stays until it is dismissed, so unlike a
status, which tells only its case, it may read the payload (writing.md
*Components*).

## The view model

```purescript
module FlightBooker (Trip, bookedLine, bookingState, plannedTrip, returnDateInvalid, startDateInvalid) where

import Prelude (bind, otherwise, (#), (&&), (*), (+), (/=), (<=), (<>), (==), (>=), (||))

import Data.Int (fromString, rem)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), split)
import Data.Variant (match)

type Trip =
  { "Flight type" :: [ "one-way flight" :: {}, "return flight" :: {} ]
  , "Return date" :: String
  , "Start date" :: String
  }

type Date = { y :: Int, m :: Int, d :: Int }

plannedTrip :: Trip
plannedTrip = { "Flight type": ."one-way flight" {}, "Start date": "27.03.2026", "Return date": "27.03.2026" }

startDateInvalid :: Trip -> String
startDateInvalid { "Start date": input } = invalidity input

returnDateInvalid :: Trip -> String
returnDateInvalid { "Return date": input } = invalidity input

bookingState :: Trip -> [ bookable :: {}, unbookable :: {} ]
bookingState trip = if bookable trip then .bookable {} else .unbookable {}

bookedLine :: Trip -> String
bookedLine { "Flight type": flightType, "Start date": out, "Return date": back } = flightType # match
  { "one-way flight": \_ -> "You have booked a one-way flight on " <> out <> "."
  , "return flight": \_ -> "You have booked a return flight on " <> out <> ", returning on " <> back <> "."
  }

invalidity :: String -> String
invalidity input = maybe "true" (\_ -> "false") (parseDate input)

bookable :: Trip -> Boolean
bookable { "Flight type": flightType, "Start date": startInput, "Return date": returnInput } =
  case parseDate startInput of
    Nothing -> false
    Just out -> flightType # match
      { "one-way flight": \_ -> true
      , "return flight": \_ -> maybe false (\back -> dateKey out <= dateKey back) (parseDate returnInput)
      }

parseDate :: String -> Maybe Date
parseDate s = case split (Pattern ".") s of
  [ dd, mm, yyyy ] -> do
    d <- fromString dd
    m <- fromString mm
    y <- fromString yyyy
    if m >= 1 && m <= 12 && d >= 1 && d <= daysIn m y && y >= 1000
      then Just { y, m, d }
      else Nothing
  _ -> Nothing

daysIn :: Int -> Int -> Int
daysIn m y
  | m == 2 = if rem y 4 == 0 && (rem y 100 /= 0 || rem y 400 == 0) then 29 else 28
  | m == 4 || m == 6 || m == 9 || m == 11 = 30
  | otherwise = 31

dateKey :: forall r1. { y :: Int, m :: Int, d :: Int | r1 } -> Int
dateKey { y, m, d } = y * 10000 + m * 100 + d
```

**No library in sight.** The module imports the domain — `Prelude`,
`Maybe`, `Data.Int`, `Data.String`, `Data.Variant` — and nothing from
`PUI`. The export list is exactly what the view imports; everything else
is a private helper. It compiles and tests without a browser (writing.md
*View module and view model module*).

**The exports, in the order the view uses them.**

- `plannedTrip` — the starting record `with` feeds the loop. Its keys are the leaves'
  labels, quoted because they are copy (`"Start date"`), and
  its variant field is written with the constructor sugar `."one-way flight" {}`
  (the type `[ … ]` is the matching type sugar). The two dates start
  equal, as the task asks.
- `Trip` — the model row, exported because the exported signatures use
  it; no view imports it, since the view writes no row.
- `startDateInvalid`, `returnDateInvalid` — the attribute values, one per
  field, each asking `parseDate` whether its text is a date the calendar
  has (31.02.2026 is not).
- `bookingState` — the classifier behind the Book pane: one of two
  exclusive states, the cases bare since the pane shows the model
  itself.
- `bookedLine` — the dialog's copy function: the selection spelled out,
  the dates as they were entered.

**Three things worth noticing.** The rows the signatures share are
named once, as `type` synonyms — `Trip` the model row, `Date` what
`parseDate` finds: a synonym is a name for a structure, not a new type,
so the shape is still the interface (writing.md *Code style* → *Types
and values*). Every exported function carries the signature the view
reported for it with its tails filled — the view named the three fields
its editors bind and left the rest unknown, and the signature closes the
row: the model row its line is fed, its fields in the compiler's
alphabetical order — while the private helper `dateKey` keeps an open
row of its own, since no view line types it (writing.md *Code style* →
*Business functions*). And `parseDate` has a real `do` — `Maybe`'s
monad: `Semigroupoid.do` in the view composes stages, `do` in the view
model module is the ordinary one.

## What to read next

- **counter** — one display reading one function
  (`headline4 $ text countLine` above its `button @"Count"`), one fold
  (`fold @"Count" increment`), over a
  model of `{ counted :: Int }`.
- **timer** — two displays of different sorts,
  `linearProgress elapsedFraction` and `text progressLine`,
  both computed from the model, neither stored; `ticks @"Clock
  ticked" tickPeriod` drives it through `fold @"Clock
  ticked" tick`.
- **temperature-converter** — two editors kept consistent with
  `# settled fromCelsius` / `# settled fromFahrenheit`, each rewriting
  the other only when the two disagree.
- **payment** — an action and the status that tells its outcome:
  `indeterminateCircularProgress # action @"Charge card" chargeFlaky`,
  then `snackbar @"Card charged" identity`, the fold that takes the
  charged order as the model.
- **todo-list** — a selectable list, `listOf @"Todo toggled" @"id"`, a filter
  selector, and panes over `remainingItems`.
- **checkout** — a wizard: the step is a model field, each step's line a
  pane `# inCaseAt @c @"step"`, and Back and Next are the face of a
  selector the app packages itself, `stepper @"step" (choice @"Cart" <+>
  …)`, editing that field in the order its options are written.
- **crud** — the list's pick `# joined @"Person picked"`, folded by
  `pick`; Update and Delete exist only while a person is picked, `#
  providedAt @"picked" @"selected"`; the prefix filter's `# settled
  filterChanged` drops a selection it hides.
- **tic-tac-toe** — a reset is a restart: `openingPosition` is the seed and
  `reset @"New game" openingPosition` the reset.
- **order-form** — all four shapes on one screen: a `looped` form in
  labelled groups inside the app's loop, a choice with a field per case
  (`tabBar @"Mode"` and its `inCaseAt` panes), the live summary, a
  `RecordToVariant.do` of buttons, an estimate joined with the model it
  lands on, and the folds and statuses.

When the word for something is missing, [vocabulary.md](vocabulary.md)
goes from the need to a word, a demo using it, and where to read more.
