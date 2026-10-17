# Flight-booker, line by line

The 7GUIs flight booker: a one-way/return selector, one or two date fields,
a live line describing the itinerary (or what is wrong with it), a Book
button there only for a bookable trip, and a confirmation. It is a small
demo that uses all four shapes — editors (`×→×`), an emitter (`×→+`), an
action (`+→+`) and a status (`+→×`) — so once it reads plainly, the larger
demos are the same moves repeated. The view is `demo/7guis/flight-booker/MDC2.purs` (module
`FlightBooker.MDC2`), the view model module
`demo/7guis/flight-booker/FlightBooker.purs`; in an app both
are under `.spago/bambik/<tag>/`. The rules the lines follow are in
[writing.md](writing.md); what each component does is in its module header
(`npx spago docs --open`).

## The view

```purescript
module FlightBooker.MDC2 (flightBooker) where

import Prelude (Unit, (#), ($), identity)

import Effect (Effect)
import FlightBooker (bookingLine, bookingState, itinerarySettleTime, plannedTrip, submit)
import PUI (action, debounced, looped, with)
import PUI.Web ((<+>), choice, inCaseAt, provided, text)
import PUI.Web.MDC2 (body, body1, button', filledTextField', indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type" (choice @"one-way" <+> choice @"return")
      filledTextField' @"Start date" { hint: "DD.MM.YYYY" }
      filledTextField' @"Return date" { hint: "DD.MM.YYYY" } # inCaseAt @"return" @"Flight type"
    ) # looped # with plannedTrip
    ( body1 $ text bookingLine ) # debounced itinerarySettleTime
    button' @"Book" { icon: "flight_takeoff" } # provided @"bookable" bookingState
    indeterminateLinearProgress # action @"Book" submit
    snackbar @"Flight booked" identity
```

**The imports.** `PUI` for the words that shape data flow (`looped`, `with`,
`debounced`, `action`); `PUI.Web` for the words every
design system shares (`choice` and `<+>`, the panes `inCaseAt` and
`provided`, the `text` leaf); and `PUI.Web.MDC2` for the design system,
its `body` included. The MDC3 twin differs in its module and entry name,
that one vocabulary import, and the typography it pulls from it
(`bodyLarge` for `body1`); the view model module is shared verbatim.
`QualifiedDo.Semigroupoid as Semigroupoid` gives `Semigroupoid.do`:
stages in sequence, not a monad.

**`body $ Semigroupoid.do`.** Mount at the document body, dressed for
Material 2, applied with `$` (layout), never `#` (data flow). The outer
`Semigroupoid.do` has five stages, and data flows top to bottom as the code
reads: the form emits the model on every edit → the itinerary line shows it
and passes it on once it has settled → the button, there only for a
bookable trip, turns it into an event → the action turns the event into
an outcome → a snackbar tells the outcome.
Code order is DOM order and data order (writing.md *The pipeline*).

**Stage 1 — the form.** An inner `Semigroupoid.do` of three editors, closed
with `# looped # with plannedTrip`; the model row is `plannedTrip`'s
signature, in the view model module.

- `select @"Flight type" (choice @"one-way" <+> choice @"return")` —
  the type argument is both the caption and the model field, so this
  edits `{ "Flight type" :: [ "one-way" :: {}, "return" :: {} ] }`. Each
  `choice @l` states an option's copy once, as its case, and `<+>` joins
  the options in writing order while closing their row. A trip always
  has a type, so the plain `select` fits: the field holds the variant
  itself.
- `filledTextField' @"Start date" { hint: "DD.MM.YYYY" }` — the label is
  the field's name, drawn as its caption; the format is not the name, so
  it rides the primed sibling's `hint`, drawn under the field.
- `filledTextField' @"Return date" { hint: "DD.MM.YYYY" } # inCaseAt @"return" @"Flight type"`
  — the editor pane: this field exists only while the stored
  `"Flight type"` is at case `return`, and the model passes straight
  through otherwise. The pane names the case and then the field, both
  by label; the model row types it (writing.md *Conditional
  visibility*).

Each editor is fed the whole record and emits it with its own field
changed. `looped # with plannedTrip` supplies the starting record and
loops each change back to the top, so all three editors see every edit;
it also closes the app's input to `{}`, which `body` requires (writing.md
*App shape*).

**Stage 2 — the itinerary line, `( body1 $ text bookingLine ) # debounced
itinerarySettleTime`.**

- `body1 $ text bookingLine` — the `text` leaf takes a **read function**:
  the whole sentence, "A one-way flight on 27.03.2026" or "⚠ the return
  date is before the start date", is `bookingLine`, one pure function in
  the view model module. The view holds no glue, and the line names its
  own copy function. One line, not a pane per state: the states differ
  in their words, not in what is on screen, and words are the copy
  function's. A display is a stage: it shows the model and releases it,
  so the next line is fed what it shows.
- `# debounced itinerarySettleTime` — `debounced` releases the model
  only once the edits pause for `itinerarySettleTime` (`{ ms: 300.0 }` in
  the view model module, so the view carries no literal): the line does
  not flash a problem at every keystroke of a half-typed date, and the
  Book button below is handed the trip only after it has stood still on
  screen.

**Stage 3 — the button, `button' @"Book" { icon: "flight_takeoff" } #
provided @"bookable" bookingState`.** The first shape change, `×→+`: fed
the model, the button emits case `"Book"` carrying the model on click
(its case is its caption; `icon` is presentation config). `provided` is
the emitter pane: the button exists only while the classifier
`bookingState` yields case `bookable`, so a trip with a problem cannot
be booked — the 7GUIs task's disabled button, by construction rather
than by a flag. The pane names only its case; the classifier's
signature states the rest (writing.md *Conditional visibility*).

**Stage 4 — the action, `indeterminateLinearProgress # action @"Book"
submit`.** `+→+`: `action @"Book"` owns the button's case, its payload
goes to `submit`, and the action is opened by its progress bar, shown
between the run's `started` and `ended`; the slot owns those two
occurrences and nothing else, so no snackbar can stand in it. The
outcome is `submit`'s: its signature names it as the words the user
will read, `"Flight booked"`, because that is what the status shows.
The line writes no outcome — the status after it owns that case, and
that is what closes the action's row (with `submit` still a hole, it is
what types it).

**Stage 5 — the status, `snackbar @"Flight booked" identity`.** `+→×`:
owning the outcome case and telling it by its label — a status takes no
copy function: it is ephemeral, so its case label is its whole copy,
and what the user must keep is a model field shown by a display — and
a fold, as every status is: `submit` returns the trip under its case,
and `identity` releases it, so the pipeline ends with the itinerary it
was booked with (writing.md *Components*). An action with several
outcomes puts one status per case in a `VariantToRecord.do` (crud).

## The view model

```purescript
module FlightBooker (Trip, bookingLine, bookingState, itinerarySettleTime, plannedTrip, submit) where

import Prelude (bind, const, discard, otherwise, pure, show, (#), (&&), (*), (+), (<), (<=), (<>), (>=), (>>>))

import Data.Either (Either(..), either)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)

type Trip =
  { "Flight type" :: [ "one-way" :: {}, return :: {} ]
  , "Return date" :: String
  , "Start date" :: String
  }

type Date = { y :: Int, m :: Int, d :: Int }

type Problem =
  [ returnBeforeStart :: {}
  , unreadableReturn :: { input :: String }
  , unreadableStart :: { input :: String }
  ]

type Itinerary = [ oneWayOn :: Date, returnBetween :: { out :: Date, back :: Date } ]

plannedTrip :: Trip
plannedTrip = { "Flight type": ."one-way" {}, "Start date": "27.03.2026", "Return date": "27.03.2026" }

itinerarySettleTime :: { ms :: Number }
itinerarySettleTime = { ms: 300.0 }

bookingLine :: Trip -> String
bookingLine = parse >>> either (\problem -> "⚠ " <> problemText problem) itineraryText

bookingState :: Trip -> [ bookable :: {}, problem :: {} ]
bookingState = parse >>> either (const (.problem {})) (const (.bookable {}))

submit :: Trip -> Aff [ "Flight booked" :: Trip ]
submit trip = do
  delay (Milliseconds 600.0)
  pure (."Flight booked" trip)

parse :: Trip -> Either Problem Itinerary
parse { "Flight type": flightType, "Start date": startInput, "Return date": returnInput } =
  case parseDate startInput of
    Nothing -> Left (.unreadableStart { input: startInput })
    Just out -> flightType # match
      { "one-way": \_ -> Right (.oneWayOn out)
      , return: \_ -> case parseDate returnInput of
          Nothing -> Left (.unreadableReturn { input: returnInput })
          Just back
            | dateKey back < dateKey out -> Left (.returnBeforeStart {})
            | otherwise -> Right (.returnBetween { out, back })
      }

problemText :: Problem -> String
problemText = match
  { unreadableStart: \{ input } -> "start date " <> show input <> " is not a valid DD.MM.YYYY date"
  , unreadableReturn: \{ input } -> "return date " <> show input <> " is not a valid DD.MM.YYYY date"
  , returnBeforeStart: \_ -> "the return date is before the start date"
  }

itineraryText :: Itinerary -> String
itineraryText = match
  { oneWayOn: \out -> "A one-way flight on " <> formatDate out
  , returnBetween: \{ out, back } -> "A return flight: out " <> formatDate out <> ", back " <> formatDate back
  }

parseDate :: String -> Maybe Date
parseDate s = case split (Pattern ".") s of
  [ dd, mm, yyyy ] -> do
    d <- fromString dd
    m <- fromString mm
    y <- fromString yyyy
    if d >= 1 && d <= 31 && m >= 1 && m <= 12 && y >= 1000
      then Just { y, m, d }
      else Nothing
  _ -> Nothing

formatDate :: forall r1. { y :: Int, m :: Int, d :: Int | r1 } -> String
formatDate { y, m, d } = pad d <> "." <> pad m <> "." <> show y
  where
  pad n = (if n < 10 then "0" else "") <> show n

dateKey :: forall r1. { y :: Int, m :: Int, d :: Int | r1 } -> Int
dateKey { y, m, d } = y * 10000 + m * 100 + d
```

**No library in sight.** The module imports the domain — `Prelude`,
`Maybe`, `Either`, `Aff`, `Data.Variant` — and nothing from `PUI`. The
export list is exactly what the view imports; everything else is a private
helper. It compiles and tests without a browser (writing.md *View module and view model
module*).

**The exports, in the order the view uses them.**

- `plannedTrip` — the starting record `with` feeds the loop. Its keys are the leaves'
  labels, quoted because they are copy (`"Start date"`), and
  its variant field is written with the constructor sugar `."one-way" {}`
  (the type `[ … ]` is the matching type sugar).
- `Trip` — the model row, exported because the exported signatures use
  it; no view imports it, since the view writes no row.
- `itinerarySettleTime` — a duration, `{ ms :: Number }`.
- `bookingLine` — the line's copy function: what `parse` finds, spelled
  out. A problem is a reason (`.returnBeforeStart {}`,
  `.unreadableStart { input }`), never a sentence, so `problemText`
  phrases it and `itineraryText` phrases an itinerary.
- `bookingState` — the classifier behind the Book pane: one of two
  exclusive states, the cases bare since the pane shows the model
  itself. It shares `parse` with `bookingLine`, so the button is there
  exactly when the line describes a trip rather than a problem.
- `submit` — the `Aff` boundary, standing in for the airline with a
  short delay so the progress bar shows. It is handed only a bookable
  trip, so its outcome row has one case, named as the snackbar's words,
  carrying the trip it was given, which the status folds back as the
  model — an effect returns the model.

**Three things worth noticing.** The rows the signatures share are
named once, as `type` synonyms — `Trip` the model row, `Itinerary` and
`Problem` what `parse` finds, `Date` inside them: a synonym is a name for
a structure, not a new type, so the shape is still the interface
(writing.md *Code style* → *Types and values*). Every exported function
carries the signature the view reported for it with its tails filled —
the view named the three fields its editors bind and left the rest
unknown, and the signature closes the row: the model row its line is
fed, its fields in the compiler's alphabetical order — while the private
helpers `formatDate` and `dateKey` keep open rows of their own, since no
view line types them (writing.md *Code style* → *Business functions*).
And `parseDate` has a real `do` — `Maybe`'s monad: `Semigroupoid.do` in
the view composes stages, `do` in the view model module is the ordinary
one.

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
  `# settled fromCelsius` / `# settled fromFahrenheit`.
- **todo-list** — a selectable list, `listOf @"Todo toggled" @"id"`, a filter
  selector, and panes over `remainingItems`.
- **checkout** — a wizard: the step is a model field, each step's line a
  pane `# inCaseAt @c @"step"`, and Back and Next are the face of a
  selector the app packages itself, `stepper @"step" (choice @"Cart" <+>
  …)`, editing that field in the order its options are written.
- **crud** — a load action before the knot, `indeterminateLinearProgress #
  load loadPeopleCatalogue`, its outcome folded in by `capture @"People
  loaded"`, run by `body`'s one feed of
  `{}`; create/update/delete are chains in the fold block, each an action
  owning its button's case, followed by the statuses that own its
  two outcomes.
- **tic-tac-toe** — a reset is a restart: `openingPosition` is the seed and
  `reset @"New game" openingPosition` the reset.
- **order-form** — all four shapes on one screen: a `looped` form in
  labelled groups inside the app's loop, a choice with a field per case
  (`tabBar @"Mode"` and its `inCaseAt` panes), the live summary, a
  `RecordToVariant.do` of buttons, an estimate joined with the model it
  lands on, and the folds and statuses.

When the word for something is missing, [vocabulary.md](vocabulary.md)
goes from the need to a word, a demo using it, and where to read more.
