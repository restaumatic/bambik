# Flight-booker, line by line

The 7GUIs flight booker: a one-way/return selector, one or two date fields,
a live line describing the itinerary (or what is wrong with it), a Book
button, and a confirmation. It is a small demo that uses all four shapes —
editors (`×→×`), an emitter (`×→+`), an action (`+→+`) and statuses
(`+→×`) — so once it reads plainly, the larger demos are the same moves
repeated. The view is `demo/7guis/flight-booker-mdc2/FlightBookerMDC2.purs`,
the view model module `demo/7guis/flight-booker/FlightBookerViewModel.purs`; in an app both
are under `.spago/bambik/v0.1.6/`. The rules the lines follow are in
[writing.md](writing.md); what each component does is in its module header
(`npx spago docs --open`).

## The view

```purescript
module FlightBookerMDC2 (flightBookerMDC2) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBookerViewModel (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit)
import PUI (action, atCase, debounced, looped, with)
import PUI.Web ((<+>), choice, inCase, shownWhen, text)
import PUI.Web.MDC2 (body, body1, button, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerMDC2 :: Effect Unit
flightBookerMDC2 =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type" {}
        (choice @"one-way" <+> choice @"return")
      filledTextField @"Start date (DD.MM.YYYY)" {}
      filledTextField @"Return date (DD.MM.YYYY)" {} # inCase @"return" _."Flight type"
    ) # looped
      @( "Flight type" :: [ "one-way" :: {}, "return" :: {} ]
       , "Start date (DD.MM.YYYY)" :: String
       , "Return date (DD.MM.YYYY)" :: String
       ) # with plannedTrip
    ( Semigroupoid.do
      body1 (text problemLine) # shownWhen @"problem" @( problem :: { problem :: String }, "one-way" :: { out :: { y :: Int, m :: Int, d :: Int } }, "return" :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ) bookingState
      body1 (text oneWayLine) # shownWhen @"one-way" bookingState
      body1 (text returnLine) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
    button @"Book" { icon: "flight_takeoff" }
    indeterminateLinearProgress # action @[ booked :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ], rejected :: String ] submit # atCase @"Book"
    VariantToRecord.do
      snackbar @"booked" bookedLine
      snackbar @"rejected" rejectedLine
```

**The imports.** `PUI` for the words that shape data flow (`looped`, `with`,
`debounced`, `action`, `atCase`); `PUI.Web` for the words every design
system shares (`choice` and `<+>`, the panes `shownWhen` and `inCase`,
the `text` leaf); `VariantToRecord` for the block that sets the two statuses side by
side; and `PUI.Web.MDC2` for the design system, its `body` included. The
MDC3 twin differs in its module and entry name, that one vocabulary import,
and the typography it pulls from it (`bodyLarge` for `body1`); the view model
module is shared verbatim. `QualifiedDo.Semigroupoid as Semigroupoid` gives
`Semigroupoid.do`: stages in sequence, not a monad.

**`body $ Semigroupoid.do`.** Mount at the document body, dressed for
Material 2, applied with `$` (layout), never `#` (data flow). The outer
`Semigroupoid.do` has five stages, and data flows top to bottom as the code
reads: the form emits the model on every edit → the itinerary line shows it
and passes it on → the button turns it into an event → the action turns the
event into an outcome → a snackbar shows the outcome. Code order is DOM
order and data order (writing.md *The pipeline*).

**Stage 1 — the form.** An inner `Semigroupoid.do` of three editors, closed
with `# looped @( … ) # with plannedTrip`, the model row declared there.

- `select @"Flight type" {} (choice @"one-way" <+> choice @"return")` —
  the type argument is both the caption and the model field, so this
  edits `{ "Flight type" :: [ "one-way" :: {}, "return" :: {} ] }`. Each
  `choice @l` states an option's copy once, as its case, and `<+>` joins
  the options in writing order while closing their row. A trip always
  has a type, so the plain `select` fits: the field holds the variant
  itself.
- `filledTextField @"Start date (DD.MM.YYYY)" {}` — the label carries the
  whole copy, format hint included; `{}` is empty presentation config.
- `filledTextField @"Return date (DD.MM.YYYY)" {} # inCase @"return" _."Flight type"`
  — the editor pane: this field exists only while the stored
  `"Flight type"` is at case `return`, and the model passes straight
  through otherwise. The pane reads the field with a plain accessor; the
  model row on the seed line types it (writing.md *Conditional
  visibility*).

Each editor is fed the whole record and emits it with its own field
changed. `looped @( … ) # with plannedTrip` declares the model row — the three
fields the editors bind, written once — supplies the starting record and
loops each change back to the top, so all three editors see every edit;
it also closes the app's input to `{}`, which `body` requires (writing.md
*App shape*).

**Stage 2 — the itinerary line.** Three panes over one classifier, under
one `# debounced itinerarySettleTime`.

- `body1 (text oneWayLine)` — the `text` leaf takes a **read function**:
  the whole sentence ("A one-way flight on 27.03.2026") is `oneWayLine`,
  one pure function in the view model module. The view holds no glue, and the
  line names its own copy function.
- `# shownWhen @"one-way" bookingState` — the pane is shown while
  `bookingState` yields case `one-way`, and its content reads that case's
  payload `{ out :: { y, m, d } }` — the source data the line is computed
  from. The model is passed on whether the pane is shown or not. Three
  panes over one classifier make the three states exclusive, because the
  classifier returns one case (writing.md *Conditional visibility*). The
  first of the three, `# shownWhen @"problem" @( … ) bookingState`,
  declares the classifier's cases — a derived row, so the view states it
  where it is introduced; the other two name only their case.
- `# debounced itinerarySettleTime` — redraw the line once the edits pause
  for `itinerarySettleTime`, which is `{ ms: 300.0 }` in the view model module,
  so the view carries no literal.

**Stage 3 — `button @"Book" { icon: "flight_takeoff" }`.** The first shape
change, `×→+`: fed the model, it emits case `"Book"` carrying the model on
click. Its case is its caption; `icon` is presentation config.

**Stage 4 — `indeterminateLinearProgress # action @[ … ] submit # atCase @"Book"`.**
`+→+`: `atCase @"Book"` takes the button's case, its payload goes to
`submit`, the progress bar shows while the `Aff` runs, and the outcome —
`[ booked :: …, rejected :: String ]`, declared on the line since no model
field holds it — is emitted when it settles.

**Stage 5 — `VariantToRecord.do` of two snackbars.** `+→×`:
`snackbar @"booked" bookedLine` and `snackbar @"rejected" rejectedLine`
each take one outcome case of `submit` and render it with its copy
function. Together they cover every case, and the block's output is `{}`,
where the pipeline ends (writing.md *Components*).

## The view model

```purescript
module FlightBookerViewModel (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit) where

import Prelude ((&&), (*), (+), (/=), (<), (<$>), (<=), (<>), (>=), (>>>), bind, pure, show)

import Data.Either (Either(..), either)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Variant (expand, match)
import Effect.Aff (Aff)

plannedTrip :: { "Flight type" :: [ "one-way" :: {}, return :: {} ], "Return date (DD.MM.YYYY)" :: String, "Start date (DD.MM.YYYY)" :: String }
plannedTrip = { "Flight type": ."one-way" {}, "Start date (DD.MM.YYYY)": "27.03.2026", "Return date (DD.MM.YYYY)": "27.03.2026" }

itinerarySettleTime :: { ms :: Number }
itinerarySettleTime = { ms: 300.0 }

bookedLine :: [ oneWayOn :: { d :: Int, m :: Int, y :: Int }, returnBetween :: { back :: { d :: Int, m :: Int, y :: Int }, out :: { d :: Int, m :: Int, y :: Int } } ] -> String
bookedLine itinerary = "You have booked: " <> summary itinerary

rejectedLine :: String -> String
rejectedLine problem = "Cannot book: " <> problem

returnBetween :: forall r1. { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } | r1 } -> Maybe [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
returnBetween { out, back } =
  if dateKey back >= dateKey out then Just (.returnBetween { out, back })
  else Nothing

parse :: forall r1. { "Flight type" :: [ "one-way" :: {}, "return" :: {} ], "Start date (DD.MM.YYYY)" :: String, "Return date (DD.MM.YYYY)" :: String | r1 } -> Either String [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
parse { "Flight type": flightType, "Start date (DD.MM.YYYY)": startInput, "Return date (DD.MM.YYYY)": returnInput } = case parseDate startInput of
  Nothing -> Left ("start date " <> show startInput <> " is not a valid DD.MM.YYYY date")
  Just start ->
    if flightType /= ."return" {} then Right (.oneWayOn start)
    else case parseDate returnInput of
      Nothing -> Left ("return date " <> show returnInput <> " is not a valid DD.MM.YYYY date")
      Just back -> case returnBetween { out: start, back } of
        Nothing -> Left "the return date is before the start date"
        Just itinerary -> Right itinerary

bookingState :: { "Flight type" :: [ "one-way" :: {}, return :: {} ], "Return date (DD.MM.YYYY)" :: String, "Start date (DD.MM.YYYY)" :: String } -> [ "one-way" :: { out :: { d :: Int, m :: Int, y :: Int } }, problem :: { problem :: String }, return :: { back :: { d :: Int, m :: Int, y :: Int }, out :: { d :: Int, m :: Int, y :: Int } } ]
bookingState = parse >>> either (\problem -> .problem { problem })
  (match
    { oneWayOn: \out -> ."one-way" { out }
    , returnBetween: ."return"
    })

problemLine :: { problem :: String } -> String
problemLine { problem } = "⚠ " <> problem

oneWayLine :: { out :: { d :: Int, m :: Int, y :: Int } } -> String
oneWayLine { out } = summary (.oneWayOn out)

returnLine :: { back :: { d :: Int, m :: Int, y :: Int }, out :: { d :: Int, m :: Int, y :: Int } } -> String
returnLine r = summary (.returnBetween { out: r.out, back: r.back })

summary :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] -> String
summary = match
  { oneWayOn: \out -> "A one-way flight on " <> formatDate out
  , returnBetween: \r -> "A return flight: out " <> formatDate r.out <> ", back " <> formatDate r.back
  }

submit :: { "Flight type" :: [ "one-way" :: {}, return :: {} ], "Return date (DD.MM.YYYY)" :: String, "Start date (DD.MM.YYYY)" :: String } -> Aff [ booked :: [ oneWayOn :: { d :: Int, m :: Int, y :: Int }, returnBetween :: { back :: { d :: Int, m :: Int, y :: Int }, out :: { d :: Int, m :: Int, y :: Int } } ], rejected :: String ]
submit trip = case parse trip of
  Left problem -> pure (.rejected problem)
  Right itinerary -> expand <$> bookFlight itinerary

bookFlight :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] -> Aff [ booked :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] ]
bookFlight itinerary = pure (.booked itinerary)

parseDate :: String -> Maybe { y :: Int, m :: Int, d :: Int }
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
  labels, quoted because they are copy (`"Start date (DD.MM.YYYY)"`), and
  its variant field is written with the constructor sugar `."one-way" {}`
  (the type `[ … ]` is the matching type sugar).
- `itinerarySettleTime` — a duration, `{ ms :: Number }`.
- `bookingState` — the classifier behind the three `shownWhen` panes: one
  of three exclusive states, each carrying exactly the data its line is
  computed from (`{ problem }`, `{ out }`, `{ out, back }`), so a pane's
  `text oneWayLine` is typed against it.
- `problemLine`/`oneWayLine`/`returnLine` — the panes' copy functions,
  glue and warning glyph included.
- `submit` — the `Aff` boundary. It shares `parse` with `bookingState`, so
  what the live line calls a problem is precisely what Book refuses.
- `bookedLine`/`rejectedLine` — the snackbars' copy functions, each
  outcome case to its sentence.

**Three things worth noticing.** The rows are spelled out in full — the
itinerary variant seven times — because application code declares no
`type` synonyms: the shape is the interface (writing.md *Code style* →
*Types and values*). Every exported function carries the signature the
view reported for it, verbatim — the row its pane or stage is fed, closed,
its fields in the compiler's alphabetical order — while the private
helpers (`parse`, `formatDate`, `dateKey`) keep open rows of their own,
since no view line types them; a function handing its argument on to a
closed helper builds the smaller record
(`returnLine r = summary (.returnBetween { out: r.out, back: r.back })`)
(writing.md *Code style* → *Business functions*). And `parseDate` has a
real `do` — `Maybe`'s monad: `Semigroupoid.do` in the view composes stages,
`do` in the view model module is the ordinary one.

## What to read next

- **counter** — one display reading one function
  (`headline4 (text countLine) # shown`), one button, one fold opened by
  its status (`snackbar @"Count" countedLine # fold increment`), over a
  model of `{ count :: Int }`.
- **timer** — two displays of different sorts,
  `linearProgress @"Elapsed" elapsedFraction` and `text progressLine`,
  both computed from the model, neither stored; `ticks @"Clock ticked"
  tickPeriod # replaying @"Clock ticked" identity` drives it through
  `snackbar @"Clock ticked" clockTickedLine # fold tick`.
- **temperature-converter** — two editors kept consistent with
  `# settled fromCelsius` / `# settled fromFahrenheit`.
- **todo-list** — a selectable list, `listOf @"Todo toggled" @"key"`, a filter
  selector, and panes over `remainingItems`.
- **checkout** — a wizard: the step is a model field, Next/Back each emit
  their own case `# joined` with the model, and one `stepTo` folds both.
- **crud** — a load action before the knot: `action @{ … } loadPeopleCatalogue`
  declares the model, the loop is `# looped` with no row of its own, and
  `body`'s one feed of `{}` runs the load; create/update/delete are `+→+` operands
  inside the loop, their outcomes `identity` in the fold.
- **tic-tac-toe** — a reset is a restart: `openingPosition` is the seed and
  `snackbar @"New game" newGameLine # fold (const openingPosition)` the reset.
- **order-form** — all four shapes on one screen: a `looped` form in
  labelled groups, a variant editor under `bracketed @"Mode"`, the debounced
  summary, `armed` buttons, and each action followed by its statuses.

When the word for something is missing, [vocabulary.md](vocabulary.md)
goes from the need to a word, a demo using it, and where to read more.
