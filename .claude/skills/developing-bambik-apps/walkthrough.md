# Flight-booker, line by line

The 7GUIs flight booker: a one-way/return selector, one or two date fields,
a live line describing the itinerary (or what is wrong with it), a Book
button, and a confirmation. It is a small demo that uses all four shapes —
editors (`×→×`), an emitter (`×→+`), an action (`+→+`) and statuses
(`+→×`) — so once it reads plainly, the larger demos are the same moves
repeated. The view is `demo/7guis/flight-booker/MDC2.purs` (module
`FlightBooker.MDC2`), the view model module
`demo/7guis/flight-booker/FlightBooker.purs`; in an app both
are under `.spago/bambik/<tag>/`. The rules the lines follow are in
[writing.md](writing.md); what each component does is in its module header
(`npx spago docs --open`).

## The view

```purescript
module FlightBooker.MDC2 (flightBooker) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBooker (bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit)
import PUI (action, debounced, joined, kept, looped, with)
import PUI.Web ((<+>), choice, inCase, inCaseAt, text)
import PUI.Web.MDC2 (body, body1, button', filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type"
        (choice @"one-way" <+> choice @"return")
      filledTextField @"Start date (DD.MM.YYYY)"
      filledTextField @"Return date (DD.MM.YYYY)" # inCaseAt @"return" @"Flight type"
    ) # looped # with plannedTrip
    ( Semigroupoid.do
      ( body1 $ text problemLine ) # inCase @"problem" bookingState
      ( body1 $ text oneWayLine ) # inCase @"one-way" bookingState
      ( body1 $ text returnLine ) # inCase @"return" bookingState
    ) # debounced itinerarySettleTime
    ( Semigroupoid.do
      button' @"Book" { icon: "flight_takeoff" }
      indeterminateLinearProgress # action @"Book"
        @( "Flight booked" :: _, "Booking rejected" :: _ ) submit ) # joined @"Flight booked" # joined @"Booking rejected"
    VariantToRecord.do
      snackbar @"Flight booked" kept
      snackbar @"Booking rejected" kept
```

**The imports.** `PUI` for the words that shape data flow (`looped`, `with`,
`debounced`, `action`, `joined`, `kept`); `PUI.Web` for the words every
design system shares (`choice` and `<+>`, the panes `inCase` and `inCaseAt`,
the `text` leaf); `VariantToRecord` for the fold block, the two statuses side by
side; and `PUI.Web.MDC2` for the design system, its `body` included. The
MDC3 twin differs in its module and entry name, that one vocabulary import,
and the typography it pulls from it (`bodyLarge` for `body1`); the view model
module is shared verbatim. `QualifiedDo.Semigroupoid as Semigroupoid` gives
`Semigroupoid.do`: stages in sequence, not a monad.

**`body $ Semigroupoid.do`.** Mount at the document body, dressed for
Material 2, applied with `$` (layout), never `#` (data flow). The outer
`Semigroupoid.do` has four stages, and data flows top to bottom as the code
reads: the form emits the model on every edit → the itinerary line shows it
and passes it on once it has settled → the button turns it into an event
and the action turns the event into an outcome, joined with the model →
a snackbar shows the outcome and keeps the model.
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
- `filledTextField @"Start date (DD.MM.YYYY)"` — the label carries the
  whole copy, format hint included; a leaf takes no options record.
- `filledTextField @"Return date (DD.MM.YYYY)" # inCaseAt @"return" @"Flight type"`
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

**Stage 2 — the itinerary line.** Three panes over one classifier, in a
block of their own, `( Semigroupoid.do … ) # debounced itinerarySettleTime`.

- `body1 $ text oneWayLine` — the `text` leaf takes a **read function**:
  the whole sentence ("A one-way flight on 27.03.2026") is `oneWayLine`,
  one pure function in the view model module. The view holds no glue, and the
  line names its own copy function. A display is a stage: it shows the
  model and releases it, so the next line is fed what it shows.
- `# inCase @"one-way" bookingState` — the pane is shown while
  `bookingState` yields case `one-way`, and its content is fed the model,
  from which `oneWayLine` reads the start date. Detached, the pane is the
  wire. Three panes over one classifier make the three states exclusive,
  because the classifier returns one case (writing.md *Conditional
  visibility*). Each pane names only its own case; the three
  cases together are `bookingState`'s signature, in the view model
  module.
- `( Semigroupoid.do … ) # debounced itinerarySettleTime` — the block of
  panes passes the model on, and `debounced` releases it only once the
  edits pause for `itinerarySettleTime` (`{ ms: 300.0 }` in the view
  model module, so the view carries no literal): the line redraws live,
  but the Book button is handed the itinerary only after it has stood
  still on screen. A display that no later line depends on needs none of
  this — it is a plain line (writing.md *The pipeline*).

**Stage 3 — the button and its action, `( Semigroupoid.do … ) # joined @l`, once per outcome.**
The first shape change, `×→+`: fed the model, `button' @"Book" { icon:
"flight_takeoff" }` emits case `"Book"` carrying the model on click (its
case is its caption; `icon` is presentation config). `action @"Book"`
owns that case, its payload goes to `submit`, and the action is opened
by its progress bar — `indeterminateLinearProgress # action`, shown
between the run's `started` and `ended`; the slot owns those two
occurrences and nothing else, so no snackbar can stand in it. The
outcome labels are named `@( … )` on the action, payloads `_` — the
action widens them into its block before any logic exists — and its
cases are written as the words the user will read,
`"Flight booked"` and `"Booking rejected"`, because they are what the
statuses show. `# joined @l` on the block, once per outcome, pairs it with the model
the button was fed, so what leaves is `{ event, model }` under the
outcome's case.

**Stage 4 — the statuses, `VariantToRecord.do`.** `+→×`: `snackbar
@"Flight booked" kept` and `snackbar @"Booking rejected" kept`, each
owning one outcome case and telling it by its label — a status takes no
copy function: it is ephemeral, so its case label is its whole copy, and
what the user must keep is a model field shown by a display — and each
a fold, as every status is: `kept` releases the model unchanged, so the
pipeline ends with the itinerary it began with (writing.md
*Components*).

## The view model

```purescript
module FlightBooker (bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit) where

import Prelude ((&&), (*), (+), (/=), (<), (<$>), (<=), (<>), (>=), (>>>), bind, const, pure, show)

import Data.Either (Either(..), either)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), split)
import Data.Variant (expand, match)
import Effect.Aff (Aff)

plannedTrip
  :: { "Flight type" :: [ "one-way" :: {}, return :: {} ]
     , "Return date (DD.MM.YYYY)" :: String
     , "Start date (DD.MM.YYYY)" :: String
     }
plannedTrip = { "Flight type": ."one-way" {}, "Start date (DD.MM.YYYY)": "27.03.2026", "Return date (DD.MM.YYYY)": "27.03.2026" }

itinerarySettleTime :: { ms :: Number }
itinerarySettleTime = { ms: 300.0 }

returnBetween :: forall r1. { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } | r1 } -> Maybe [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
returnBetween { out, back } =
  if dateKey back >= dateKey out then Just (.returnBetween { out, back })
  else Nothing

parse :: forall r1. { "Flight type" :: [ "one-way" :: {}, "return" :: {} ], "Start date (DD.MM.YYYY)" :: String, "Return date (DD.MM.YYYY)" :: String | r1 } -> Either [ returnBeforeStart :: {}, unreadableReturn :: { input :: String }, unreadableStart :: { input :: String } ] [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
parse { "Flight type": flightType, "Start date (DD.MM.YYYY)": startInput, "Return date (DD.MM.YYYY)": returnInput } = case parseDate startInput of
  Nothing -> Left (.unreadableStart { input: startInput })
  Just start ->
    if flightType /= ."return" {} then Right (.oneWayOn start)
    else case parseDate returnInput of
      Nothing -> Left (.unreadableReturn { input: returnInput })
      Just back -> case returnBetween { out: start, back } of
        Nothing -> Left (.returnBeforeStart {})
        Just itinerary -> Right itinerary

bookingState
  :: { "Flight type" :: [ "one-way" :: {}, return :: {} ]
     , "Return date (DD.MM.YYYY)" :: String
     , "Start date (DD.MM.YYYY)" :: String
     }
  -> [ "one-way" :: {}, problem :: {}, return :: {} ]
bookingState = parse >>> either (const (.problem {}))
  (match
    { oneWayOn: \_ -> ."one-way" {}
    , returnBetween: \_ -> .return {}
    })

problemLine
  :: { "Flight type" :: [ "one-way" :: {}, return :: {} ]
     , "Return date (DD.MM.YYYY)" :: String
     , "Start date (DD.MM.YYYY)" :: String
     }
  -> String
problemLine = parse >>> either (\problem -> "⚠ " <> problemText problem) (const "")

problemText :: [ returnBeforeStart :: {}, unreadableReturn :: { input :: String }, unreadableStart :: { input :: String } ] -> String
problemText = match
  { unreadableStart: \{ input } -> "start date " <> show input <> " is not a valid DD.MM.YYYY date"
  , unreadableReturn: \{ input } -> "return date " <> show input <> " is not a valid DD.MM.YYYY date"
  , returnBeforeStart: \_ -> "the return date is before the start date"
  }

oneWayLine
  :: { "Flight type" :: [ "one-way" :: {}, return :: {} ]
     , "Return date (DD.MM.YYYY)" :: String
     , "Start date (DD.MM.YYYY)" :: String
     }
  -> String
oneWayLine { "Start date (DD.MM.YYYY)": startInput } =
  maybe "" (\out -> summary (.oneWayOn out)) (parseDate startInput)

returnLine
  :: { "Flight type" :: [ "one-way" :: {}, return :: {} ]
     , "Return date (DD.MM.YYYY)" :: String
     , "Start date (DD.MM.YYYY)" :: String
     }
  -> String
returnLine { "Start date (DD.MM.YYYY)": startInput, "Return date (DD.MM.YYYY)": returnInput } =
  case parseDate startInput, parseDate returnInput of
    Just out, Just back -> summary (.returnBetween { out, back })
    _, _ -> ""

summary :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] -> String
summary = match
  { oneWayOn: \out -> "A one-way flight on " <> formatDate out
  , returnBetween: \r -> "A return flight: out " <> formatDate r.out <> ", back " <> formatDate r.back
  }

submit
  :: { "Flight type" :: [ "one-way" :: {}, return :: {} ]
     , "Return date (DD.MM.YYYY)" :: String
     , "Start date (DD.MM.YYYY)" :: String
     }
  -> Aff [ "Booking rejected" :: {}, "Flight booked" :: {} ]
submit trip = case parse trip of
  Left _ -> pure (."Booking rejected" {})
  Right itinerary -> expand <$> bookFlight itinerary

bookFlight :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] -> Aff [ "Flight booked" :: {} ]
bookFlight _ = pure (."Flight booked" {})

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
- `bookingState` — the classifier behind the three panes: one of
  three exclusive states, the cases bare since the panes show the
  model itself. A problem is a reason (`.returnBeforeStart {}`,
  `.unreadableStart { input }`) `parse` finds, never a sentence: the
  copy functions spell it out.
- `problemLine`/`oneWayLine`/`returnLine` — the panes' copy functions,
  each reading the model, glue and warning glyph included;
  `problemLine` shares `parse` with the classifier, so what it says is
  exactly what the pane was shown for.
- `submit` — the `Aff` boundary. It shares `parse` with `bookingState`, so
  what the live line calls a problem is precisely what Book refuses; its
  outcome row names the two outcomes as the snackbars' words, each
  carrying `{}`, since nothing reads a payload the statuses do not show.

**Three things worth noticing.** The rows are spelled out in full in the
view model module — the model row seven times, the itinerary variant
four — because
application code declares no `type` synonyms: the shape is the interface
(writing.md *Code style* → *Types and values*). Every exported function
carries the signature the view reported for it with its tails filled —
the view named the three fields its editors bind and left the rest
unknown, and the signature closes the row: the model row its line is fed, its fields in the compiler's alphabetical
order — so the pane copy functions take the whole model and derive what
they show from it (`oneWayLine` parses the start date again), while the
private helpers (`parse`, `returnBetween`, `formatDate`, `dateKey`) keep
open rows of their own, since no view line types them (writing.md *Code
style* → *Business functions*). And `parseDate` has a real `do` —
`Maybe`'s monad: `Semigroupoid.do` in the view composes stages, `do` in
the view model module is the ordinary one.

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
- **todo-list** — a selectable list, `listOf @"Todo toggled" @"key"`, a filter
  selector, and panes over `remainingItems`.
- **checkout** — a wizard: the step is a model field, each step's line a
  pane `# inCaseAt @c @"step"`, Next and Back each a button shown by its
  own classifier and folded by its own handler (`stepOnward`, `stepBack`).
- **crud** — a load action before the knot, `indeterminateLinearProgress #
  load loadPeopleCatalogue`, its outcome folded in by `capture @"People
  loaded"`, run by `body`'s one feed of
  `{}`; create/update/delete are chains in the fold block, each an action
  owning its button's case and naming its two outcomes, followed by
  their folds.
- **tic-tac-toe** — a reset is a restart: `openingPosition` is the seed and
  `reset @"New game" openingPosition` the reset.
- **order-form** — all four shapes on one screen: a `looped` form in
  labelled groups inside the app's loop, a choice with a field per case
  (`tabBar @"Mode"` and its `inCaseAt` panes), the live summary, a
  `RecordToVariant.do` of buttons, an estimate joined with the model it
  lands on, and the folds and statuses.

When the word for something is missing, [vocabulary.md](vocabulary.md)
goes from the need to a word, a demo using it, and where to read more.
