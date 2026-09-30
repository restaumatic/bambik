# Flight-booker, line by line

The 7GUIs flight booker: a one-way/return selector, one or two date fields,
a live line describing the itinerary (or what is wrong with it), a Book
button, and a confirmation. It is the smallest demo that crosses three of
the four shapes — editors (`×→×`), an event (`×→+`), an action (`+→+`)
and a status (`+→×`) — so once it reads plainly, every larger demo is the
same moves repeated. The view is
`demo/7guis/flight-booker-mdc2/FlightBookerMDC2.purs`, the logic
`demo/7guis/flight-booker/FlightBookerLogic.purs`; run it with
`npm run dev flight-booker-mdc2`. Every word used here has a row in
[vocabulary.md](vocabulary.md) and its rule in [writing.md](writing.md).

## The view

```purescript
module FlightBookerMDC2 (flightBookerMDC2) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBookerLogic (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit, tripType)
import PUI (action, atCase, debounced, mvu)
import PUI.Web (choice, inCase, shownWhen, text)
import PUI.Web.MDC2 (body, body1, button, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerMDC2 :: Effect Unit
flightBookerMDC2 =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type" {}
        [ choice @"one-way", choice @"return" ]
      filledTextField @"Start date (DD.MM.YYYY)" {}
      filledTextField @"Return date (DD.MM.YYYY)" {} # inCase @"return" tripType
    ) # mvu plannedTrip
    ( Semigroupoid.do
      body1 (text problemLine) # shownWhen @"problem" bookingState
      body1 (text oneWayLine) # shownWhen @"one-way" bookingState
      body1 (text returnLine) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
    button @"Book" { icon: "flight_takeoff" }
    indeterminateLinearProgress @"Booking flight" # action submit # atCase @"Book"
    VariantToRecord.do
      snackbar @"booked" bookedLine
      snackbar @"rejected" rejectedLine
```

**The imports.** Three vocabularies and one merge sugar: `PUI` for the
words that shape data flow (`mvu`, `debounced`, `action`, `atCase`),
`PUI.Web` for the words every vocabulary shares (`choice`, and the display
stages `shownWhen`, `inCase`, `text`), `VariantToRecord` for the qualified-do
that sets the two statuses side by side, and `PUI.Web.MDC2` for the design
system — its `body` included:
every vocabulary exports the entry under that one name and signature,
dressing the page for its catalogue before it mounts.
The MDC3 twin differs from this file in exactly the last import (and the
typography names it pulls from it); the logic module is shared verbatim.
No merge block appears: each displayed line is one read function at one
leaf, so no stage here reads more than one leaf. `QualifiedDo.Semigroupoid as Semigroupoid`
gives `Semigroupoid.do`: sequential composition, not a monad.

**`body $ Semigroupoid.do`.** Mount at the document body, dressed for
Material 2 (the `body` is the vocabulary's) and applied with `$`, the
visual plumbing, never with `#`, the data plumbing. There is no wrapper:
the app takes no surface of its own — the page it is shown on frames it —
and inside it a card holds either a model sub-record (`group @l`) or
content that edits nothing (a summary card) — this flat booking needs
neither. The outer `Semigroupoid.do` has five stages, and data
flows top to bottom exactly as the code reads: the form emits the model on
every edit → the itinerary line shows it and passes it on → the button turns
it into an event → the action turns the event into an outcome → the snackbar
shows the outcome. Code order is DOM order *and* data order.

**Stage 1 — the form.** An inner `Semigroupoid.do` of three editors, closed with
`# mvu plannedTrip`.

- `select @"Flight type" {} [ choice @"one-way", choice @"return" ]`
  — the leaf's type argument is the caption *and* the model field, so this
  edits `{ "Flight type" :: [ "one-way" :: {}, "return" :: {} ] }`. Each
  `choice @l` states an option's copy once, as its case. The plain
  `select` says a selection always exists: the model holds the variant
  itself, not a `Maybe` — the selector is an editor of that field,
  answering every feed (`selectUnpicked` would hold a choice owed but not
  yet made, `selectOptional` one the user may leave unmade).
- `filledTextField @"Start date (DD.MM.YYYY)" {}` — the label carries the
  whole copy, format hint included; the `{}` is empty presentation config.
- `filledTextField @"Return date (DD.MM.YYYY)" {} # inCase @"return" tripType`
  — this editor *exists* only while `tripType model` yields case `return`;
  detached otherwise, the model passing straight through. `tripType` is a
  business function, so what "return" means lives in the logic module.

Every editor is a **whole-row stage**: fed the whole record, it emits the
whole record with its own field changed. `mvu plannedTrip` seeds the record
at registration and loops each emission back to the top, so all three
editors see every change. It also closes the stage's input to `{}` — the
compile-time proof that nothing here waits for a seed nobody supplies.

**Stage 2 — the itinerary line.** Three panes, one visible at a time,
under one `# debounced itinerarySettleTime`.

- `body1 (text oneWayLine)` — the leaf takes the **read function**, not a
  label: its content *is* the copy, so there is no field to name. The whole
  sentence ("A one-way flight on 27.03.2026") is `oneWayLine`, one pure
  function in the logic module (writing.md, *copy is a function, not a
  field*): the copy is under unit test, the view holds no glue, and the line
  names its own writer.
- `# shownWhen @"one-way" bookingState` — attach and feed this pane when
  `bookingState model` yields case `one-way`, with that case's payload
  `{ out :: { y, m, d } }` — the **source** data the line is computed from,
  not a rendering of it; detach on any other case. Either way the fed
  model is released downstream: a hidden pane never blocks the flow. Three
  such stages over one classifier make the three states exclusive by
  construction — exclusivity is computed in `bookingState`, not arranged
  in the view.
- `# debounced { ms: 300.0 }` — redraw the whole line 300 ms after the last
  edit; the duration is model data (`itinerarySettleTime`), not a literal.

The stage's type is model → model: a display *is* a pass-through stage.

**Stage 3 — `button @"Book" { icon: "flight_takeoff" }`.** The first
shape change, `×→+`: fed the model, it emits `[ "Book" :: model ]` on
click, replaying the last model it was fed. Its case is its caption; `icon`
is presentation config.

**Stage 4 — `indeterminateLinearProgress @"Booking flight" # action submit # atCase @"Book"`.**
`+→+`: `atCase` adopts the button's case, so its payload goes to
`submit :: model -> Aff [ booked :: …, rejected :: String ]`, the progress
bar shows while the `Aff` runs, and the outcome variant emits when it
settles.

**Stage 5 — `snackbar @"booked" bookedLine` beside `snackbar @"rejected"
rejectedLine`.** `+→×`: a `VariantToRecord.do` of two statuses, each owning
one outcome case of `submit`'s variant and rendering it with its copy
function — the status's counterpart of a display's read function. Their
merged output is `{}`, which is where every pipeline must end — no emission is
ever dropped silently.

## The logic

```purescript
module FlightBookerLogic (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit, tripType) where

import Prelude ((&&), (*), (+), (/=), (<), (<$>), (<=), (<>), (>=), (>>>), bind, pure, show)

import Data.Either (Either(..), either)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Variant (expand, match)
import Effect.Aff (Aff)

plannedTrip :: { "Flight type" :: [ "one-way" :: {}, "return" :: {} ], "Start date (DD.MM.YYYY)" :: String, "Return date (DD.MM.YYYY)" :: String }
plannedTrip = { "Flight type": ."one-way" {}, "Start date (DD.MM.YYYY)": "27.03.2026", "Return date (DD.MM.YYYY)": "27.03.2026" }

itinerarySettleTime :: { ms :: Number }
itinerarySettleTime = { ms: 300.0 }

bookedLine :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] -> String
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

bookingState :: forall r1. { "Flight type" :: [ "one-way" :: {}, "return" :: {} ], "Start date (DD.MM.YYYY)" :: String, "Return date (DD.MM.YYYY)" :: String | r1 } -> [ problem :: { problem :: String }, "one-way" :: { out :: { y :: Int, m :: Int, d :: Int } }, "return" :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
bookingState = parse >>> either (\problem -> .problem { problem })
  (match
    { oneWayOn: \out -> ."one-way" { out }
    , returnBetween: ."return"
    })

problemLine :: forall r1. { problem :: String | r1 } -> String
problemLine { problem } = "⚠ " <> problem

oneWayLine :: forall r1. { out :: { y :: Int, m :: Int, d :: Int } | r1 } -> String
oneWayLine { out } = summary (.oneWayOn out)

returnLine :: forall r1. { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } | r1 } -> String
returnLine r = summary (.returnBetween { out: r.out, back: r.back })

summary :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] -> String
summary = match
  { oneWayOn: \out -> "A one-way flight on " <> formatDate out
  , returnBetween: \r -> "A return flight: out " <> formatDate r.out <> ", back " <> formatDate r.back
  }

submit :: forall r1. { "Flight type" :: [ "one-way" :: {}, "return" :: {} ], "Start date (DD.MM.YYYY)" :: String, "Return date (DD.MM.YYYY)" :: String | r1 } -> Aff [ booked :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ], rejected :: String ]
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

tripType :: forall r1. { "Flight type" :: [ "one-way" :: {}, "return" :: {} ] | r1 } -> [ "one-way" :: {}, "return" :: {} ]
tripType = _."Flight type"
```

**No library in sight.** The module imports the domain — `Prelude`,
`Maybe`, `Either`, `Aff`, `Data.Variant` — and nothing from `PUI`. The
export list is exactly what the view imports; everything else is a private
helper. It compiles and tests without a browser.

**The exports, in the order the view uses them.**

- `plannedTrip` — the seed `mvu` feeds at registration. Its keys are the
  leaves' labels, quoted because they are copy (`"Start date (DD.MM.YYYY)"`),
  and its variant field is written with the constructor sugar `."one-way" {}`
  (the forked compiler's `.label` form; the type `[ … ]` is its type sugar).
- `itinerarySettleTime` — a duration is a structural `{ ms :: Number }`, held
  in the logic so the view carries no literal.
- `tripType` — the classifier behind `# inCase @"return"`: a one-field read
  returning the variant, so "the return date exists in return trips" is a
  business statement, not a view condition.
- `bookingState` — the classifier behind the three `shownWhen` panes. It
  turns the model into one of three exclusive display states, each carrying
  exactly the source data its pane's line is computed from (`{ out }`,
  `{ out, back }`, `{ problem }`), so a pane's `text oneWayLine` is typed
  against it and the copy — glue, warning glyph and all — is one testable
  function beside it.
- `submit` — the `Aff` boundary. `parse` is shared with `bookingState`, so
  what the live line calls a problem is precisely what Book refuses.
- `bookedLine`/`rejectedLine` — the copy functions the two snackbars
  take: each outcome case to its sentence.

**Three things worth noticing.** The rows are spelled out in full, eight
times for the itinerary variant — deliberately: there are no `type`
synonyms in application code, the shape *is* the interface, and the price
of that is paid here in repetition (writing.md, *Types and values*). Every
record a function reads is an **open row** — `forall r1. { out :: …, back
:: … | r1 }` — the fields it names and a tail for the rest of whatever it
is fed: the footprint is exact by parametricity, and the view type-checks
while these functions are still holes (writing.md, *Footprints as open
rows*); a function handing its argument on to a closed helper projects it
(`returnLine r = summary (.returnBetween { out: r.out, back: r.back })`). And
`parseDate` has a real `do` — `Maybe`'s monad — which is the contrast to
keep in mind: `Semigroupoid.do` in the view is composition of stages, `do` in
the logic is the ordinary one.

## What to read next

- **counter** — the floor: one display reading one function
  (`headline4 (text countLine) # shown`), one button, one fold (`applied`).
  Its whole model is `{ count :: Int }` — every display is a function, so
  nothing in the row exists for the screen's benefit.
- **timer** — the same at two displays of different sorts:
  `progressBar @"Elapsed" elapsedFraction` and `text progressLine` over a
  model of `{ "Duration", elapsed }`, both derived, neither stored.
- **temperature-converter** — two editors kept consistent with `settled`:
  the surviving job of `settled`, an invariant among *edited* fields.
- **flight-booker** — this file.
- **todo-list** — a collection (`listOf`, `foreach`), a selectable list emitting
  its key as `listOf @l @"key"`, a filter selector.
- **checkout** — a wizard: `folding @"next" @"step"` loops the step state silently, and two
  buttons each emit their own case, introduced into one loop case with
  `toCase`.
- **order-form** — all four shapes in one screen: `looped` form with
  labelled groups (`group @"Customer" $ …`), a variant editor with
  `bracketed`, the debounced summary, `armed` buttons, each action with its statuses.

When a screen needs something and the word for it is missing,
[vocabulary.md](vocabulary.md) goes from the need to the word and to the
place its rule is stated.
