# Flight-booker, line by line

The 7GUIs flight booker: a one-way/return selector, one or two date fields,
a live line describing the itinerary (or what is wrong with it), a Book
button, and a confirmation. It is the smallest demo that crosses three of
the four directions — editors (`×→×`), an event (`×→+`), an action (`+→+`)
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

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, submit, tripType)
import PUI (action, debounced, forCases, mvu, required)
import PUI.Web (choice)
import PUI.Web.HTML (inCase, shownCase, body, staticText, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Pipeline

flightBookerMDC2 :: Effect Unit
flightBookerMDC2 =
  body $
    elevation20 $
      card $ Pipeline.do
      ( Pipeline.do
          select @"Flight type" {}
            [ choice @"one-way", choice @"return" ] # required
          filledTextField @"Start date (DD.MM.YYYY)" {}
          filledTextField @"Return date (DD.MM.YYYY)" {} # inCase @"return" tripType
      ) # mvu plannedTrip
      ( Pipeline.do
          ( body1 $ RecordToRecord.do
              staticText "⚠ "
              text @"problem" ) # shownCase @"problem" bookingState
          ( body1 $ RecordToRecord.do
              staticText "A one-way flight on "
              text @"date" ) # shownCase @"one-way" bookingState
          ( body1 $ RecordToRecord.do
              staticText "A return flight: out "
              text @"out"
              staticText ", back "
              text @"back" ) # shownCase @"return" bookingState ) # debounced itinerarySettleTime
      button @"Book" { icon: "flight_takeoff" }
      indeterminateLinearProgress @"busy" # action (match { "Book": submit })
      snackbar # forCases bookingLine
```

**The imports.** Three vocabularies and nothing else: `PUI` for the words
that shape data flow (`mvu`, `required`, `debounced`, `action`, `forCases`),
`PUI.Web.HTML` for the page and the display rungs (`body`, `shownCase`,
`inCase`, `text`, `staticText`), and `PUI.Web.MDC2` for the design system.
The MDC3 twin differs from this file in exactly the last import (and the
typography names it pulls from it); the logic module is shared verbatim.
`RecordToRecord` is the one merge this screen needs — chrome and a field
side by side. `QualifiedDo.Semigroupoid as Pipeline` gives `Pipeline.do`:
sequential composition, not a monad.

**`body $ elevation20 $ card $ Pipeline.do`.** Mount at the document body;
`elevation20` and `card` are *oculars* — visual wrappers that touch no data,
which is why they are applied with `$`, the visual plumbing, and never with
`#`, the data plumbing. The outer `Pipeline.do` has five stages, and data
flows top to bottom exactly as the code reads: the form emits the model on
every edit → the itinerary line shows it and passes it on → the button turns
it into an event → the action turns the event into an outcome → the snackbar
shows the outcome. Code order is DOM order *and* data order.

**Stage 1 — the form.** An inner `Pipeline.do` of three editors, closed with
`# mvu plannedTrip`.

- `select @"Flight type" {} [ choice @"one-way", choice @"return" ] # required`
  — the leaf's type argument is the caption *and* the model field, so this
  edits `{ "Flight type" :: [ "one-way" :: {}, "return" :: {} ] }`. Each
  `choice @l` states an option's copy once, as its case. `# required` says a
  selection always exists: the model holds the variant, not a `Maybe`.
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

- `( body1 $ RecordToRecord.do staticText "A one-way flight on "; text @"date" )`
  — a *merge*: chrome and a field reading **one record**, the pane's payload
  `{ date :: String }`. Merges are for things that share a value; pipelines
  for things that follow each other.
- `# shownCase @"one-way" bookingState` — attach and feed this pane when
  `bookingState model` yields case `one-way`, with that case's payload;
  detach on any other case. Either way the fed model is released downstream:
  a hidden pane never blocks the flow. Three rungs over one classifier make
  the three states exclusive by construction — exclusivity is computed in
  `bookingState`, not arranged in the view.
- `# debounced { ms: 300.0 }` — redraw the whole line 300 ms after the last
  edit; the duration is model data (`itinerarySettleTime`), not a literal.

The stage's type is model → model: a display *is* a pass-through stage.

**Stage 3 — `button @"Book" { icon: "flight_takeoff" }`.** The first
direction change, `×→+`: fed the model, it emits `[ "Book" :: model ]` on
click, replaying the last model it was fed. Its case is its caption; `icon`
is presentation config.

**Stage 4 — `indeterminateLinearProgress @"busy" # action (match { "Book": submit })`.**
`+→+`: the event's payload goes to `submit :: model -> Aff [ booked :: …,
rejected :: String ]`, the progress bar shows while the `Aff` runs, and the
outcome variant emits when it settles.

**Stage 5 — `snackbar # forCases bookingLine`.** `+→×`: one snackbar serves
both outcomes; `bookingLine` renders each case to its line of copy. Its
output is `{}`, which is where every pipeline must end — no emission is
ever dropped silently.

## The logic

```purescript
module FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, submit, tripType) where

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

bookingLine :: [ booked :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ], rejected :: String ] -> String
bookingLine = match
  { booked: \itinerary -> "You have booked: " <> summary itinerary
  , rejected: \problem -> "Cannot book: " <> problem
  }

returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } -> Maybe [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
returnBetween { out, back } =
  if dateKey back >= dateKey out then Just (.returnBetween { out, back })
  else Nothing

parse :: { "Flight type" :: [ "one-way" :: {}, "return" :: {} ], "Start date (DD.MM.YYYY)" :: String, "Return date (DD.MM.YYYY)" :: String } -> Either String [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
parse { "Flight type": flightType, "Start date (DD.MM.YYYY)": startInput, "Return date (DD.MM.YYYY)": returnInput } = case parseDate startInput of
  Nothing -> Left ("start date " <> show startInput <> " is not a valid DD.MM.YYYY date")
  Just start ->
    if flightType /= ."return" {} then Right (.oneWayOn start)
    else case parseDate returnInput of
        Nothing -> Left ("return date " <> show returnInput <> " is not a valid DD.MM.YYYY date")
        Just back -> case returnBetween { out: start, back } of
          Nothing -> Left "the return date is before the start date"
          Just itinerary -> Right itinerary

bookingState :: { "Flight type" :: [ "one-way" :: {}, "return" :: {} ], "Start date (DD.MM.YYYY)" :: String, "Return date (DD.MM.YYYY)" :: String } -> [ problem :: { problem :: String }, "one-way" :: { date :: String }, "return" :: { out :: String, back :: String } ]
bookingState = parse >>> either (\problem -> .problem { problem })
  (match
    { oneWayOn: \out -> ."one-way" { date: formatDate out }
    , returnBetween: \r -> ."return" { out: formatDate r.out, back: formatDate r.back }
    })

summary :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] -> String
summary = match
  { oneWayOn: \out -> "A one-way flight on " <> formatDate out
  , returnBetween: \r -> "A return flight: out " <> formatDate r.out <> ", back " <> formatDate r.back
  }

submit :: { "Flight type" :: [ "one-way" :: {}, "return" :: {} ], "Start date (DD.MM.YYYY)" :: String, "Return date (DD.MM.YYYY)" :: String } -> Aff [ booked :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ], rejected :: String ]
submit { "Flight type": flightType, "Start date (DD.MM.YYYY)": start, "Return date (DD.MM.YYYY)": back } = case parse { "Flight type": flightType, "Start date (DD.MM.YYYY)": start, "Return date (DD.MM.YYYY)": back } of
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

formatDate :: { y :: Int, m :: Int, d :: Int } -> String
formatDate { y, m, d } = pad d <> "." <> pad m <> "." <> show y
  where
  pad n = (if n < 10 then "0" else "") <> show n

dateKey :: { y :: Int, m :: Int, d :: Int } -> Int
dateKey { y, m, d } = y * 10000 + m * 100 + d

tripType :: { "Flight type" :: [ "one-way" :: {}, "return" :: {} ] } -> [ "one-way" :: {}, "return" :: {} ]
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
- `bookingState` — the classifier behind the three `shownCase` panes. It
  turns the model into one of three exclusive display states, each carrying
  exactly the payload its pane shows (`{ date }`, `{ out, back }`,
  `{ problem }`), so a pane's `text @"date"` is typed against it.
- `submit` — the `Aff` boundary. `parse` is shared with `bookingState`, so
  what the live line calls a problem is precisely what Book refuses.
- `bookingLine` — the copy function behind `snackbar # forCases`: every
  outcome case to its sentence.

**Two things worth noticing.** The rows are spelled out in full, eight
times for the itinerary variant — deliberately: there are no `type`
synonyms in application code, the shape *is* the interface, and the price
of that is paid here in repetition (writing.md, *Types and values*). And
`parseDate` has a real `do` — `Maybe`'s monad — which is the contrast to
keep in mind: `Pipeline.do` in the view is composition of stages, `do` in
the logic is the ordinary one.

## What to read next

- **counter** — the floor: one display, one button, one fold (`updated`).
- **temperature-converter** — two editors kept consistent with `settled`.
- **flight-booker** — this file.
- **todomvc** — a collection (`listOf`, `foreach`), a selectable list emitting
  its key with `toCase @l _.key`, a filter selector.
- **checkout** — a wizard: `folding` loops the step state silently, and two
  buttons carry their own cases into one loop case with `toCases`.
- **order-form** — all four directions in one screen: `looped` form with
  nested `field @l` sub-records, a variant editor with `bracketed`, the
  debounced summary, `armed` buttons, dispatch, statuses.

When a screen needs something and the word for it is missing,
[vocabulary.md](vocabulary.md) goes from the need to the word and to the
place its rule is stated.
