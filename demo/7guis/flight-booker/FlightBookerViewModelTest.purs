module FlightBookerViewModelTest (flightBookerClaims) where

import Prelude ((==))

import FlightBookerViewModel (bookingState, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine)

flightBookerClaims :: Array { claim :: String, holds :: Boolean }
flightBookerClaims =
  [ { claim: "the planned trip is a one-way flight on its start date", holds: bookingState plannedTrip == ."one-way" { out: { y: 2026, m: 3, d: 27 } } }
  , { claim: "a return before the start is a problem", holds: bookingState backBeforeOut == .problem { problem: .returnBeforeStart {} } }
  , { claim: "a date that does not parse is named in the problem", holds: bookingState badStart == .problem { problem: .unreadableStart { input: "32.03.2026" } } }
  , { claim: "a one-way line names its date", holds: oneWayLine { out: { y: 2026, m: 3, d: 27 } } == "A one-way flight on 27.03.2026" }
  , { claim: "a return line names both dates", holds: returnLine { out: { y: 2026, m: 3, d: 27 }, back: { y: 2026, m: 4, d: 3 } } == "A return flight: out 27.03.2026, back 03.04.2026" }
  , { claim: "a problem line spells the problem out, flagged", holds: problemLine { problem: .returnBeforeStart {} } == "⚠ the return date is before the start date" }
  , { claim: "a rejection says why it cannot book", holds: rejectedLine (.unreadableReturn { input: "soon" }) == "Cannot book: return date \"soon\" is not a valid DD.MM.YYYY date" }
  ]
  where
  backBeforeOut = plannedTrip { "Flight type" = ."return" {}, "Return date" = "26.03.2026" }
  badStart = plannedTrip { "Start date" = "32.03.2026" }
