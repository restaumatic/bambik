module PunchClock.MDC2 (punchClock) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (actedOf, foldInto, looped, stepped, with)
import PUI.Web (text, timestamped, timestamping)
import PUI.Web.MDC2 (body, button, filledTextField, headline6, list, listItem)
import PunchClock (clockLine, clockPeriod, emptyShift, punchIn, punchLine, punches)
import QualifiedDo.Semigroupoid as Semigroupoid

punchClock :: Effect Unit
punchClock =
  body $ Semigroupoid.do
    filledTextField @"Name"
    ( headline6 $ text clockLine ) # timestamped @"time" clockPeriod
    ( Semigroupoid.do
      button @"Punch in" # timestamping @"time"
      foldInto @"Punch in" punchIn ) # stepped
    ( list $ ( listItem $ text punchLine ) # actedOf @"number" punches ) # timestamped @"time" clockPeriod
  # looped # with emptyShift
