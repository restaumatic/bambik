module Stopwatch.MDC2 (stopwatch) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (actedOf, fold, foldInto, looped, stepped, with)
import PUI.Web (providedAt, text, timed, timing)
import PUI.Web.HTML (li, ul)
import PUI.Web.MDC2 (body, button', headline3)
import QualifiedDo.Semigroupoid as Semigroupoid
import Stopwatch (beginTiming, clearStopwatch, elapsedLine, haltTiming, lapLine, lapRows, recordLap, tickPeriod, zeroedStopwatch)

stopwatch :: Effect Unit
stopwatch =
  body $ Semigroupoid.do
    ( headline3 $ text elapsedLine ) # timed @"time" tickPeriod
    ( Semigroupoid.do
      RecordToVariant.do
        button' @"Start" { icon: "play_arrow" } # timing @"time" # providedAt @"halted" @"phase"
        button' @"Stop" { icon: "stop" } # timing @"time" # providedAt @"timing" @"phase"
        button' @"Lap" { icon: "flag" } # timing @"time" # providedAt @"timing" @"phase"
        button' @"Reset" { icon: "replay" } # providedAt @"halted" @"phase"
      VariantToRecord.do
        foldInto @"Start" beginTiming
        foldInto @"Stop" haltTiming
        foldInto @"Lap" recordLap
        fold @"Reset" clearStopwatch ) # stepped
    ul $ ( li $ text lapLine ) # actedOf @"number" lapRows
  # looped # with zeroedStopwatch
