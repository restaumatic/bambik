module Stopwatch.MDC2 (stopwatch) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (actedOf, fold, looped, stepped, ticks, with)
import PUI.Web (providedAt, text)
import PUI.Web.HTML (li, ul)
import PUI.Web.MDC2 (body, button', headline3)
import QualifiedDo.Semigroupoid as Semigroupoid
import Stopwatch (beginTiming, clearStopwatch, elapsedLine, haltTiming, lapLine, lapRows, recordLap, tick, tickPeriod, zeroedStopwatch)

stopwatch :: Effect Unit
stopwatch =
  body $ Semigroupoid.do
    headline3 $ text elapsedLine
    ( Semigroupoid.do
      RecordToVariant.do
        ticks @"Clock ticked" tickPeriod
        button' @"Start" { icon: "play_arrow" } # providedAt @"halted" @"phase"
        button' @"Stop" { icon: "stop" } # providedAt @"timing" @"phase"
        button' @"Lap" { icon: "flag" } # providedAt @"timing" @"phase"
        button' @"Reset" { icon: "replay" } # providedAt @"halted" @"phase"
      VariantToRecord.do
        fold @"Clock ticked" tick
        fold @"Start" beginTiming
        fold @"Stop" haltTiming
        fold @"Lap" recordLap
        fold @"Reset" clearStopwatch ) # stepped
    ul $ ( li $ text lapLine ) # actedOf @"number" lapRows
  # looped # with zeroedStopwatch
