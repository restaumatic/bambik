module Stopwatch.MDC2 (stopwatch) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (fold, foreach, looped, ticks, with)
import PUI.Web (providedAt, text)
import PUI.Web.HTML (li, ul)
import PUI.Web.MDC2 (body, button', headline3)
import QualifiedDo.Semigroupoid as Semigroupoid
import Stopwatch (beginTiming, clearStopwatch, elapsedText, haltTiming, lapLine, lapRows, recordLap, tick, tickPeriod, zeroedStopwatch)

stopwatch :: Effect Unit
stopwatch =
  body $ Semigroupoid.do
    headline3 $ text elapsedText
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
      fold @"Reset" clearStopwatch
    ul $ ( li $ text lapLine ) # foreach @"number" lapRows
  # looped # with zeroedStopwatch
