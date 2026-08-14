module StopwatchMDC3 (stopwatchMDC3) where

import Prelude (Unit, const, identity, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, displayed, every, projection, foreach, mvu, updated)
import PUI.Web.HTML (providedCase, body, li, staticText, text, ul)
import PUI.Web.MDC3 (button, card, elevation5, displaySmall)
import QualifiedDo.Semigroupoid as Semigroupoid
import StopwatchLogic (beginTiming, clearStopwatch, formatTime, haltTiming, lapRows, recordLap, stopwatchPhase, tick, tickPeriod, zeroedStopwatch)

stopwatchMDC3 :: Effect Unit
stopwatchMDC3 =
  body $
    elevation5 $
      card { caption: "Stopwatch" } $ ( Semigroupoid.do
          displaySmall (text @"elapsedTenths") # projection formatTime # completed
          every tickPeriod tick
          ( RecordToVariant.do
              button @"start" { label: "Start", icon: "play_arrow" } # providedCase @"halted" stopwatchPhase
              button @"stop" { label: "Stop", icon: "stop" } # providedCase @"timing" stopwatchPhase) # updated (match { start: const (const beginTiming), stop: const (const haltTiming) })
          ( RecordToVariant.do
              button @"lap" { label: "Lap", icon: "flag" } # providedCase @"timing" stopwatchPhase
              button @"reset" { label: "Reset", icon: "replay" } # providedCase @"halted" stopwatchPhase) # updated (match { lap: const recordLap, reset: const (const clearStopwatch) })
          ul ( ( li $ RecordToRecord.do
                   staticText "Lap "
                   text @"number"
                   staticText " — "
                   text @"time" ) # foreach @"number" lapRows ) # displayed
      ) # mvu zeroedStopwatch
