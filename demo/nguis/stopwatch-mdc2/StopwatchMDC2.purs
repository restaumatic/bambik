module StopwatchMDC2 (stopwatchMDC2) where

import Prelude (Unit, const, identity, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, completed, displayed, every, forField, foreach, mvu, updated)
import PUI.Web.HTML (providedCase, body, li, staticText, text, ul)
import PUI.Web.MDC2 (button, card, elevation20, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid
import StopwatchLogic (beginTiming, clearStopwatch, formatTime, haltTiming, lapRows, recordLap, stopwatchPhase, tick, tickPeriod, zeroedStopwatch)

stopwatchMDC2 :: Effect Unit
stopwatchMDC2 =
  body $
    elevation20 $
      card { caption: "Stopwatch" } $ ( Semigroupoid.do
          headline3 text # forField @"elapsedTenths" formatTime # completed
          every tickPeriod tick
          ( RecordToVariant.do
              button { label: "Start", icon: "play_arrow" } # asCase @"clicked" @"start" # providedCase @"halted" stopwatchPhase
              button { label: "Stop", icon: "stop" } # asCase @"clicked" @"stop" # providedCase @"timing" stopwatchPhase) # updated (match { start: const (const beginTiming), stop: const (const haltTiming) })
          ( RecordToVariant.do
              button { label: "Lap", icon: "flag" } # asCase @"clicked" @"lap" # providedCase @"timing" stopwatchPhase
              button { label: "Reset", icon: "replay" } # asCase @"clicked" @"reset" # providedCase @"halted" stopwatchPhase) # updated (match { lap: const recordLap, reset: const (const clearStopwatch) })
          ul ( ( li $ RecordToRecord.do
                   staticText "Lap "
                   text # forField @"number" identity
                   staticText " — "
                   text # forField @"time" identity ) # foreach @"number" lapRows ) # displayed
      ) # mvu zeroedStopwatch
