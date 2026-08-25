module StopwatchMDC2 (stopwatchMDC2) where

import Prelude (Unit, const, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, updated)
import PUI.Web.HTML (shown, shownEach, providedCase, body, li, staticText, text, ul)
import PUI.Web.MDC2 (button, card, elevation20, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid
import StopwatchLogic (beginTiming, clearStopwatch, formatTime, haltTiming, lapRows, recordLap, stopwatchPhase, tick, tickPeriod, zeroedStopwatch)

stopwatchMDC2 :: Effect Unit
stopwatchMDC2 =
  body $
    elevation20 $
      card $ ( Semigroupoid.do
          headline3 (shown @"elapsedTenths" formatTime)
          every tickPeriod tick
          ( RecordToVariant.do
              button @"Start" { icon: "play_arrow" } # providedCase @"halted" stopwatchPhase
              button @"Stop" { icon: "stop" } # providedCase @"timing" stopwatchPhase) # updated (match { "Start": const (const beginTiming), "Stop": const (const haltTiming) })
          ( RecordToVariant.do
              button @"Lap" { icon: "flag" } # providedCase @"timing" stopwatchPhase
              button @"Reset" { icon: "replay" } # providedCase @"halted" stopwatchPhase) # updated (match { "Lap": const recordLap, "Reset": const (const clearStopwatch) })
          ul $ shownEach @"number" lapRows ( li $ RecordToRecord.do
                   staticText "Lap "
                   text @"number"
                   staticText " — "
                   text @"time" )
      ) # mvu zeroedStopwatch
