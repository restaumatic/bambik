module StopwatchMDC3 (stopwatchMDC3) where

import Prelude (Unit, const, (#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, updated)
import PUI.Web.HTML (shown, shownEach, provided, body, li, text, ul)
import PUI.Web.MDC3 (button, card, elevation5, displaySmall)
import QualifiedDo.Category as Category
import StopwatchLogic (beginTiming, clearStopwatch, elapsedText, haltTiming, lapLine, lapRows, recordLap, stopwatchPhase, tick, tickPeriod, zeroedStopwatch)

stopwatchMDC3 :: Effect Unit
stopwatchMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          displaySmall (text elapsedText) # shown
          every tickPeriod tick
          ( RecordToVariant.do
              button @"Start" { icon: "play_arrow" } # provided @"halted" stopwatchPhase
              button @"Stop" { icon: "stop" } # provided @"timing" stopwatchPhase ) # updated (match { "Start": const (const beginTiming), "Stop": const (const haltTiming) })
          ( RecordToVariant.do
              button @"Lap" { icon: "flag" } # provided @"timing" stopwatchPhase
              button @"Reset" { icon: "replay" } # provided @"halted" stopwatchPhase ) # updated (match { "Lap": const recordLap, "Reset": const (const clearStopwatch) })
          ul $ ( li $ text lapLine ) # shownEach @"number" lapRows
      ) # mvu zeroedStopwatch
