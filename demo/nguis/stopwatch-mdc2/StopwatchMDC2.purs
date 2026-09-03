module StopwatchMDC2 (stopwatchMDC2) where

import Prelude (Unit, const, (#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, updated)
import PUI.Web.HTML (shown, shownEach, provided, body, li, text, ul)
import PUI.Web.MDC2 (button, card, elevation20, headline3)
import QualifiedDo.Category as Category
import StopwatchLogic (beginTiming, clearStopwatch, elapsedText, haltTiming, lapLine, lapRows, recordLap, stopwatchPhase, tick, tickPeriod, zeroedStopwatch)

stopwatchMDC2 :: Effect Unit
stopwatchMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          headline3 (text elapsedText) # shown
          every tickPeriod tick
          ( RecordToVariant.do
              button @"Start" { icon: "play_arrow" } # provided @"halted" stopwatchPhase
              button @"Stop" { icon: "stop" } # provided @"timing" stopwatchPhase ) # updated (match { "Start": const (const beginTiming), "Stop": const (const haltTiming) })
          ( RecordToVariant.do
              button @"Lap" { icon: "flag" } # provided @"timing" stopwatchPhase
              button @"Reset" { icon: "replay" } # provided @"halted" stopwatchPhase ) # updated (match { "Lap": const recordLap, "Reset": const (const clearStopwatch) })
          ul $ ( li $ text lapLine ) # shownEach @"number" lapRows
      ) # mvu zeroedStopwatch
