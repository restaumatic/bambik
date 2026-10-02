module StopwatchMDC2 (stopwatchMDC2) where

import Prelude (Unit, const, (#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, state, updated)
import PUI.Web (provided, shown, shownEach, text)
import PUI.Web.HTML (li, ul)
import PUI.Web.MDC2 (body, button, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid
import StopwatchViewModel (beginTiming, clearStopwatch, elapsedText, haltTiming, lapLine, lapRows, recordLap, tick, tickPeriod, zeroedStopwatch)

stopwatchMDC2 :: Effect Unit
stopwatchMDC2 =
  body $
    ( Semigroupoid.do
      state @"phase" @[ halted :: {}, timing :: {} ]
      state @"elapsedTenths" @Int
      state @"laps" @(Array Int)
      headline3 (text elapsedText) # shown
      every tickPeriod tick
      ( RecordToVariant.do
        button @"Start" { icon: "play_arrow" } # provided @"halted" _.phase
        button @"Stop" { icon: "stop" } # provided @"timing" _.phase ) # updated (match { "Start": const beginTiming, "Stop": const haltTiming })
      ( RecordToVariant.do
        button @"Lap" { icon: "flag" } # provided @"timing" _.phase
        button @"Reset" { icon: "replay" } # provided @"halted" _.phase ) # updated (match { "Lap": const recordLap, "Reset": const clearStopwatch })
      ul $ ( Semigroupoid.do
        state @"tenths" @Int
        li $ text lapLine ) # shownEach @"number" @Int lapRows
    ) # mvu zeroedStopwatch
