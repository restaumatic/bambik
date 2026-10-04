module StopwatchMDC3 (stopwatchMDC3) where

import Prelude (Unit, const, (#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, updated)
import PUI.Web (provided, shown, shownEach, text)
import PUI.Web.HTML (li, ul)
import PUI.Web.MDC3 (body, button, displaySmall)
import QualifiedDo.Semigroupoid as Semigroupoid
import StopwatchViewModel (beginTiming, clearStopwatch, elapsedText, haltTiming, lapLine, lapRows, recordLap, tick, tickPeriod, zeroedStopwatch)

stopwatchMDC3 :: Effect Unit
stopwatchMDC3 =
  body $
    ( Semigroupoid.do
      displaySmall (text elapsedText) # shown
      every tickPeriod tick
      ( RecordToVariant.do
        button @"Start" { icon: "play_arrow" } # provided @"halted" _.phase
        button @"Stop" { icon: "stop" } # provided @"timing" _.phase ) # updated (match { "Start": const beginTiming, "Stop": const haltTiming })
      ( RecordToVariant.do
        button @"Lap" { icon: "flag" } # provided @"timing" _.phase
        button @"Reset" { icon: "replay" } # provided @"halted" _.phase ) # updated (match { "Lap": const recordLap, "Reset": const clearStopwatch })
      ul $ ( li $ text lapLine ) # shownEach @"number" @( number :: Int, tenths :: Int ) lapRows
    ) # mvu @( phase :: [ halted :: {}, timing :: {} ], elapsedTenths :: Int, laps :: Array Int ) zeroedStopwatch
