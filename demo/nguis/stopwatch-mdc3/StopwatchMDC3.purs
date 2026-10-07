module StopwatchMDC3 (stopwatchMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (blankStatus, fold, joined, looped, ticks, with)
import PUI.Web (provided, shown, shownEach, text)
import PUI.Web.HTML (li, ul)
import PUI.Web.MDC3 (body, button, displaySmall, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid
import StopwatchViewModel (beginTiming, clearStopwatch, elapsedText, haltTiming, lapLine, lapRecordedLine, lapRows, recordLap, stopwatchResetLine, tick, tickPeriod, timingBegunLine, timingHaltedLine, zeroedStopwatch)

stopwatchMDC3 :: Effect Unit
stopwatchMDC3 =
  body $
    Semigroupoid.do
      displaySmall (text elapsedText) # shown
      RecordToVariant.do
        blankStatus @"Clock ticked" # ticks tickPeriod
        button @"Start" { icon: "play_arrow" } # provided @"halted" _.phase # joined @"Start"
        button @"Stop" { icon: "stop" } # provided @"timing" _.phase # joined @"Stop"
        button @"Lap" { icon: "flag" } # provided @"timing" _.phase # joined @"Lap"
        button @"Reset" { icon: "replay" } # provided @"halted" _.phase # joined @"Reset"
      VariantToRecord.do
        blankStatus @"Clock ticked" # fold tick
        snackbar @"Start" timingBegunLine # fold beginTiming
        snackbar @"Stop" timingHaltedLine # fold haltTiming
        snackbar @"Lap" lapRecordedLine # fold recordLap
        snackbar @"Reset" stopwatchResetLine # fold clearStopwatch
      ul $ ( li $ text lapLine ) # shownEach @"number" @( number :: Int, tenths :: Int ) lapRows
    # looped @( phase :: [ halted :: {}, timing :: {} ], elapsedTenths :: Int, laps :: Array Int ) # with zeroedStopwatch
