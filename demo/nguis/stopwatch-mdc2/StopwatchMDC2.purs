module StopwatchMDC2 (stopwatchMDC2) where

import Prelude ((#), ($), Unit, identity)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (fold, joined, mvu, replaying, ticks)
import PUI.Web (provided, shown, shownEach, text)
import PUI.Web.HTML (li, ul)
import PUI.Web.MDC2 (body, button, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid
import StopwatchViewModel (beginTiming, clearStopwatch, elapsedText, haltTiming, lapLine, lapRows, recordLap, tick, tickPeriod, zeroedStopwatch)

stopwatchMDC2 :: Effect Unit
stopwatchMDC2 =
  body $
    ( Semigroupoid.do
      headline3 (text elapsedText) # shown
      RecordToVariant.do
        ticks @"tick" tickPeriod # replaying @"tick" identity
        button @"Start" { icon: "play_arrow" } # provided @"halted" _.phase # joined @"Start"
        button @"Stop" { icon: "stop" } # provided @"timing" _.phase # joined @"Stop"
        button @"Lap" { icon: "flag" } # provided @"timing" _.phase # joined @"Lap"
        button @"Reset" { icon: "replay" } # provided @"halted" _.phase # joined @"Reset"
      VariantToRecord.do
        fold @"tick" tick
        fold @"Start" beginTiming
        fold @"Stop" haltTiming
        fold @"Lap" recordLap
        fold @"Reset" clearStopwatch
      ul $ ( li $ text lapLine ) # shownEach @"number" @( number :: Int, tenths :: Int ) lapRows
    ) # mvu @( phase :: [ halted :: {}, timing :: {} ], elapsedTenths :: Int, laps :: Array Int ) zeroedStopwatch
