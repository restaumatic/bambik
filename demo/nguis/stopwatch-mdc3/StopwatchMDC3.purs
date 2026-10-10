module StopwatchMDC3 (stopwatchMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (blankStatus, fold, joined, looped, ticks, with)
import PUI.Web (providedAt, shown, shownEach, text)
import PUI.Web.HTML (li, ul)
import PUI.Web.MDC3 (body, button, displaySmall)
import QualifiedDo.Semigroupoid as Semigroupoid
import StopwatchViewModel (beginTiming, clearStopwatch, elapsedText, haltTiming, lapLine, lapRows, recordLap, tick, tickPeriod, zeroedStopwatch)

stopwatchMDC3 :: Effect Unit
stopwatchMDC3 =
  body $ Semigroupoid.do
    displaySmall (text elapsedText) # shown
    RecordToVariant.do
      blankStatus @"Clock ticked" # ticks tickPeriod
      button @"Start" { icon: "play_arrow" } # providedAt @"halted" @"phase" # joined @"Start"
      button @"Stop" { icon: "stop" } # providedAt @"timing" @"phase" # joined @"Stop"
      button @"Lap" { icon: "flag" } # providedAt @"timing" @"phase" # joined @"Lap"
      button @"Reset" { icon: "replay" } # providedAt @"halted" @"phase" # joined @"Reset"
    VariantToRecord.do
      blankStatus @"Clock ticked" # fold tick
      blankStatus @"Start" # fold beginTiming
      blankStatus @"Stop" # fold haltTiming
      blankStatus @"Lap" # fold recordLap
      blankStatus @"Reset" # fold clearStopwatch
    ul $ ( li $ text lapLine ) # shownEach @"number" @( number :: Int, tenths :: Int ) lapRows
  # looped
    @( phase :: [ halted :: {}, timing :: {} ]
     , elapsedTenths :: Int
     , laps :: Array Int
     ) # with zeroedStopwatch
