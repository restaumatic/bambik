module StopwatchMDC3 (stopwatchMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (fold, foreach, joined, looped, prefixed, suffixed, ticks, unshown, with)
import PUI.Web (providedAt, text)
import PUI.Web.HTML (li, ul)
import PUI.Web.MDC3 (body, buttonWith, displaySmall)
import QualifiedDo.Semigroupoid as Semigroupoid
import StopwatchViewModel (beginTiming, clearStopwatch, elapsedText, haltTiming, lapLine, lapRows, recordLap, tick, tickPeriod, zeroedStopwatch)

stopwatchMDC3 :: Effect Unit
stopwatchMDC3 =
  body $ Semigroupoid.do
    prefixed (displaySmall (text elapsedText)) $ RecordToVariant.do
      unshown @"Clock ticked" # ticks tickPeriod
      buttonWith @"Start" { icon: "play_arrow" } # providedAt @"halted" @"phase" # joined @"Start"
      buttonWith @"Stop" { icon: "stop" } # providedAt @"timing" @"phase" # joined @"Stop"
      buttonWith @"Lap" { icon: "flag" } # providedAt @"timing" @"phase" # joined @"Lap"
      buttonWith @"Reset" { icon: "replay" } # providedAt @"halted" @"phase" # joined @"Reset"
    ( VariantToRecord.do
      unshown @"Clock ticked" # fold tick
      unshown @"Start" # fold beginTiming
      unshown @"Stop" # fold haltTiming
      unshown @"Lap" # fold recordLap
      unshown @"Reset" # fold clearStopwatch ) # suffixed (ul $ ( li $ text lapLine ) # foreach @"number" @( number :: Int, tenths :: Int ) lapRows)
  # looped
    @( phase :: [ halted :: {}, timing :: {} ]
     , elapsedTenths :: Int
     , laps :: Array Int
     ) # with zeroedStopwatch
