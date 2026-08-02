module StopwatchMDC2 (stopwatchMDC2) where

import Prelude (Unit, const, identity, show, (#), ($), (+), (<), (<>))

import Data.Array (mapWithIndex, snoc)
import Data.Int (quot, rem)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, completed, displayed, every, forField, foreach, mvu, updated)
import PUI.Web.HTML (atCase, body, li, staticText, text, ul)
import PUI.Web.MDC2 (button, card, elevation20, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid

stopwatchMDC2 :: Effect Unit
stopwatchMDC2 =
  body $
    elevation20 $
      card { caption: "Stopwatch" } $ ( Semigroupoid.do
          headline3 text # forField @"elapsedTenths" formatTime # completed
          every tickPeriod tick
          ( RecordToVariant.do
              button { label: "Start", icon: "play_arrow" } # asCase @"start" # atCase @"halted" stopwatchPhase
              button { label: "Stop", icon: "stop" } # asCase @"stop" # atCase @"timing" stopwatchPhase) # updated (match { start: const (const beginTiming), stop: const (const haltTiming) })
          ( RecordToVariant.do
              button { label: "Lap", icon: "flag" } # asCase @"lap" # atCase @"timing" stopwatchPhase
              button { label: "Reset", icon: "replay" } # asCase @"reset" # atCase @"halted" stopwatchPhase) # updated (match { lap: const recordLap, reset: const (const clearStopwatch) })
          ul ( ( li $ RecordToRecord.do
                   staticText "Lap "
                   text # forField @"number" identity
                   staticText " — "
                   text # forField @"time" identity ) # foreach @"number" lapRows ) # displayed
      ) # mvu zeroedStopwatch

beginTiming :: { phase :: [ halted :: {}, timing :: {} ] }
beginTiming = { phase: .timing {} }

haltTiming :: { phase :: [ halted :: {}, timing :: {} ] }
haltTiming = { phase: .halted {} }

recordLap
  :: { elapsedTenths :: Int, laps :: Array Int }
  -> { elapsedTenths :: Int, laps :: Array Int }
recordLap sw@{ laps, elapsedTenths } = sw { laps = snoc laps elapsedTenths }

clearStopwatch :: { elapsedTenths :: Int, laps :: Array Int }
clearStopwatch = { elapsedTenths: 0, laps: [] }

tick
  :: { phase :: [ halted :: {}, timing :: {} ], elapsedTenths :: Int }
  -> Maybe { phase :: [ halted :: {}, timing :: {} ], elapsedTenths :: Int }
tick sw@{ phase, elapsedTenths } =
  match { timing: \_ -> Just (sw { elapsedTenths = elapsedTenths + 1 }), halted: \_ -> Nothing } phase

stopwatchPhase :: { phase :: [ halted :: {}, timing :: {} ] } -> [ halted :: {}, timing :: {} ]
stopwatchPhase { phase } = phase

lapRows :: { laps :: Array Int } -> Array { number :: String, time :: String }
lapRows { laps } = mapWithIndex (\i t -> { number: show (i + 1), time: formatTime t }) laps

formatTime :: Int -> String
formatTime tenths =
  pad2 (tenths `quot` 600) <> ":" <> pad2 ((tenths `rem` 600) `quot` 10) <> "." <> show (tenths `rem` 10)

pad2 :: Int -> String
pad2 n = if n < 10 then "0" <> show n else show n

zeroedStopwatch :: { phase :: [ halted :: {}, timing :: {} ], elapsedTenths :: Int, laps :: Array Int }
zeroedStopwatch = { phase: .halted {}, elapsedTenths: 0, laps: [] }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 100.0 }
