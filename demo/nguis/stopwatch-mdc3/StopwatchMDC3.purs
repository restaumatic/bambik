module StopwatchMDC3 (stopwatchMDC3) where

import Prelude ((#), ($), (+), (<), (<>), Unit, const, not, show)

import Data.Array (mapWithIndex, snoc)
import Data.Int (quot, rem)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, completed, displayed, every, forField, forValue, foreach, mvu, projection, updates)
import PUI.HTML (body, li, provided, staticText, text, ul)
import PUI.MDC3 (button, card, elevation5, displaySmall)
import QualifiedDo.Semigroupoid as Semigroupoid

stopwatchMDC3 :: Effect Unit
stopwatchMDC3 =
  body $
    elevation5 $
      card { caption: "Stopwatch" } $ ( Semigroupoid.do
          displaySmall text # projection readout # completed
          every tickPeriod tick
          ( RecordToVariant.do
              button { label: "Start", icon: "play_arrow" } # asCase @"start" # provided whenHalted
              button { label: "Stop", icon: "stop" } # asCase @"stop" # provided whenRunning) # updates (match { start: const (const beginTiming), stop: const (const haltTiming) })
          ( RecordToVariant.do
              button { label: "Lap", icon: "flag" } # asCase @"lap" # provided whenRunning
              button { label: "Reset", icon: "replay" } # asCase @"reset" # provided whenHalted) # updates (match { lap: const recordLap, reset: const (const clearStopwatch) })
          ul ( ( li $ RecordToRecord.do
                   staticText "Lap "
                   text # forValue # forField @"number"
                   staticText " — "
                   text # forValue # forField @"time" ) # foreach @"number" lapRows ) # displayed
      ) # mvu zeroedStopwatch

beginTiming :: { running :: Boolean }
beginTiming = { running: true }

haltTiming :: { running :: Boolean }
haltTiming = { running: false }

recordLap
  :: { elapsedTenths :: Int, laps :: Array Int }
  -> { elapsedTenths :: Int, laps :: Array Int }
recordLap sw@{ laps, elapsedTenths } = sw { laps = snoc laps elapsedTenths }

clearStopwatch :: { elapsedTenths :: Int, laps :: Array Int }
clearStopwatch = { elapsedTenths: 0, laps: [] }

tick
  :: { running :: Boolean, elapsedTenths :: Int }
  -> Maybe { running :: Boolean, elapsedTenths :: Int }
tick sw@{ running, elapsedTenths } =
  if running then Just (sw { elapsedTenths = elapsedTenths + 1 })
  else Nothing

whenHalted :: { running :: Boolean } -> Maybe {}
whenHalted { running } = if not running then Just {} else Nothing

whenRunning :: { running :: Boolean } -> Maybe {}
whenRunning { running } = if running then Just {} else Nothing

readout :: { elapsedTenths :: Int } -> String
readout { elapsedTenths } = formatTime elapsedTenths

lapRows :: { laps :: Array Int } -> Array { number :: String, time :: String }
lapRows { laps } = mapWithIndex (\i t -> { number: show (i + 1), time: formatTime t }) laps

formatTime :: Int -> String
formatTime tenths =
  pad2 (tenths `quot` 600) <> ":" <> pad2 ((tenths `rem` 600) `quot` 10) <> "." <> show (tenths `rem` 10)

pad2 :: Int -> String
pad2 n = if n < 10 then "0" <> show n else show n

zeroedStopwatch :: { running :: Boolean, elapsedTenths :: Int, laps :: Array Int }
zeroedStopwatch = { running: false, elapsedTenths: 0, laps: [] }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 100.0 }
