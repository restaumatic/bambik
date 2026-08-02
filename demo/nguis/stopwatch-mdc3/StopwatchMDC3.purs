module StopwatchMDC3 (stopwatchMDC3) where

import Prelude (identity, (#), ($), (+), (<), (<>), Unit, const, not, show)

import Data.Array (mapWithIndex, snoc)
import Data.Int (quot, rem)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, completed, displayed, every, forField, foreach, mvu, projected, updated)
import PUI.HTML (atCase, body, li, staticText, text, ul)
import PUI.MDC3 (button, card, elevation5, displaySmall)
import QualifiedDo.Semigroupoid as Semigroupoid

stopwatchMDC3 :: Effect Unit
stopwatchMDC3 =
  body $
    elevation5 $
      card { caption: "Stopwatch" } $ ( Semigroupoid.do
          displaySmall text # projected readout # completed
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

stopwatchPhase :: { running :: Boolean } -> [ halted :: {}, timing :: {} ]
stopwatchPhase { running } = if running then .timing {} else .halted {}

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
