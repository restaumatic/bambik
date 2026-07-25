module Stopwatch (stopwatch) where

import Prelude ((#), ($), (+), (<), (<<<), (<>), Unit, const, identity, not, show)

import Data.Array (mapWithIndex, snoc)
import Data.Int (quot, rem)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import PUI (asCase, completed, displayed, every, forValue, foreach, mvu, projection, updates)
import PUI.HTML (body, li, provided, text, ul)
import PUI.MDC (button, card, elevation20, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid

stopwatch :: Effect Unit
stopwatch =
  body $
    elevation20 $
      card { caption: "Stopwatch" } $ ( Semigroupoid.do
          headline3 text # projection readout # completed
          every tickPeriod tick
          ( RecordToVariant.do
              button { label: "Start", icon: "play_arrow" } # asCase @"start" # provided # lcmap whenHalted
              button { label: "Stop", icon: "stop" } # asCase @"stop" # provided # lcmap whenRunning
              button { label: "Lap", icon: "flag" } # asCase @"lap" # provided # lcmap whenRunning
              button { label: "Reset", icon: "replay" } # asCase @"reset" # provided # lcmap whenHalted) # updates (match { start: const <<< beginTiming, stop: const <<< haltTiming, lap: const <<< recordLap, reset: const <<< clearStopwatch })
          ul ( (li text # projection _.line) # foreach @"line" ) # lcmap lapLines # displayed
      ) # mvu zeroedStopwatch

beginTiming
  :: { running :: Boolean, elapsedTenths :: Int, laps :: Array Int }
  -> { running :: Boolean, elapsedTenths :: Int, laps :: Array Int }
beginTiming sw = sw { running = true }

haltTiming
  :: { running :: Boolean, elapsedTenths :: Int, laps :: Array Int }
  -> { running :: Boolean, elapsedTenths :: Int, laps :: Array Int }
haltTiming sw = sw { running = false }

recordLap
  :: { running :: Boolean, elapsedTenths :: Int, laps :: Array Int }
  -> { running :: Boolean, elapsedTenths :: Int, laps :: Array Int }
recordLap sw = sw { laps = snoc sw.laps sw.elapsedTenths }

clearStopwatch
  :: { running :: Boolean, elapsedTenths :: Int, laps :: Array Int }
  -> { running :: Boolean, elapsedTenths :: Int, laps :: Array Int }
clearStopwatch sw = sw { elapsedTenths = 0, laps = [] }

tick
  :: { running :: Boolean, elapsedTenths :: Int }
  -> Maybe { running :: Boolean, elapsedTenths :: Int }
tick sw =
  if sw.running then Just (sw { elapsedTenths = sw.elapsedTenths + 1 })
  else Nothing

whenHalted
  :: { running :: Boolean, elapsedTenths :: Int, laps :: Array Int }
  -> Maybe { running :: Boolean, elapsedTenths :: Int, laps :: Array Int }
whenHalted sw = if not sw.running then Just sw else Nothing

whenRunning
  :: { running :: Boolean, elapsedTenths :: Int, laps :: Array Int }
  -> Maybe { running :: Boolean, elapsedTenths :: Int, laps :: Array Int }
whenRunning sw = if sw.running then Just sw else Nothing

readout :: { elapsedTenths :: Int } -> String
readout sw = formatTime sw.elapsedTenths

lapLines :: { laps :: Array Int } -> Array { line :: String }
lapLines sw = mapWithIndex (\i t -> { line: "Lap " <> show (i + 1) <> " — " <> formatTime t }) sw.laps

formatTime :: Int -> String
formatTime tenths =
  pad2 (tenths `quot` 600) <> ":" <> pad2 ((tenths `rem` 600) `quot` 10) <> "." <> show (tenths `rem` 10)

pad2 :: Int -> String
pad2 n = if n < 10 then "0" <> show n else show n

zeroedStopwatch :: { running :: Boolean, elapsedTenths :: Int, laps :: Array Int }
zeroedStopwatch = { running: false, elapsedTenths: 0, laps: [] }

tickPeriod :: Milliseconds
tickPeriod = Milliseconds 100.0
