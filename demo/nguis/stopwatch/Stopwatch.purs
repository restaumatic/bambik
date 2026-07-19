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
import PUI (asCase, completed, displayed, every, forValue, mvu, projection, updates)
import PUI.HTML (body, foreach, li, provided, text, ul)
import PUI.MDC (button, card, elevation20, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid

type Stopwatch =
  { running :: Boolean
  , elapsedTenths :: Int
  , laps :: Array Int
  }

stopwatch :: Effect Unit
stopwatch =
  body $
    elevation20 $
      card { caption: "Stopwatch" } $ ( Semigroupoid.do
          headline3 (text # projection readout # forValue) # completed
          every tickPeriod tick
          ( RecordToVariant.do
              button { label: "Start", icon: "play_arrow" } # asCase @"start" # provided # lcmap whenHalted
              button { label: "Stop", icon: "stop" } # asCase @"stop" # provided # lcmap whenRunning
              button { label: "Lap", icon: "flag" } # asCase @"lap" # provided # lcmap whenRunning
              button { label: "Reset", icon: "replay" } # asCase @"reset" # provided # lcmap whenHalted
          ) # updates (match
              { start: const <<< beginTiming
              , stop: const <<< haltTiming
              , lap: const <<< recordLap
              , reset: const <<< clearStopwatch
              })
          ul ( (li (text # forValue)) # foreach identity ) # lcmap lapLines # displayed
      ) # mvu zeroedStopwatch

beginTiming :: Stopwatch -> Stopwatch
beginTiming sw = sw { running = true }

haltTiming :: Stopwatch -> Stopwatch
haltTiming sw = sw { running = false }

recordLap :: Stopwatch -> Stopwatch
recordLap sw = sw { laps = snoc sw.laps sw.elapsedTenths }

clearStopwatch :: Stopwatch -> Stopwatch
clearStopwatch sw = sw { elapsedTenths = 0, laps = [] }

tick :: Stopwatch -> Maybe Stopwatch
tick sw =
  if sw.running then Just (sw { elapsedTenths = sw.elapsedTenths + 1 })
  else Nothing

whenHalted :: Stopwatch -> Maybe Stopwatch
whenHalted sw = if not sw.running then Just sw else Nothing

whenRunning :: Stopwatch -> Maybe Stopwatch
whenRunning sw = if sw.running then Just sw else Nothing

readout :: Stopwatch -> String
readout sw = formatTime sw.elapsedTenths

lapLines :: Stopwatch -> Array String
lapLines sw = mapWithIndex (\i t -> "Lap " <> show (i + 1) <> " — " <> formatTime t) sw.laps

formatTime :: Int -> String
formatTime tenths =
  pad2 (tenths `quot` 600) <> ":" <> pad2 ((tenths `rem` 600) `quot` 10) <> "." <> show (tenths `rem` 10)

pad2 :: Int -> String
pad2 n = if n < 10 then "0" <> show n else show n

zeroedStopwatch :: Stopwatch
zeroedStopwatch = { running: false, elapsedTenths: 0, laps: [] }

tickPeriod :: Milliseconds
tickPeriod = Milliseconds 100.0
