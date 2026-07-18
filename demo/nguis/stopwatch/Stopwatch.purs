module Stopwatch (stopwatch) where

import Prelude ((#), ($), (+), (<), (<>), (>>>), Unit, not, show)

import Data.Array (mapWithIndex, snoc)
import Data.Int (quot, rem)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import PUI (asCase, completed, every, forValue, mvu, projection, updates)
import PUI.HTML (attr, body, div, foreach, li, shownWhen, text, ul)
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
          ( div >>> attr "style" "display: flex; gap: 8px;" $ RecordToVariant.do
              shownWhen halted $ button { label: "Start", icon: "play_arrow" } # asCase @"start"
              shownWhen _.running $ button { label: "Stop", icon: "stop" } # asCase @"stop"
              shownWhen _.running $ button { label: "Lap", icon: "flag" } # asCase @"lap"
              shownWhen halted $ button { label: "Reset", icon: "replay" } # asCase @"reset"
          ) # updates (match
              { start: \sw _ -> beginTiming sw
              , stop: \sw _ -> haltTiming sw
              , lap: \sw _ -> recordLap sw
              , reset: \sw _ -> clearStopwatch sw
              })
          ul (foreach (li (text # forValue))) # lcmap lapLines # updates (\_ sw -> sw)
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

halted :: Stopwatch -> Boolean
halted sw = not sw.running

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
