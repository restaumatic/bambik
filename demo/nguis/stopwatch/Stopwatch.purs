module Stopwatch (Lap, Stopwatch, beginTiming, clearStopwatch, elapsedLine, haltTiming, lapLine, lapRows, recordLap, tick, tickPeriod, zeroedStopwatch) where

import Prelude ((<>), (+), (<), show)

import Data.Array (mapWithIndex, snoc)
import Data.Int (quot, rem)
import Data.Variant (match)

type Stopwatch =
  { elapsedTenths :: Int
  , laps :: Array Int
  , phase :: [ halted :: {}, timing :: {} ]
  }

type Lap = { number :: Int, tenths :: Int }

zeroedStopwatch :: Stopwatch
zeroedStopwatch = { phase: .halted {}, elapsedTenths: 0, laps: [] }

elapsedLine :: Stopwatch -> String
elapsedLine { elapsedTenths } = formatTime elapsedTenths

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 100.0 }

beginTiming :: Stopwatch -> Stopwatch
beginTiming sw = sw { phase = .timing {} }

haltTiming :: Stopwatch -> Stopwatch
haltTiming sw = sw { phase = .halted {} }

recordLap :: Stopwatch -> Stopwatch
recordLap sw@{ laps, elapsedTenths } = sw { laps = snoc laps elapsedTenths }

clearStopwatch :: Stopwatch -> Stopwatch
clearStopwatch sw = sw { elapsedTenths = 0, laps = [] }

tick :: Stopwatch -> Stopwatch
tick sw@{ phase, elapsedTenths } =
  match { timing: \_ -> sw { elapsedTenths = elapsedTenths + 1 }, halted: \_ -> sw } phase

lapRows :: Stopwatch -> Array Lap
lapRows { laps } = mapWithIndex (\i t -> { number: i + 1, tenths: t }) laps

lapLine :: Lap -> String
lapLine { number, tenths } = "Lap " <> show number <> " — " <> formatTime tenths

formatTime :: Int -> String
formatTime tenths =
  pad2 (tenths `quot` 600) <> ":" <> pad2 ((tenths `rem` 600) `quot` 10) <> "." <> show (tenths `rem` 10)

pad2 :: Int -> String
pad2 n = if n < 10 then "0" <> show n else show n
