module Stopwatch (Lap, Moment, Stopwatch, StopwatchRow, Timed, beginTiming, clearStopwatch, elapsedLine, haltTiming, lapLine, lapRows, recordLap, tickPeriod, zeroedStopwatch) where

import Prelude ((+), (-), (/), (<), (<>), show)

import Data.Array (mapWithIndex, snoc)
import Data.Int (floor, quot, rem)
import Data.Variant (match)

type Stopwatch = { | StopwatchRow () }

type Timed = { | StopwatchRow ( time :: Moment ) }

type StopwatchRow r =
  ( laps :: Array Number
  , phase :: [ halted :: { elapsed :: Number }, timing :: { since :: Moment } ]
  | r
  )

type Moment = { ms :: Number }

type Lap = { elapsed :: Number, number :: Int }

zeroedStopwatch :: Stopwatch
zeroedStopwatch = { phase: .halted { elapsed: 0.0 }, laps: [] }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 100.0 }

elapsedLine :: Timed -> String
elapsedLine { phase, time } = formatTime (elapsedAt time phase)

beginTiming :: { event :: Timed, model :: Stopwatch } -> Stopwatch
beginTiming { event: { time }, model } = model { phase = .timing { since: { ms: time.ms - elapsedAt time model.phase } } }

haltTiming :: { event :: Timed, model :: Stopwatch } -> Stopwatch
haltTiming { event: { time }, model } = model { phase = .halted { elapsed: elapsedAt time model.phase } }

recordLap :: { event :: Timed, model :: Stopwatch } -> Stopwatch
recordLap { event: { time }, model } = model { laps = snoc model.laps (elapsedAt time model.phase) }

clearStopwatch :: Stopwatch -> Stopwatch
clearStopwatch sw = sw { phase = .halted { elapsed: 0.0 }, laps = [] }

lapRows :: Stopwatch -> Array Lap
lapRows { laps } = mapWithIndex (\i t -> { number: i + 1, elapsed: t }) laps

lapLine :: Lap -> String
lapLine lap = "Lap " <> show lap.number <> " — " <> formatTime lap.elapsed

elapsedAt :: Moment -> [ halted :: { elapsed :: Number }, timing :: { since :: Moment } ] -> Number
elapsedAt moment = match { halted: \{ elapsed } -> elapsed, timing: \{ since } -> moment.ms - since.ms }

formatTime :: Number -> String
formatTime ms =
  pad2 (tenths `quot` 600) <> ":" <> pad2 ((tenths `rem` 600) `quot` 10) <> "." <> show (tenths `rem` 10)
  where
  tenths = floor (ms / 100.0)

pad2 :: Int -> String
pad2 n = if n < 10 then "0" <> show n else show n
