module StopwatchViewModel (beginTiming, clearStopwatch, elapsedText, haltTiming, lapLine, lapRecordedLine, lapRows, recordLap, stopwatchResetLine, tick, tickPeriod, timingBegunLine, timingHaltedLine, zeroedStopwatch) where

import Prelude ((<>), (+), (<), show)

import Data.Array (last, length, mapWithIndex, snoc)
import Data.Int (quot, rem)
import Data.Maybe (maybe)
import Data.Variant (match)

zeroedStopwatch :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] }
zeroedStopwatch = { phase: .halted {}, elapsedTenths: 0, laps: [] }

elapsedText :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } -> String
elapsedText { elapsedTenths } = formatTime elapsedTenths

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 100.0 }

beginTiming :: { event :: {}, model :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } } -> { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] }
beginTiming { model: sw } = sw { phase = .timing {} }

haltTiming :: { event :: {}, model :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } } -> { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] }
haltTiming { model: sw } = sw { phase = .halted {} }

recordLap :: { event :: {}, model :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } } -> { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] }
recordLap { model: sw@{ laps, elapsedTenths } } = sw { laps = snoc laps elapsedTenths }

clearStopwatch :: { event :: {}, model :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } } -> { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] }
clearStopwatch { model: sw } = sw { elapsedTenths = 0, laps = [] }

tick :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } -> { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] }
tick sw@{ phase, elapsedTenths } =
  match { timing: \_ -> sw { elapsedTenths = elapsedTenths + 1 }, halted: \_ -> sw } phase

timingBegunLine :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } -> String
timingBegunLine { elapsedTenths } = "Started timing at " <> formatTime elapsedTenths

timingHaltedLine :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } -> String
timingHaltedLine { elapsedTenths } = "Stopped at " <> formatTime elapsedTenths

lapRecordedLine :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } -> String
lapRecordedLine { laps } = "Lap " <> show (length laps) <> " at " <> maybe "—" formatTime (last laps)

stopwatchResetLine :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } -> String
stopwatchResetLine { elapsedTenths } = "Reset to " <> formatTime elapsedTenths

lapRows :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } -> Array { number :: Int, tenths :: Int }
lapRows { laps } = mapWithIndex (\i t -> { number: i + 1, tenths: t }) laps

lapLine :: { number :: Int, tenths :: Int } -> String
lapLine { number, tenths } = "Lap " <> show number <> " — " <> formatTime tenths

formatTime :: Int -> String
formatTime tenths =
  pad2 (tenths `quot` 600) <> ":" <> pad2 ((tenths `rem` 600) `quot` 10) <> "." <> show (tenths `rem` 10)

pad2 :: Int -> String
pad2 n = if n < 10 then "0" <> show n else show n
