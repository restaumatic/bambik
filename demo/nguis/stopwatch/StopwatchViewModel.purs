module StopwatchViewModel (beginTiming, clearStopwatch, clockTickedLine, elapsedText, haltTiming, lapLine, lapRecordedLine, lapRows, recordLap, stopwatchResetLine, tick, tickPeriod, timingBegunLine, timingHaltedLine, zeroedStopwatch) where

import Prelude ((<>), (+), (<), show)

import Data.Array (length, mapWithIndex, snoc)
import Data.Int (quot, rem)
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

clockTickedLine :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } -> String
clockTickedLine { elapsedTenths } = "Ticked past " <> formatTime elapsedTenths

timingBegunLine :: { event :: {}, model :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } } -> String
timingBegunLine { model: { elapsedTenths } } = "Started timing at " <> formatTime elapsedTenths

timingHaltedLine :: { event :: {}, model :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } } -> String
timingHaltedLine { model: { elapsedTenths } } = "Stopped at " <> formatTime elapsedTenths

lapRecordedLine :: { event :: {}, model :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } } -> String
lapRecordedLine { model: { laps, elapsedTenths } } = "Lap " <> show (length laps + 1) <> " at " <> formatTime elapsedTenths

stopwatchResetLine :: { event :: {}, model :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } } -> String
stopwatchResetLine { model: { elapsedTenths } } = "Reset from " <> formatTime elapsedTenths

lapRows :: { elapsedTenths :: Int, laps :: Array Int, phase :: [ halted :: {}, timing :: {} ] } -> Array { number :: Int, tenths :: Int }
lapRows { laps } = mapWithIndex (\i t -> { number: i + 1, tenths: t }) laps

lapLine :: { number :: Int, tenths :: Int } -> String
lapLine { number, tenths } = "Lap " <> show number <> " — " <> formatTime tenths

formatTime :: Int -> String
formatTime tenths =
  pad2 (tenths `quot` 600) <> ":" <> pad2 ((tenths `rem` 600) `quot` 10) <> "." <> show (tenths `rem` 10)

pad2 :: Int -> String
pad2 n = if n < 10 then "0" <> show n else show n
