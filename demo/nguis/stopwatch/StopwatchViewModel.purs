module StopwatchViewModel (beginTiming, clearStopwatch, elapsedText, haltTiming, lapLine, lapRows, recordLap, tick, tickPeriod, zeroedStopwatch) where

import Prelude ((<>), (+), (<), show)

import Data.Array (mapWithIndex, snoc)
import Data.Int (quot, rem)
import Data.Maybe (Maybe(..))
import Data.Variant (match)

zeroedStopwatch :: { phase :: [ halted :: {}, timing :: {} ], elapsedTenths :: Int, laps :: Array Int }
zeroedStopwatch = { phase: .halted {}, elapsedTenths: 0, laps: [] }

elapsedText :: forall r1. { elapsedTenths :: Int | r1 } -> String
elapsedText { elapsedTenths } = formatTime elapsedTenths

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 100.0 }

beginTiming :: forall r. { phase :: [ halted :: {}, timing :: {} ] | r } -> { phase :: [ halted :: {}, timing :: {} ] | r }
beginTiming sw = sw { phase = .timing {} }

haltTiming :: forall r. { phase :: [ halted :: {}, timing :: {} ] | r } -> { phase :: [ halted :: {}, timing :: {} ] | r }
haltTiming sw = sw { phase = .halted {} }

recordLap :: forall r1. { elapsedTenths :: Int, laps :: Array Int | r1 } -> { elapsedTenths :: Int, laps :: Array Int | r1 }
recordLap sw@{ laps, elapsedTenths } = sw { laps = snoc laps elapsedTenths }

clearStopwatch :: forall r. { elapsedTenths :: Int, laps :: Array Int | r } -> { elapsedTenths :: Int, laps :: Array Int | r }
clearStopwatch sw = sw { elapsedTenths = 0, laps = [] }

tick :: forall r1. { phase :: [ halted :: {}, timing :: {} ], elapsedTenths :: Int | r1 } -> Maybe { phase :: [ halted :: {}, timing :: {} ], elapsedTenths :: Int | r1 }
tick sw@{ phase, elapsedTenths } =
  match { timing: \_ -> Just (sw { elapsedTenths = elapsedTenths + 1 }), halted: \_ -> Nothing } phase

lapRows :: forall r1. { laps :: Array Int | r1 } -> Array { number :: Int, tenths :: Int }
lapRows { laps } = mapWithIndex (\i t -> { number: i + 1, tenths: t }) laps

lapLine :: forall r1. { number :: Int, tenths :: Int | r1 } -> String
lapLine { number, tenths } = "Lap " <> show number <> " — " <> formatTime tenths

formatTime :: Int -> String
formatTime tenths =
  pad2 (tenths `quot` 600) <> ":" <> pad2 ((tenths `rem` 600) `quot` 10) <> "." <> show (tenths `rem` 10)

pad2 :: Int -> String
pad2 n = if n < 10 then "0" <> show n else show n
