module StopwatchLogic (beginTiming, clearStopwatch, elapsedLine, formatTime, haltTiming, lapRows, recordLap, stopwatchPhase, tick, tickPeriod, zeroedStopwatch) where

import Prelude ((<>), (+), (<), show)

import Data.Array (mapWithIndex, snoc)
import Data.Int (quot, rem)
import Data.Maybe (Maybe(..))
import Data.Variant (match)

zeroedStopwatch :: { phase :: [ halted :: {}, timing :: {} ], elapsedTenths :: Int, laps :: Array Int }
zeroedStopwatch = { phase: .halted {}, elapsedTenths: 0, laps: [] }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 100.0 }

beginTiming :: { phase :: [ halted :: {}, timing :: {} ] }
beginTiming = { phase: .timing {} }

haltTiming :: { phase :: [ halted :: {}, timing :: {} ] }
haltTiming = { phase: .halted {} }

recordLap
  :: { elapsedTenths :: Int, laps :: Array Int }
  -> { elapsedTenths :: Int, laps :: Array Int }
recordLap sw@{ laps, elapsedTenths } = sw { laps = snoc laps elapsedTenths }

clearStopwatch :: { elapsedTenths :: Int, laps :: Array Int }
clearStopwatch = { elapsedTenths: 0, laps: [] }

tick
  :: { phase :: [ halted :: {}, timing :: {} ], elapsedTenths :: Int }
  -> Maybe { phase :: [ halted :: {}, timing :: {} ], elapsedTenths :: Int }
tick sw@{ phase, elapsedTenths } =
  match { timing: \_ -> Just (sw { elapsedTenths = elapsedTenths + 1 }), halted: \_ -> Nothing } phase

stopwatchPhase :: { phase :: [ halted :: {}, timing :: {} ] } -> [ halted :: {}, timing :: {} ]
stopwatchPhase { phase } = phase

lapRows :: { laps :: Array Int } -> Array { number :: String, time :: String }
lapRows { laps } = mapWithIndex (\i t -> { number: show (i + 1), time: formatTime t }) laps

formatTime :: Int -> String
formatTime tenths =
  pad2 (tenths `quot` 600) <> ":" <> pad2 ((tenths `rem` 600) `quot` 10) <> "." <> show (tenths `rem` 10)

pad2 :: Int -> String
pad2 n = if n < 10 then "0" <> show n else show n

elapsedLine :: { elapsedTenths :: Int } -> String
elapsedLine { elapsedTenths } = formatTime elapsedTenths
