module DeparturesLogic (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine) where

import Prelude ((+), (<>), div, mod)

import Data.Array (index, length)
import Data.Maybe (Maybe(..), fromMaybe)

boardOpening :: { beat :: Int }
boardOpening = { beat: 0 }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }

tick :: { beat :: Int } -> Maybe { beat :: Int }
tick { beat } = Just { beat: beat + 1 }

arrival :: { beat :: Int } -> { key :: String, value :: { code :: String, status :: String } }
arrival { beat } =
  let
    code = pick flights beat
    status = pick statuses (beat + beat `div` length flights)
  in
    { key: code, value: { code, status } }

flightLine :: { code :: String, status :: String } -> String
flightLine { code, status } = code <> " — " <> status

updateLine :: { key :: String, value :: { code :: String, status :: String } } -> String
updateLine { value: { code, status } } = "Last update: " <> code <> " → " <> status

pick :: Array String -> Int -> String
pick options i = fromMaybe "" (index options (i `mod` length options))

flights :: Array String
flights = [ "LH 441", "BA 902", "LO 331", "AF 118", "KL 605" ]

statuses :: Array String
statuses = [ "Scheduled", "Check-in", "Boarding", "Departed" ]
