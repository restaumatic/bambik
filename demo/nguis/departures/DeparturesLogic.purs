module DeparturesLogic (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine) where

import Prelude ((+), (<>), div, mod)

import Data.Array (index, length)
import Data.Maybe (Maybe(..), fromMaybe)

boardOpening :: { n :: Int }
boardOpening = { n: 0 }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }

tick :: { n :: Int } -> Maybe { n :: Int }
tick { n } = Just { n: n + 1 }

arrival :: { n :: Int } -> { key :: String, value :: { code :: String, status :: String } }
arrival { n } =
  let
    code = pick flights n
    status = pick statuses (n + n `div` length flights)
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
