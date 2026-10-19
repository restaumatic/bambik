module Departures (Departure, Update, arrival, clockPeriod, flightLine, updateLine) where

import Prelude ((*), (+), (<>), div, mod)

import Data.Array (index, length)
import Data.Maybe (fromMaybe)

type Departure = { code :: String, status :: String }

type Update = { key :: String, value :: Departure }

clockPeriod :: { ms :: Number }
clockPeriod = { ms: 1000.0 }

arrival :: { time :: { hours :: Int, minutes :: Int, seconds :: Int } } -> Update
arrival { time } =
  let
    beat = time.minutes * 60 + time.seconds
    code = pick flights beat
    status = pick statuses (beat `div` length flights + beat `mod` length flights)
  in
    { key: code, value: { code, status } }

flightLine :: Departure -> String
flightLine { code, status } = code <> " — " <> status

updateLine :: Update -> String
updateLine { value: { code, status } } = "Last update: " <> code <> " → " <> status

pick :: Array String -> Int -> String
pick options i = fromMaybe "" (index options (i `mod` length options))

flights :: Array String
flights = [ "LH 441", "BA 902", "LO 331", "AF 118", "KL 605" ]

statuses :: Array String
statuses = [ "Scheduled", "Check-in", "Boarding", "Departed" ]
