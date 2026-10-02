module DeparturesViewModel (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine) where

import Prelude ((+), (<>), div, mod)

import Data.Array (index, length)
import Data.Maybe (Maybe(..), fromMaybe)

boardOpening :: { beat :: Int }
boardOpening = { beat: 0 }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }

tick :: forall r1. { beat :: Int | r1 } -> Maybe { beat :: Int }
tick { beat } = Just { beat: beat + 1 }

arrival :: forall r1. { beat :: Int | r1 } -> { key :: String, value :: { code :: String, status :: String } }
arrival { beat } =
  let
    code = pick flights beat
    status = pick statuses (beat + beat `div` length flights)
  in
    { key: code, value: { code, status } }

flightLine :: forall r1. { code :: String, status :: String | r1 } -> String
flightLine { code, status } = code <> " — " <> status

updateLine :: forall r1. { key :: String, value :: { code :: String, status :: String } | r1 } -> String
updateLine { value: { code, status } } = "Last update: " <> code <> " → " <> status

pick :: Array String -> Int -> String
pick options i = fromMaybe "" (index options (i `mod` length options))

flights :: Array String
flights = [ "LH 441", "BA 902", "LO 331", "AF 118", "KL 605" ]

statuses :: Array String
statuses = [ "Scheduled", "Check-in", "Boarding", "Departed" ]
