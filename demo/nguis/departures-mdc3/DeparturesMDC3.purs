module DeparturesMDC3 (departuresMDC3) where

import Prelude ((#), ($), (+), Unit, div, mod)

import Data.Array (index, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (dispatched, displayed, every, forField, forValue, mvu)
import PUI.HTML (body, staticText, text)
import PUI.MDC3 (bodyMedium, card, elevation5, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departuresMDC3 :: Effect Unit
departuresMDC3 =
  body $
    elevation5 $
      card { caption: "Departures" } $ ( Semigroupoid.do
          every tickPeriod tick
          ( Semigroupoid.do
              ( list $
                  ( ( listItem $ RecordToRecord.do
                        text # forValue # forField @"code"
                        staticText " — "
                        text # forValue # forField @"status"
                    ) # displayed
                  ) # dispatched
              ) # lcmap arrival
              bodyMedium ( RecordToRecord.do
                  staticText "Last update: "
                  text # forValue # forField @"code"
                  staticText " → "
                  text # forValue # forField @"status" ) # lcmap _.value
          ) # displayed
      ) # mvu boardOpening

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }

boardOpening :: { n :: Int }
boardOpening = { n: 0 }

tick :: { n :: Int } -> Maybe { n :: Int }
tick { n } = Just { n: n + 1 }

arrival :: { n :: Int } -> { key :: String, value :: { code :: String, status :: String } }
arrival { n } =
  let
    code = pick flights n
    status = pick statuses (n + n `div` length flights)
  in
    { key: code, value: { code, status } }

pick :: Array String -> Int -> String
pick options i = fromMaybe "" (index options (i `mod` length options))

flights :: Array String
flights = [ "LH 441", "BA 902", "LO 331", "AF 118", "KL 605" ]

statuses :: Array String
statuses = [ "Scheduled", "Check-in", "Boarding", "Departed" ]
