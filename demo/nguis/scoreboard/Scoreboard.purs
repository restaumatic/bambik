module Scoreboard (scoreboard) where

import Prelude ((#), ($), (+), (<<<), (<>), (==), Unit, mod, show)

import Data.Array (filter, index, length, range)
import Data.Foldable (maximumBy)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Ord (comparing)
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import PUI (accumulated, displayed, every, forField, forValue, mvu, projection)
import PUI.HTML (body, staticText, text)
import PUI.MDC (body2, card, elevation20, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

scoreboard :: Effect Unit
scoreboard =
  body $
    elevation20 $
      card { caption: "Scoreboard" } $ ( Semigroupoid.do
          every (Milliseconds 1000.0) tick
          ( Semigroupoid.do
              ( list $
                  ( ( listItem $ RecordToRecord.do
                        text # forValue # forField @"team"
                        staticText ": "
                        text # projection show # forField @"points"
                    ) # displayed
                  ) # accumulated
              ) # lcmap goal
              body2 text # projection standings
          ) # displayed
      ) # mvu { n: 0 }

tick :: { n :: Int } -> Maybe { n :: Int }
tick match = Just { n: match.n + 1 }

goal :: { n :: Int } -> { key :: String, value :: { team :: String, points :: Int } }
goal match =
  let team = pick teams match.n
  in { key: team, value: { team, points: scored team match.n } }

scored :: String -> Int -> Int
scored team n = length (filter (\i -> pick teams i == team) (range 0 n))

standings :: Array { team :: String, points :: Int } -> String
standings scores =
  show (length scores) <> " teams on the board — leading: "
    <> maybe "—" (\s -> s.team <> " (" <> show s.points <> ")") (maximumBy (comparing _.points) scores)

pick :: Array String -> Int -> String
pick options i = fromMaybe "" (index options (i `mod` length options))

teams :: Array String
teams = [ "Owls", "Foxes", "Herons" ]
