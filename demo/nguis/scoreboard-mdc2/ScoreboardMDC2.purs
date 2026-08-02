module ScoreboardMDC2 (scoreboardMDC2) where

import Prelude (identity, (#), ($), (+), (==), Unit, mod, show)

import Data.Array (filter, index, length, range)
import Data.Foldable (maximumBy)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (comparing)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (accumulated, displayed, every, forField, foreach, mvu)
import PUI.Web.HTML (body, provided, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

scoreboardMDC2 :: Effect Unit
scoreboardMDC2 =
  body $
    elevation20 $
      card { caption: "Scoreboard" } $ ( Semigroupoid.do
          every tickPeriod tick
          ( Semigroupoid.do
              ( list $
                  ( ( listItem $ RecordToRecord.do
                        text # forField @"team" identity
                        staticText ": "
                        text # forField @"points" show
                    ) # displayed
                  ) # accumulated goal
              )
              ( body2 $ Semigroupoid.do
                  ( RecordToRecord.do
                      text # forField @"teams" identity
                      staticText " teams on the board — leading: " ) # displayed
                  ( RecordToRecord.do
                      text # forField @"team" identity
                      staticText " ("
                      text # forField @"points" show
                      staticText ")" ) # provided leadingTeam # displayed
                  staticText "—" # provided noLeader # displayed
              ) # foreach @"key" boardSummary
          ) # displayed
      ) # mvu gameStart

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }

gameStart :: { n :: Int }
gameStart = { n: 0 }

tick :: { n :: Int } -> Maybe { n :: Int }
tick { n } = Just { n: n + 1 }

goal :: { n :: Int } -> { key :: String, value :: { team :: String, points :: Int } }
goal { n } =
  let team = pick teams n
  in { key: team, value: { team, points: scored team n } }

scored :: String -> Int -> Int
scored team n = length (filter (\i -> pick teams i == team) (range 0 n))

boardSummary :: Array { team :: String, points :: Int } -> Array { key :: String, teams :: String, leader :: Maybe { team :: String, points :: Int } }
boardSummary scores = [ { key: "summary", teams: show (length scores), leader: maximumBy (comparing _.points) scores } ]


leadingTeam :: { leader :: Maybe { team :: String, points :: Int } } -> Maybe { team :: String, points :: Int }
leadingTeam { leader } = leader

noLeader :: { leader :: Maybe { team :: String, points :: Int } } -> Maybe {}
noLeader { leader } = case leader of
  Just _ -> Nothing
  Nothing -> Just {}

pick :: Array String -> Int -> String
pick options i = fromMaybe "" (index options (i `mod` length options))

teams :: Array String
teams = [ "Owls", "Foxes", "Herons" ]
