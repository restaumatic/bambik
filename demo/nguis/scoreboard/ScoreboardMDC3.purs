module ScoreboardMDC3 (scoreboardMDC3) where

import Prelude (Unit, identity, (#), ($))

import Effect (Effect)
import PUI (accumulated, fold, foreach, looped, muted, prefixed, ticks, unshown, with)
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyMedium, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import ScoreboardViewModel (boardSummary, gameStart, goal, scoreLine, summaryLine, tick, tickPeriod)

scoreboardMDC3 :: Effect Unit
scoreboardMDC3 =
  body $ Semigroupoid.do
    prefixed ( Semigroupoid.do
      list $ prefixed (listItem $ text scoreLine) identity # accumulated @String @{ team :: String, points :: Int } goal
      ( bodyMedium $ text summaryLine ) # foreach @"key"
        @( key :: String
         , teams :: Int
         , leader :: [ led :: { team :: String, points :: Int }, unled :: {} ]
         ) boardSummary # muted ) $ unshown @"Points scored" # ticks tickPeriod
    unshown @"Points scored" # fold tick
  # looped @( beat :: Int ) # with gameStart
