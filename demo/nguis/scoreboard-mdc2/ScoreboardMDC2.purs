module ScoreboardMDC2 (scoreboardMDC2) where

import Prelude (Unit, identity, (#), ($))

import Effect (Effect)
import PUI (accumulated, blankStatus, fold, foreach, looped, muted, prefixed, ticks, with)
import PUI.Web (text)
import PUI.Web.MDC2 (body, body2, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import ScoreboardViewModel (boardSummary, gameStart, goal, scoreLine, summaryLine, tick, tickPeriod)

scoreboardMDC2 :: Effect Unit
scoreboardMDC2 =
  body $ Semigroupoid.do
    prefixed ( Semigroupoid.do
      list $ prefixed (listItem $ text scoreLine) identity # accumulated @String @{ team :: String, points :: Int } goal
      ( body2 $ text summaryLine ) # foreach @"key"
        @( key :: String
         , teams :: Int
         , leader :: [ led :: { team :: String, points :: Int }, unled :: {} ]
         ) boardSummary # muted ) $ blankStatus @"Points scored" # ticks tickPeriod
    blankStatus @"Points scored" # fold tick
  # looped @( beat :: Int ) # with gameStart
