module ScoreboardMDC2 (scoreboardMDC2) where

import Prelude ((#), ($), Unit, identity)

import Effect (Effect)
import PUI (accumulated, blankStatus, fold, foreach, looped, muted, replaying, ticks, with)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body2, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import ScoreboardViewModel (boardSummary, gameStart, goal, scoreLine, summaryLine, tick, tickPeriod)

scoreboardMDC2 :: Effect Unit
scoreboardMDC2 =
  body $
    ( Semigroupoid.do
      ( Semigroupoid.do
        list $ ( listItem $ text scoreLine ) # shown # accumulated @String @{ team :: String, points :: Int } goal
        ( body2 $ text summaryLine # shown ) # foreach @"key" @( key :: String, teams :: Int, leader :: [ led :: { team :: String, points :: Int }, unled :: {} ] ) boardSummary # muted ) # shown
      ticks @"Points scored" tickPeriod # replaying @"Points scored" identity
      blankStatus @"Points scored" # fold tick
    ) # looped @( beat :: Int ) # with gameStart
