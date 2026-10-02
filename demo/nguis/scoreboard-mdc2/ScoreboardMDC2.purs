module ScoreboardMDC2 (scoreboardMDC2) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (accumulated, every, foreach, muted, mvu, state)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body2, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import ScoreboardViewModel (boardSummary, gameStart, goal, scoreLine, summaryLine, tick, tickPeriod)

scoreboardMDC2 :: Effect Unit
scoreboardMDC2 =
  body $
    ( Semigroupoid.do
      state @"beat" @Int
      every tickPeriod tick
      ( Semigroupoid.do
        list $ ( Semigroupoid.do
          state @"team" @String
          state @"points" @Int
          listItem $ text scoreLine ) # shown # accumulated @String goal
        ( Semigroupoid.do
          state @"teams" @Int
          state @"leader" @[ led :: { team :: String, points :: Int }, unled :: {} ]
          body2 $ text summaryLine # shown ) # foreach @"key" @String boardSummary # muted ) # shown
    ) # mvu gameStart
