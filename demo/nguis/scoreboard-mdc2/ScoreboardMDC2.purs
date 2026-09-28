module ScoreboardMDC2 (scoreboardMDC2) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (muted, accumulated, every, foreach, mvu)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body2, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import ScoreboardLogic (boardSummary, gameStart, goal, scoreLine, summaryLine, tick, tickPeriod)

scoreboardMDC2 :: Effect Unit
scoreboardMDC2 =
  body $
    ( Semigroupoid.do
      every tickPeriod tick
      ( Semigroupoid.do
        list $ ( listItem $ text scoreLine ) # shown # accumulated goal
        ( body2 $ text summaryLine # shown ) # foreach @"key" boardSummary # muted ) # shown
    ) # mvu gameStart
