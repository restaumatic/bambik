module ScoreboardMDC3 (scoreboardMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (muted, accumulated, every, foreach, mvu)
import PUI.Web.HTML (shown, body, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, list, listItem)
import QualifiedDo.Category as Category
import ScoreboardLogic (boardSummary, gameStart, goal, scoreLine, summaryLine, tick, tickPeriod)

scoreboardMDC3 :: Effect Unit
scoreboardMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          every tickPeriod tick
          ( Category.do
              list $ ( listItem $ text scoreLine ) # shown # accumulated goal
              ( bodyMedium $ text summaryLine # shown ) # foreach @"key" boardSummary # muted ) # shown
      ) # mvu gameStart
