module ScoreboardMDC2 (scoreboardMDC2) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (muted, accumulated, every, foreach, mvu)
import PUI.Web.HTML (shownWhen, shown, body, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, list, listItem)
import QualifiedDo.Category as Category
import ScoreboardLogic (boardSummary, gameStart, goal, standing, tick, tickPeriod)

scoreboardMDC2 :: Effect Unit
scoreboardMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          every tickPeriod tick
          ( Category.do
              list ( ( listItem $ text @"scoreLine" ) # shown ) # accumulated goal
              ( body2 $ Category.do
                  (text @"teamsLine") # shown
                  (text @"leaderLine") # shownWhen @"led" standing
                  (staticText "—") # shownWhen @"unled" standing ) # foreach @"key" boardSummary # muted ) # shown
      ) # mvu gameStart
