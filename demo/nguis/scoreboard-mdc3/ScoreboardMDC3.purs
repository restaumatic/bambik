module ScoreboardMDC3 (scoreboardMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (muted, accumulated, every, foreach, mvu)
import PUI.Web.HTML (shownWhen, shown, body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, list, listItem)
import QualifiedDo.Category as Category
import ScoreboardLogic (boardSummary, gameStart, goal, leaderLine, scoreLine, standing, teamsLine, tick, tickPeriod)

scoreboardMDC3 :: Effect Unit
scoreboardMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          every tickPeriod tick
          ( Category.do
              list ( ( listItem $ text scoreLine ) # shown ) # accumulated goal
              ( bodyMedium $ Category.do
                  (text teamsLine) # shown
                  (text leaderLine) # shownWhen @"led" standing
                  (staticText "—") # shownWhen @"unled" standing ) # foreach @"key" boardSummary # muted ) # shown
      ) # mvu gameStart
