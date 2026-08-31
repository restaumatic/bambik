module ScoreboardMDC3 (scoreboardMDC3) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (muted, accumulated, every, foreach, mvu)
import PUI.Web.HTML (shownWhen, shown, body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, list, listItem)
import QualifiedDo.Category as Category
import ScoreboardLogic (boardSummary, gameStart, goal, standing, tick, tickPeriod)

scoreboardMDC3 :: Effect Unit
scoreboardMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          every tickPeriod tick
          ( Category.do
              list ( ( listItem $ RecordToRecord.do
                  text @"team"
                  staticText ": "
                  text @"pointsText" ) # shown ) # accumulated goal
              ( bodyMedium $ Category.do
                  ( RecordToRecord.do
                      text @"teams"
                      staticText " teams on the board — leading: " ) # shown
                  ( RecordToRecord.do
                      text @"team"
                      staticText " ("
                      text @"pointsText"
                      staticText ")" ) # shownWhen @"led" standing
                  (staticText "—") # shownWhen @"unled" standing ) # foreach @"key" boardSummary # muted ) # shown
      ) # mvu gameStart
