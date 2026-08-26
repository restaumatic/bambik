module ScoreboardMDC2 (scoreboardMDC2) where

import Prelude (Unit, show, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (muted, accumulated, every, projection, foreach, mvu)
import PUI.Web.HTML (shownWhen, shown, body, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, list, listItem)
import QualifiedDo.Category as Category
import ScoreboardLogic (boardSummary, gameStart, goal, leadingTeam, noLeader, tick, tickPeriod)

scoreboardMDC2 :: Effect Unit
scoreboardMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          every tickPeriod tick
          ( Category.do
              list ( ( listItem $ RecordToRecord.do
                  text @"team"
                  staticText ": "
                  text @"points" # projection show ) # shown ) # accumulated goal
              ( body2 $ Category.do
                  ( RecordToRecord.do
                      text @"teams"
                      staticText " teams on the board — leading: " ) # shown
                  ( RecordToRecord.do
                      text @"team"
                      staticText " ("
                      text @"points" # projection show
                      staticText ")" ) # shownWhen leadingTeam
                  (staticText "—") # shownWhen noLeader ) # foreach @"key" boardSummary # muted ) # shown
      ) # mvu gameStart
