module ScoreboardMDC3 (scoreboardMDC3) where

import Prelude (Unit, show, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (muted, accumulated, every, projection, foreach, mvu)
import PUI.Web.HTML (shownWhen, shown, body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, list, listItem)
import QualifiedDo.Semigroupoid as Pipeline
import ScoreboardLogic (boardSummary, gameStart, goal, leadingTeam, noLeader, tick, tickPeriod)

scoreboardMDC3 :: Effect Unit
scoreboardMDC3 =
  body $
    elevation5 $
      card $ ( Pipeline.do
          every tickPeriod tick
          ( Pipeline.do
              list ( ( listItem $ RecordToRecord.do
                  text @"team"
                  staticText ": "
                  text @"points" # projection show ) # shown ) # accumulated goal
              ( bodyMedium $ Pipeline.do
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
