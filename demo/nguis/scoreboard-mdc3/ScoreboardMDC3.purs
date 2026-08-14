module ScoreboardMDC3 (scoreboardMDC3) where

import Prelude (identity, (#), ($), Unit, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (accumulated, displayed, every, forField, foreach, mvu)
import PUI.Web.HTML (body, provided, staticText, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import ScoreboardLogic (boardSummary, gameStart, goal, leadingTeam, noLeader, tick, tickPeriod)

scoreboardMDC3 :: Effect Unit
scoreboardMDC3 =
  body $
    elevation5 $
      card { caption: "Scoreboard" } $ ( Semigroupoid.do
          every tickPeriod tick
          ( Semigroupoid.do
              list ( ( listItem $ RecordToRecord.do
                  text @"team"
                  staticText ": "
                  text @"value" # forField @"points" show ) # displayed ) # accumulated goal
              ( bodyMedium $ Semigroupoid.do
                  ( RecordToRecord.do
                      text @"teams"
                      staticText " teams on the board — leading: " ) # displayed
                  ( RecordToRecord.do
                      text @"team"
                      staticText " ("
                      text @"value" # forField @"points" show
                      staticText ")" ) # provided leadingTeam # displayed
                  staticText "—" # provided noLeader # displayed ) # foreach @"key" boardSummary ) # displayed
      ) # mvu gameStart
