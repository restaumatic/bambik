module ScoreboardMDC2 (scoreboardMDC2) where

import Prelude (identity, (#), ($), Unit, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (accumulated, displayed, every, forField, foreach, mvu)
import PUI.Web.HTML (body, provided, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import ScoreboardLogic (boardSummary, gameStart, goal, leadingTeam, noLeader, tick, tickPeriod)

scoreboardMDC2 :: Effect Unit
scoreboardMDC2 =
  body $
    elevation20 $
      card { caption: "Scoreboard" } $ ( Semigroupoid.do
          every tickPeriod tick
          ( Semigroupoid.do
              list ( ( listItem $ RecordToRecord.do
                  text # forField @"value" @"team" identity
                  staticText ": "
                  text # forField @"value" @"points" show ) # displayed ) # accumulated goal
              ( body2 $ Semigroupoid.do
                  ( RecordToRecord.do
                      text # forField @"value" @"teams" identity
                      staticText " teams on the board — leading: " ) # displayed
                  ( RecordToRecord.do
                      text # forField @"value" @"team" identity
                      staticText " ("
                      text # forField @"value" @"points" show
                      staticText ")" ) # provided leadingTeam # displayed
                  staticText "—" # provided noLeader # displayed ) # foreach @"key" boardSummary ) # displayed
      ) # mvu gameStart
