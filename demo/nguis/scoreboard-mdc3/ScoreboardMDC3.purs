module ScoreboardMDC3 (scoreboardMDC3) where

import Prelude (Unit, show, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (muted, accumulated, tapped, every, projection, foreach, mvu)
import PUI.Web.HTML (body, provided, staticText, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import ScoreboardLogic (boardSummary, gameStart, goal, leadingTeam, noLeader, tick, tickPeriod)

scoreboardMDC3 :: Effect Unit
scoreboardMDC3 =
  body $
    elevation5 $
      card $ ( Semigroupoid.do
          every tickPeriod tick
          ( Semigroupoid.do
              list ( ( listItem $ RecordToRecord.do
                  text @"team"
                  staticText ": "
                  text @"points" # projection show ) # tapped ) # accumulated goal
              ( bodyMedium $ Semigroupoid.do
                  ( RecordToRecord.do
                      text @"teams"
                      staticText " teams on the board — leading: " ) # tapped
                  ( RecordToRecord.do
                      text @"team"
                      staticText " ("
                      text @"points" # projection show
                      staticText ")" ) # provided leadingTeam # tapped
                  staticText "—" # provided noLeader # tapped ) # foreach @"key" boardSummary # muted ) # tapped
      ) # mvu gameStart
