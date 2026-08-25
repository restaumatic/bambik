module ScoreboardMDC3 (scoreboardMDC3) where

import Prelude (identity, Unit, show, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (muted, accumulated, every, projection, foreach, mvu)
import PUI.Web.HTML (shownWhen, shownAs, body, staticText, text)
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
                  text @"points" # projection show ) # shownAs identity ) # accumulated goal
              ( bodyMedium $ Semigroupoid.do
                  ( RecordToRecord.do
                      text @"teams"
                      staticText " teams on the board — leading: " ) # shownAs identity
                  ( RecordToRecord.do
                      text @"team"
                      staticText " ("
                      text @"points" # projection show
                      staticText ")" ) # shownWhen leadingTeam
                  (staticText "—") # shownWhen noLeader ) # foreach @"key" boardSummary # muted ) # shownAs identity
      ) # mvu gameStart
