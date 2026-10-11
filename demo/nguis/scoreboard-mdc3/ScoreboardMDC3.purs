module ScoreboardMDC3 (scoreboardMDC3) where

import Prelude (Unit, identity, (#), ($))

import Effect (Effect)
import PUI (accumulated, blankStatus, fold, foreach, looped, muted, prefixed, ticks, with)
import PUI.Pipeline as Pipeline
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyMedium, list, listItem)
import ScoreboardViewModel (boardSummary, gameStart, goal, scoreLine, summaryLine, tick, tickPeriod)

scoreboardMDC3 :: Effect Unit
scoreboardMDC3 =
  body $ Pipeline.do
    ( Pipeline.do
      list $ prefixed (listItem $ text scoreLine) identity # accumulated @String @{ team :: String, points :: Int } goal
      ( bodyMedium $ text summaryLine ) # foreach @"key"
        @( key :: String
         , teams :: Int
         , leader :: [ led :: { team :: String, points :: Int }, unled :: {} ]
         ) boardSummary # muted )
    blankStatus @"Points scored" # ticks tickPeriod
    blankStatus @"Points scored" # fold tick
  # looped @( beat :: Int ) # with gameStart
