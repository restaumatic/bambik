module TimerShoelace (timerShoelace) where

import Prelude ((#), ($), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, settled, updated, with)
import PUI.Web.HTML (shown, body, p, staticText, text)
import PUI.Web.Shoelace (button, card, progressBar, sliderLive)
import QualifiedDo.Category as Category
import TimerLogic (nothingElapsed, presentTimer, tenSecondFreshTimer, tick, tickPeriod)

timerShoelace :: Effect Unit
timerShoelace =
  body $
    card $ ( Category.do
        ( RecordToRecord.do
            progressBar @"fraction"
            p RecordToRecord.do
              text @"elapsedText"
              staticText "s / "
              text @"durationText"
              staticText "s" ) # shown
        sliderLive @"Duration" {}
        every tickPeriod tick
        button @"Reset" {} # with nothingElapsed # updated (match { "Reset": const })
    ) # settled presentTimer # mvu tenSecondFreshTimer
