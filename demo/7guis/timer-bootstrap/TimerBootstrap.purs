module TimerBootstrap (timerBootstrap) where

import Prelude ((#), ($), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, settled, updated, with)
import PUI.Web.Bootstrap (button, card, progress, sliderLive)
import PUI.Web.HTML (shown, body, p, staticText, text)
import QualifiedDo.Category as Category
import TimerLogic (nothingElapsed, presentTimer, tenSecondFreshTimer, tick, tickPeriod)

timerBootstrap :: Effect Unit
timerBootstrap =
  body $
    card $ ( Category.do
        ( RecordToRecord.do
            progress @"fraction"
            p RecordToRecord.do
              text @"elapsedText"
              staticText "s / "
              text @"durationText"
              staticText "s" ) # shown
        sliderLive @"Duration" {}
        every tickPeriod tick
        button @"Reset" {} # with nothingElapsed # updated (match { "Reset": const })
    ) # settled presentTimer # mvu tenSecondFreshTimer
