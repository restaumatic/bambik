module TimerMDC2 (timerMDC2) where

import Prelude ((#), ($), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, settled, updated, with)
import PUI.Web.HTML (shown, body, staticText, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, linearProgress, sliderLive)
import QualifiedDo.Category as Category
import TimerLogic (nothingElapsed, presentTimer, tenSecondFreshTimer, tick, tickPeriod)

timerMDC2 :: Effect Unit
timerMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          ( RecordToRecord.do
              linearProgress @"fraction"
              body1 RecordToRecord.do
                text @"elapsedText"
                staticText "s / "
                text @"durationText"
                staticText "s" ) # shown
          sliderLive @"Duration" {}
          every tickPeriod tick
          button @"Reset" { icon: "replay" } # with nothingElapsed # updated (match { "Reset": const })
      ) # settled presentTimer # mvu tenSecondFreshTimer
