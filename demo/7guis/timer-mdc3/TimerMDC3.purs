module TimerMDC3 (timerMDC3) where

import Prelude ((#), ($), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, settled, updated, with)
import PUI.Web.HTML (shown, body, staticText, text)
import PUI.Web.MDC3 (bodyLarge, button, card, elevation5, linearProgress, sliderLive)
import QualifiedDo.Category as Category
import TimerLogic (nothingElapsed, presentTimer, tenSecondFreshTimer, tick, tickPeriod)

timerMDC3 :: Effect Unit
timerMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          ( RecordToRecord.do
              linearProgress @"fraction"
              bodyLarge RecordToRecord.do
                text @"elapsedText"
                staticText "s / "
                text @"durationText"
                staticText "s" ) # shown
          sliderLive @"Duration" {}
          every tickPeriod tick
          button @"Reset" { icon: "replay" } # with nothingElapsed # updated (match { "Reset": const })
      ) # settled presentTimer # mvu tenSecondFreshTimer
