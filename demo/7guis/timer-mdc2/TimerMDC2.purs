module TimerMDC2 (timerMDC2) where

import Prelude ((#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (every, projection, mvu, projected, updated, with)
import PUI.Web.HTML (shown, body, staticText, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, linearProgress, sliderLive)
import QualifiedDo.Category as Category
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerMDC2 :: Effect Unit
timerMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          ( RecordToRecord.do
              linearProgress @"fraction" # projected fraction
              body1 RecordToRecord.do
                text @"elapsed" # projection show
                staticText "s / "
                text @"Duration" # projection wholeSeconds
                staticText "s" ) # shown
          sliderLive @"Duration" {}
          every tickPeriod tick
          button @"Reset" { icon: "replay" } # with nothingElapsed # updated (match { "Reset": const })
      ) # mvu tenSecondFreshTimer
