module TimerMDC2 (timerMDC2) where

import Prelude ((#), ($), Unit, const)

import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, updated, with)
import PUI.Web.HTML (shown, body, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, linearProgress, sliderLive)
import QualifiedDo.Category as Category
import TimerLogic (elapsedFraction, nothingElapsed, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerMDC2 :: Effect Unit
timerMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          linearProgress @"Elapsed" elapsedFraction # shown
          (body1 $ text progressLine) # shown
          sliderLive @"Duration" {}
          every tickPeriod tick
          button @"Reset" { icon: "replay" } # with nothingElapsed # updated (match { "Reset": const })
      ) # mvu tenSecondFreshTimer
