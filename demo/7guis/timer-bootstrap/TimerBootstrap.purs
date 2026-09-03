module TimerBootstrap (timerBootstrap) where

import Prelude ((#), ($), Unit, const)

import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, updated, with)
import PUI.Web.Bootstrap (button, card, progress, sliderLive)
import PUI.Web.HTML (shown, body, p, text)
import QualifiedDo.Category as Category
import TimerLogic (elapsedFraction, nothingElapsed, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerBootstrap :: Effect Unit
timerBootstrap =
  body $
    card $ ( Category.do
        progress @"Elapsed" elapsedFraction # shown
        (p $ text progressLine) # shown
        sliderLive @"Duration" {}
        every tickPeriod tick
        button @"Reset" {} # with nothingElapsed # updated (match { "Reset": const })
    ) # mvu tenSecondFreshTimer
