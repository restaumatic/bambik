module TimerShoelace (timerShoelace) where

import Prelude ((#), ($), Unit, const)

import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, updated, with)
import PUI.Web.HTML (shown, body, p, text)
import PUI.Web.Shoelace (button, card, progressBar, sliderLive)
import QualifiedDo.Category as Category
import TimerLogic (elapsedFraction, nothingElapsed, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerShoelace :: Effect Unit
timerShoelace =
  body $
    card $ ( Category.do
        progressBar @"Elapsed" elapsedFraction # shown
        (p $ text progressLine) # shown
        sliderLive @"Duration" {}
        every tickPeriod tick
        button @"Reset" {} # with nothingElapsed # updated (match { "Reset": const })
    ) # mvu tenSecondFreshTimer
