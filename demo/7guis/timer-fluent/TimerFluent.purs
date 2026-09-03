module TimerFluent (timerFluent) where

import Prelude ((#), ($), Unit, const)

import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, updated, with)
import PUI.Web.Fluent (body1, button, card, progressBar, slider)
import PUI.Web.HTML (shown, body, text)
import QualifiedDo.Category as Category
import TimerLogic (elapsedFraction, nothingElapsed, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerFluent :: Effect Unit
timerFluent =
  body $
    card $ ( Category.do
        progressBar @"Elapsed" elapsedFraction # shown
        (body1 $ text progressLine) # shown
        slider @"Duration" {}
        every tickPeriod tick
        button @"Reset" {} # with nothingElapsed # updated (match { "Reset": const })
    ) # mvu tenSecondFreshTimer
