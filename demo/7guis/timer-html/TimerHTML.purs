module TimerHTML (timerHTML) where

import Prelude ((#), ($), Unit, const, identity)

import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, toCase, updated, with)
import PUI.Web.HTML (shown, body, button, div, label, p, progress, rangeInput, staticText, text)
import QualifiedDo.Category as Category
import TimerLogic (elapsedFraction, nothingElapsed, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerHTML :: Effect Unit
timerHTML =
  body $ div $ ( Category.do
      progress @"Elapsed" elapsedFraction # shown
      (p $ text progressLine) # shown
      p ( label $ Category.do
          (staticText "Duration ") # shown
          rangeInput @"Duration" )
      every tickPeriod tick
      button (staticText "Reset") # with nothingElapsed # toCase @"reset" identity # updated (match { reset: const })
  ) # mvu tenSecondFreshTimer
