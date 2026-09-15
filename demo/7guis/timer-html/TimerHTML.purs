module TimerHTML (timerHTML) where

import Prelude ((#), ($), Unit, const)

import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, updated, with)
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
    button @"Reset" (staticText "Reset") # with nothingElapsed # updated (match { "Reset": const })
  ) # mvu tenSecondFreshTimer
