module TimerHTML (timerHTML) where

import Prelude ((#), ($), Unit, const)

import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, updated, with)
import PUI.Web (shown, staticText, text)
import PUI.Web.HTML (body, button, div, label, p, progress, rangeInput)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (elapsedFraction, nothingElapsed, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerHTML :: Effect Unit
timerHTML =
  body $ div $ ( Semigroupoid.do
    progress @"Elapsed" elapsedFraction # shown
    (p $ text progressLine) # shown
    p ( label $ Semigroupoid.do
      (staticText "Duration ") # shown
      rangeInput @"Duration" )
    every tickPeriod tick
    button @"Reset" {} # with nothingElapsed # updated (match { "Reset": const })
  ) # mvu tenSecondFreshTimer
