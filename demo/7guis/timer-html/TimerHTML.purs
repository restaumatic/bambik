module TimerHTML (timerHTML) where

import Prelude ((#), ($), Unit, const, identity)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, settled, toCase, updated, with)
import PUI.Web.HTML (shown, body, button, div, label, p, progress, rangeInput, staticText, text)
import QualifiedDo.Category as Category
import TimerLogic (nothingElapsed, presentTimer, tenSecondFreshTimer, tick, tickPeriod)

timerHTML :: Effect Unit
timerHTML =
  body $ div $ ( Category.do
      ( RecordToRecord.do
          progress @"fraction"
          p (text @"progressLine") ) # shown
      p ( label $ Category.do
          (staticText "Duration ") # shown
          rangeInput @"Duration" )
      every tickPeriod tick
      button (staticText "Reset") # with nothingElapsed # toCase @"reset" identity # updated (match { reset: const })
  ) # settled presentTimer # mvu tenSecondFreshTimer
