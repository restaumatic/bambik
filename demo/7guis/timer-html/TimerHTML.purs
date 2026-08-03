module TimerHTML (timerHTML) where

import Prelude ((#), ($), Unit, const, identity, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, every, forField, mvu, projected, toCase, updated, with)
import PUI.Web.HTML (body, button, div, label, p, progress, rangeInput, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerHTML :: Effect Unit
timerHTML =
  body $ div $ ( Semigroupoid.do
      ( RecordToRecord.do
          progress # projected fraction
          p RecordToRecord.do
            text # forField @"elapsed" show
            staticText "s / "
            text # forField @"duration" wholeSeconds
            staticText "s"
          p ( label $ RecordToRecord.do
              staticText "Duration "
              rangeInput ) # asField @"duration") # completed
      every tickPeriod tick
      button (staticText "Reset") # with nothingElapsed # toCase @"clicked" identity # updated (match { clicked: const })
  ) # mvu tenSecondFreshTimer
