module TimerHTML (timerHTML) where

import Prelude ((>>>), (#), ($), Unit, const, identity, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (announce, asField, completed, every, forField, mvu, projected, toCase, updated)
import PUI.Web.HTML (body, button, div, label, p, progress, rangeInput, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerHTML :: Effect Unit
timerHTML =
  body $ div $ ( Semigroupoid.do
      ( RecordToRecord.do
          progress # projected @"value" fraction
          p RecordToRecord.do
            text # forField @"value" @"elapsed" show
            staticText "s / "
            text # forField @"value" @"duration" wholeSeconds
            staticText "s"
          p ( label $ RecordToRecord.do
              staticText "Duration "
              rangeInput ) # asField @"value" @"duration") # completed
      every tickPeriod tick
      announce nothingElapsed >>> button (staticText "Reset") # toCase @"clicked" identity # updated (match { clicked: const })
  ) # mvu tenSecondFreshTimer
