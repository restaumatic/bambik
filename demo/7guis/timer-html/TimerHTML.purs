module TimerHTML (timerHTML) where

import Prelude ((>>>), (#), ($), Unit, const, identity, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (announce, completed, every, projection, mvu, projected, toCase, updated)
import PUI.Web.HTML (body, button, div, label, p, progress, rangeInput, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerHTML :: Effect Unit
timerHTML =
  body $ div $ ( Semigroupoid.do
      ( RecordToRecord.do
          progress @"fraction" # projected fraction
          p RecordToRecord.do
            text @"elapsed" # projection show
            staticText "s / "
            text @"duration" # projection wholeSeconds
            staticText "s"
          p ( label $ RecordToRecord.do
              staticText "Duration "
              rangeInput @"duration" )) # completed
      every tickPeriod tick
      announce nothingElapsed >>> button (staticText "Reset") # toCase @"reset" identity # updated (match { reset: const })
  ) # mvu tenSecondFreshTimer
