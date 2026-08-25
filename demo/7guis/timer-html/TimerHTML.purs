module TimerHTML (timerHTML) where

import Prelude ((#), ($), Unit, const, identity, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (every, projection, mvu, projected, toCase, updated, with)
import PUI.Web.HTML (shownAs, body, button, div, label, p, progress, rangeInput, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerHTML :: Effect Unit
timerHTML =
  body $ div $ ( Semigroupoid.do
      shownAs identity ( RecordToRecord.do
          progress @"fraction" # projected fraction
          p RecordToRecord.do
            text @"elapsed" # projection show
            staticText "s / "
            text @"Duration" # projection wholeSeconds
            staticText "s" )
      p ( label $ Semigroupoid.do
          shownAs identity (staticText "Duration ")
          rangeInput @"Duration" )
      every tickPeriod tick
      button (staticText "Reset") # with nothingElapsed # toCase @"reset" identity # updated (match { reset: const })
  ) # mvu tenSecondFreshTimer
