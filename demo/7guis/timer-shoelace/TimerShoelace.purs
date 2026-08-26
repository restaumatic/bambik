module TimerShoelace (timerShoelace) where

import Prelude ((#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (every, projection, mvu, projected, updated, with)
import PUI.Web.HTML (shownAlways, body, p, staticText, text)
import PUI.Web.Shoelace (button, card, progressBar, sliderLive)
import QualifiedDo.Semigroupoid as Pipeline
import TimerLogic (fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds)

timerShoelace :: Effect Unit
timerShoelace =
  body $
    card $ ( Pipeline.do
        ( RecordToRecord.do
            progressBar @"fraction" # projected fraction
            p RecordToRecord.do
              text @"elapsed" # projection show
              staticText "s / "
              text @"Duration" # projection wholeSeconds
              staticText "s" ) # shownAlways
        sliderLive @"Duration" {}
        every tickPeriod tick
        button @"Reset" {} # with nothingElapsed # updated (match { "Reset": const })
    ) # mvu tenSecondFreshTimer
