module TimerFluent (timerFluent) where

import Prelude ((#), ($), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (every, mvu, settled, updated, with)
import PUI.Web.Fluent (body1, button, card, progressBar, slider)
import PUI.Web.HTML (shown, body, text)
import QualifiedDo.Category as Category
import TimerLogic (nothingElapsed, presentTimer, tenSecondFreshTimer, tick, tickPeriod)

timerFluent :: Effect Unit
timerFluent =
  body $
    card $ ( Category.do
        ( RecordToRecord.do
            progressBar @"fraction"
            body1 (text @"progressLine") ) # shown
        slider @"Duration" {}
        every tickPeriod tick
        button @"Reset" {} # with nothingElapsed # updated (match { "Reset": const })
    ) # settled presentTimer # mvu tenSecondFreshTimer
