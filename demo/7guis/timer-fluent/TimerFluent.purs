module TimerFluent (timerFluent) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (blankStatus, fold, looped, ticks, with)
import PUI.Web.Fluent (body, body1, button, messageBar, progressBar, slider)
import PUI.Web (shown, text)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerViewModel (elapsedFraction, resetLine, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerFluent :: Effect Unit
timerFluent =
  body $
    Semigroupoid.do
      progressBar elapsedFraction # shown
      (body1 $ text progressLine) # shown
      slider @"Duration" {}
      RecordToVariant.do
        blankStatus @"Clock ticked" # ticks tickPeriod
        button @"Reset" {}
      VariantToRecord.do
        blankStatus @"Clock ticked" # fold tick
        messageBar @"Reset" resetLine # fold restarted
    # looped
      @( "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , elapsed :: Number
       ) # with tenSecondFreshTimer
