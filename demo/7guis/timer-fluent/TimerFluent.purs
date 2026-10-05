module TimerFluent (timerFluent) where

import Prelude ((#), ($), Unit, identity)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, looped, replaying, ticks, with)
import PUI.Web.Fluent (body, body1, button, progressBar, slider)
import PUI.Web (shown, text)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerViewModel (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerFluent :: Effect Unit
timerFluent =
  body $
    ( Semigroupoid.do
      progressBar @"Elapsed" elapsedFraction # shown
      (body1 $ text progressLine) # shown
      slider @"Duration" {}
      RecordToVariant.do
        ticks @"tick" tickPeriod # replaying @"tick" identity
        button @"Reset" {}
      VariantToRecord.do
        fold @"tick" tick
        fold @"Reset" restarted
    ) # looped
      @( "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , elapsed :: Number
       ) # with tenSecondFreshTimer
