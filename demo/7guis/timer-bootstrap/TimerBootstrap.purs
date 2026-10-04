module TimerBootstrap (timerBootstrap) where

import Prelude ((#), ($), Unit, identity)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, mvu, replaying, ticks)
import PUI.Web.Bootstrap (body, button, progress, sliderLive)
import PUI.Web (shown, text)
import PUI.Web.HTML (p)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerViewModel (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerBootstrap :: Effect Unit
timerBootstrap =
  body $
    ( Semigroupoid.do
      progress @"Elapsed" elapsedFraction # shown
      (p $ text progressLine) # shown
      sliderLive @"Duration" {}
      RecordToVariant.do
        ticks @"tick" tickPeriod # replaying @"tick" identity
        button @"Reset" {}
      VariantToRecord.do
        fold @"tick" tick
        fold @"Reset" restarted
    ) # mvu
      @( "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , elapsed :: Number
       )
      tenSecondFreshTimer
