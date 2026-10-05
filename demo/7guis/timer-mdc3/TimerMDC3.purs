module TimerMDC3 (timerMDC3) where

import Prelude ((#), ($), Unit, identity)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, looped, replaying, ticks, with)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyLarge, button, linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerViewModel (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerMDC3 :: Effect Unit
timerMDC3 =
  body $
    ( Semigroupoid.do
      linearProgress @"Elapsed" elapsedFraction # shown
      (bodyLarge $ text progressLine) # shown
      sliderLive @"Duration" {}
      RecordToVariant.do
        ticks @"tick" tickPeriod # replaying @"tick" identity
        button @"Reset" { icon: "replay" }
      VariantToRecord.do
        fold @"tick" tick
        fold @"Reset" restarted
    ) # looped
      @( "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , elapsed :: Number
       ) # with tenSecondFreshTimer
