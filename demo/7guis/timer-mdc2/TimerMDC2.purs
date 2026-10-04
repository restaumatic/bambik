module TimerMDC2 (timerMDC2) where

import Prelude ((#), ($), Unit, identity)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, mvu, replaying, ticks)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body1, button, linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerViewModel (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerMDC2 :: Effect Unit
timerMDC2 =
  body $
    ( Semigroupoid.do
      linearProgress @"Elapsed" elapsedFraction # shown
      (body1 $ text progressLine) # shown
      sliderLive @"Duration" {}
      RecordToVariant.do
        ticks @"tick" tickPeriod # replaying @"tick" identity
        button @"Reset" { icon: "replay" }
      VariantToRecord.do
        fold @"tick" tick
        fold @"Reset" restarted
    ) # mvu
      @( "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , elapsed :: Number
       )
      tenSecondFreshTimer
