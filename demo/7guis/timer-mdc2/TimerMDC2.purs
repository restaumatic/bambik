module TimerMDC2 (timerMDC2) where

import Prelude ((#), ($), Unit, identity)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (blankStatus, fold, looped, ticks, with)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body1, button, linearProgress, sliderLive, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerViewModel (elapsedFraction, resetLine, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerMDC2 :: Effect Unit
timerMDC2 =
  body $
    Semigroupoid.do
      linearProgress elapsedFraction # shown
      (body1 $ text progressLine) # shown
      sliderLive @"Duration" {}
      RecordToVariant.do
        blankStatus @"Clock ticked" # ticks tickPeriod
        button @"Reset" { icon: "replay" }
      VariantToRecord.do
        blankStatus @"Clock ticked" # fold tick
        snackbar @"Reset" resetLine # fold restarted
    # looped
      @( "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , elapsed :: Number
       ) # with tenSecondFreshTimer
