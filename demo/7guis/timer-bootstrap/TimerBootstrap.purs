module TimerBootstrap (timerBootstrap) where

import Prelude ((#), ($), Unit, identity)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, looped, replaying, ticks, with)
import PUI.Web.Bootstrap (body, button, progress, sliderLive, toast)
import PUI.Web (shown, text)
import PUI.Web.HTML (p)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerViewModel (clockTickedLine, elapsedFraction, resetLine, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerBootstrap :: Effect Unit
timerBootstrap =
  body $
    ( Semigroupoid.do
      progress @"Elapsed" elapsedFraction # shown
      (p $ text progressLine) # shown
      sliderLive @"Duration" {}
      RecordToVariant.do
        ticks @"Clock ticked" tickPeriod # replaying @"Clock ticked" identity
        button @"Reset" {}
      VariantToRecord.do
        toast @"Clock ticked" clockTickedLine # fold tick
        toast @"Reset" resetLine # fold restarted
    ) # looped
      @( "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , elapsed :: Number
       ) # with tenSecondFreshTimer
