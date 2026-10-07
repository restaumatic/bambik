module TimerShoelace (timerShoelace) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (blankStatus, fold, looped, ticks, with)
import PUI.Web (shown, text)
import PUI.Web.HTML (p)
import PUI.Web.Shoelace (body, button, progressBar, sliderLive, toast)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerViewModel (elapsedFraction, resetLine, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerShoelace :: Effect Unit
timerShoelace =
  body $
    Semigroupoid.do
      progressBar elapsedFraction # shown
      (p $ text progressLine) # shown
      sliderLive @"Duration" {}
      RecordToVariant.do
        blankStatus @"Clock ticked" # ticks tickPeriod
        button @"Reset" {}
      VariantToRecord.do
        blankStatus @"Clock ticked" # fold tick
        toast @"Reset" resetLine # fold restarted
    # looped
      @( "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , elapsed :: Number
       ) # with tenSecondFreshTimer
