module TimerHTML (timerHTML) where

import Prelude ((#), ($), Unit, identity)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, looped, replaying, ticks, with)
import PUI.Web (shown, staticText, text)
import PUI.Web.HTML (body, button, div, label, output, p, progress, rangeInput)
import QualifiedDo.Semigroupoid as Semigroupoid
import TimerViewModel (clockTickedLine, elapsedFraction, resetLine, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod)

timerHTML :: Effect Unit
timerHTML =
  body $ div $ ( Semigroupoid.do
    progress @"Elapsed" elapsedFraction # shown
    (p $ text progressLine) # shown
    p ( label $ Semigroupoid.do
      (staticText @"Duration ") # shown
      rangeInput @"Duration" )
    RecordToVariant.do
      ticks @"Clock ticked" tickPeriod # replaying @"Clock ticked" identity
      button @"Reset" {}
    VariantToRecord.do
      output @"Clock ticked" clockTickedLine # fold tick
      output @"Reset" resetLine # fold restarted
  ) # looped
    @( "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , elapsed :: Number
     ) # with tenSecondFreshTimer
