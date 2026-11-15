module Timer.MDC3 (timer) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Effect (Effect)
import PUI (edits)
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyLarge, button', linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import Timer (elapsedFraction, progressLine, timed)

timer :: Effect Unit
timer =
  body $ Semigroupoid.do
    linearProgress elapsedFraction
    bodyLarge $ text progressLine
    RecordToVariant.do
      sliderLive @"Duration" # edits @"Duration changed"
      button' @"Reset" { icon: "replay" }
  # timed
