module Timer.MDC2 (timer) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Effect (Effect)
import PUI (edits)
import PUI.Web (text)
import PUI.Web.MDC2 (body, body1, button', linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid
import Timer (elapsedFraction, progressLine, timed)

timer :: Effect Unit
timer =
  body $ Semigroupoid.do
    linearProgress elapsedFraction
    body1 $ text progressLine
    RecordToVariant.do
      sliderLive @"Duration" # edits @"Duration changed"
      button' @"Reset" { icon: "replay" }
  # timed
