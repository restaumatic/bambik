module Timer (timer) where

import Prelude ((#), ($), (+), (/), (<), (<=), Unit, const, min, show)

import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, every, forField, mvu, projection, updates)
import PUI.HTML (body, staticText, text)
import PUI.MDC (body1, button, card, elevation20, linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

timer :: Effect Unit
timer =
  body $
    elevation20 $
      card { caption: "Timer" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              linearProgress # projection fraction
              body1 RecordToRecord.do
                text # projection show # forField @"elapsed"
                staticText "s / "
                text # projection show # forField @"duration"
                staticText "s"
              sliderLive { min: minDuration, max: maxDuration, step: durationStep } # asField @"duration") # completed
          every tickPeriod tick
          button { label: "Reset", icon: "replay" } # lcmap reset # updates (match { clicked: const })
      ) # mvu tenSecondFreshTimer

reset :: {} -> { elapsed :: Number }
reset {} = { elapsed: 0.0 }

tick :: { duration :: Number, elapsed :: Number } -> Maybe { duration :: Number, elapsed :: Number }
tick t@{ duration, elapsed } =
  if elapsed < duration then Just (t { elapsed = min duration (elapsed + 1.0) })
  else Nothing

fraction :: { duration :: Number, elapsed :: Number } -> Number
fraction { duration, elapsed } = if duration <= 0.0 then 1.0 else min 1.0 (elapsed / duration)

tenSecondFreshTimer :: { duration :: Number, elapsed :: Number }
tenSecondFreshTimer = { duration: 10.0, elapsed: 0.0 }

minDuration :: Number
minDuration = 0.0

maxDuration :: Number
maxDuration = 60.0

durationStep :: Number
durationStep = 1.0

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }
