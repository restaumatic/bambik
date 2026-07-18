module Timer (timer) where

import Prelude ((#), ($), (+), (/), (<), (<=), Unit, min, show)

import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import PUI (asField, completed, every, forField, forValue, mvu, projection, updates)
import PUI.HTML (body, staticText, text)
import PUI.MDC (body1, button, card, elevation20, linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

type Timer =
  { duration :: Number
  , elapsed :: Number
  }

timer :: Effect Unit
timer =
  body $
    elevation20 $
      card { caption: "Timer" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              linearProgress # projection fraction # forValue
              body1 RecordToRecord.do
                text # projection show # forField @"elapsed"
                staticText "s / "
                text # projection show # forField @"duration"
                staticText "s"
              sliderLive { min: minDuration, max: maxDuration, step: durationStep } # asField @"duration"
          ) # completed
          every tickPeriod tick
          button { label: "Reset", icon: "replay" } # updates (match { clicked: \t _ -> reset t })
      ) # mvu tenSecondTimer

reset :: Timer -> Timer
reset t = t { elapsed = 0.0 }

tick :: Timer -> Maybe Timer
tick t =
  if t.elapsed < t.duration then Just (t { elapsed = min t.duration (t.elapsed + 1.0) })
  else Nothing

fraction :: Timer -> Number
fraction t = if t.duration <= 0.0 then 1.0 else min 1.0 (t.elapsed / t.duration)

tenSecondTimer :: Timer
tenSecondTimer = { duration: 10.0, elapsed: 0.0 }

minDuration :: Number
minDuration = 0.0

maxDuration :: Number
maxDuration = 60.0

durationStep :: Number
durationStep = 1.0

tickPeriod :: Milliseconds
tickPeriod = Milliseconds 1000.0
