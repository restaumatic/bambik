module Main (main) where

import Prelude ((#), ($), (+), (/), (<), (<=), Unit, min, otherwise, show)

import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
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

main :: Effect Unit
main =
  body $ elevation20 $ card { caption: Just "Timer" } $ ( Semigroupoid.do
      ( RecordToRecord.do
          linearProgress # projection fraction # forValue
          body1 RecordToRecord.do
            text # projection show # forField @"elapsed"
            staticText "s / "
            text # projection show # forField @"duration"
            staticText "s"
          sliderLive { label: "Duration", min: 0.0, max: 60.0, step: Just 1.0 } # asField @"duration"
      ) # completed
      every (Milliseconds 1000.0) tick
      button { label: Just "Reset", icon: Just "replay" } # updates reset
  ) # mvu { duration: 10.0, elapsed: 0.0 }

reset :: forall click. click -> Timer -> Timer
reset _ t = t { elapsed = 0.0 }

tick :: Timer -> Maybe Timer
tick t
  | t.elapsed < t.duration = Just (t { elapsed = min t.duration (t.elapsed + 1.0) })
  | otherwise = Nothing

fraction :: Timer -> Number
fraction t = if t.duration <= 0.0 then 1.0 else min 1.0 (t.elapsed / t.duration)
