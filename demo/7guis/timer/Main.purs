module Main (main) where

import Prelude ((#), ($), (+), (/), (<), (<=), Unit, min, otherwise, show)

import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import PUI (asField, completed, every, forField, forValue, mvu, projection, updates)
import PUI.HTML (body, staticText, text) as HTML
import PUI.MDC (body1, button, card, elevation20, linearProgress, sliderLive) as MDC
import QualifiedDo.Semigroupoid as Semigroupoid

type Timer =
  { duration :: Number
  , elapsed :: Number
  }

main :: Effect Unit
main =
  HTML.body $ MDC.elevation20 $ MDC.card { caption: Just "Timer" } $ ( Semigroupoid.do
      ( RecordToRecord.do
          MDC.linearProgress # projection fraction # forValue
          MDC.body1 RecordToRecord.do
            HTML.text # projection show # forField @"elapsed"
            HTML.staticText "s / "
            HTML.text # projection show # forField @"duration"
            HTML.staticText "s"
          MDC.sliderLive { label: "Duration", min: 0.0, max: 60.0, step: Just 1.0 } # asField @"duration"
      ) # completed
      every (Milliseconds 1000.0) tick
      MDC.button { label: Just "Reset", icon: Just "replay" } # updates reset
  ) # mvu { duration: 10.0, elapsed: 0.0 }

reset :: forall click. click -> Timer -> Timer
reset _ t = t { elapsed = 0.0 }

tick :: Timer -> Maybe Timer
tick t
  | t.elapsed < t.duration = Just (t { elapsed = min t.duration (t.elapsed + 1.0) })
  | otherwise = Nothing

fraction :: Timer -> Number
fraction t = if t.duration <= 0.0 then 1.0 else min 1.0 (t.elapsed / t.duration)
