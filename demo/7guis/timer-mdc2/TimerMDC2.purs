module TimerMDC2 (timerMDC2) where

import Prelude ((#), ($), (+), (/), (<), (<=), Unit, const, min, show)

import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, every, forField, mvu, projection, updates)
import PUI.HTML (body, staticText, text)
import PUI.MDC2 (body1, button, card, elevation20, linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

timerMDC2 :: Effect Unit
timerMDC2 =
  body $
    elevation20 $
      card { caption: "Timer" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              linearProgress # projection fraction
              body1 RecordToRecord.do
                text # projection show # forField @"elapsed"
                staticText "s / "
                text # projection wholeSeconds # forField @"duration"
                staticText "s"
              sliderLive { label: "" } # asField @"duration") # completed
          every tickPeriod tick
          button { label: "Reset", icon: "replay" } # lcmap reset # updates (match { clicked: const })
      ) # mvu tenSecondFreshTimer

reset :: {} -> { elapsed :: Number }
reset {} = { elapsed: 0.0 }

tick :: { duration :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, elapsed :: Number } -> Maybe { duration :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, elapsed :: Number }
tick t@{ duration, elapsed } =
  if elapsed < duration.current then Just (t { elapsed = min duration.current (elapsed + 1.0) })
  else Nothing

fraction :: { duration :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, elapsed :: Number } -> Number
fraction { duration, elapsed } = if duration.current <= 0.0 then 1.0 else min 1.0 (elapsed / duration.current)

tenSecondFreshTimer :: { duration :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, elapsed :: Number }
tenSecondFreshTimer = { duration: { current: 10.0, min: 0.0, max: 60.0, step: Just 1.0 }, elapsed: 0.0 }

wholeSeconds :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } -> String
wholeSeconds q = show q.current




tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }
