module TimerMDC3 (timerMDC3) where

import Prelude ((#), ($), (+), (/), (<), (<=), Unit, const, min, show)

import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, every, forField, mvu, projected, updated, with)
import PUI.HTML (body, staticText, text)
import PUI.MDC3 (bodyLarge, button, card, elevation5, linearProgress, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

timerMDC3 :: Effect Unit
timerMDC3 =
  body $
    elevation5 $
      card { caption: "Timer" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              linearProgress # projected fraction
              bodyLarge RecordToRecord.do
                text # forField @"elapsed" show
                staticText "s / "
                text # forField @"duration" wholeSeconds
                staticText "s"
              sliderLive { label: "" } # asField @"duration") # completed
          every tickPeriod tick
          button { label: "Reset", icon: "replay" } # with nothingElapsed # updated (match { clicked: const })
      ) # mvu tenSecondFreshTimer

nothingElapsed :: { elapsed :: Number }
nothingElapsed = { elapsed: 0.0 }

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
