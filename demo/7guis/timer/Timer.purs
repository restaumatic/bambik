module Timer (Timer, elapsedFraction, progressLine, restarted, tenSecondFreshTimer, tick, tickPeriod) where

import Prelude ((*), (+), (/), (<), (<=), (<>), min, show)

import Data.Int (toNumber)

type Timer =
  { "Duration" ::
      { current :: Number
      , max :: Number
      , min :: Number
      , step :: [ continuous :: {}, discrete :: Number ]
      }
  , elapsedTenths :: Int
  }

tenSecondFreshTimer :: Timer
tenSecondFreshTimer =
  { "Duration": { current: 10.0, min: 0.0, max: 60.0, step: .discrete 1.0 }
  , elapsedTenths: 0
  }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 100.0 }

restarted :: Timer -> Timer
restarted t = t { elapsedTenths = 0 }

tick :: Timer -> Timer
tick t@{ "Duration": duration, elapsedTenths } =
  if toNumber elapsedTenths < 10.0 * duration.current then t { elapsedTenths = elapsedTenths + 1 } else t

elapsedFraction :: Timer -> Number
elapsedFraction t@{ "Duration": duration } =
  if duration.current <= 0.0 then 1.0 else min 1.0 (elapsedSeconds t / duration.current)

progressLine :: Timer -> String
progressLine t@{ "Duration": duration } = show (elapsedSeconds t) <> "s / " <> show duration.current <> "s"

elapsedSeconds :: Timer -> Number
elapsedSeconds { elapsedTenths } = toNumber elapsedTenths / 10.0
