module TimerViewModel (elapsedFraction, restarted, progressLine, tenSecondFreshTimer, tick, tickPeriod) where

import Prelude ((/), (+), (<), (<=), (<>), min, show)

tenSecondFreshTimer :: { "Duration" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, elapsed :: Number }
tenSecondFreshTimer = { "Duration": { current: 10.0, min: 0.0, max: 60.0, step: .discrete 1.0 }, elapsed: 0.0 }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }

restarted :: { "Duration" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, elapsed :: Number } -> { "Duration" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, elapsed :: Number }
restarted t = t { elapsed = 0.0 }

tick :: { "Duration" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, elapsed :: Number } -> { "Duration" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, elapsed :: Number }
tick t@{ "Duration": duration, elapsed } =
  if elapsed < duration.current then t { elapsed = min duration.current (elapsed + 1.0) } else t

elapsedFraction :: { "Duration" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, elapsed :: Number } -> Number
elapsedFraction { "Duration": duration, elapsed } =
  if duration.current <= 0.0 then 1.0 else min 1.0 (elapsed / duration.current)

progressLine :: { "Duration" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, elapsed :: Number } -> String
progressLine { "Duration": duration, elapsed } = show elapsed <> "s / " <> show duration.current <> "s"
