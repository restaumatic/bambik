module TimerLogic (elapsedFraction, nothingElapsed, progressLine, tenSecondFreshTimer, tick, tickPeriod) where

import Prelude ((/), (+), (<), (<=), (<>), min, show)

import Data.Maybe (Maybe(..))

tenSecondFreshTimer :: { "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, elapsed :: Number }
tenSecondFreshTimer = { "Duration": { current: 10.0, min: 0.0, max: 60.0, step: .discrete 1.0 }, elapsed: 0.0 }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }

nothingElapsed :: { elapsed :: Number }
nothingElapsed = { elapsed: 0.0 }

tick :: { "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, elapsed :: Number } -> Maybe { "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, elapsed :: Number }
tick t@{ "Duration": duration, elapsed } =
  if elapsed < duration.current then Just (t { elapsed = min duration.current (elapsed + 1.0) })
  else Nothing

elapsedFraction :: { "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, elapsed :: Number } -> Number
elapsedFraction { "Duration": duration, elapsed } =
  if duration.current <= 0.0 then 1.0 else min 1.0 (elapsed / duration.current)

progressLine :: { "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, elapsed :: Number } -> String
progressLine { "Duration": duration, elapsed } = show elapsed <> "s / " <> show duration.current <> "s"
