module TimerLogic (elapsedLine, fraction, nothingElapsed, tenSecondFreshTimer, tick, tickPeriod, wholeSeconds) where

import Prelude ((+), (/), (<), (<=), (<>), min, show)

import Data.Maybe (Maybe(..))

tenSecondFreshTimer :: { "Duration" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, elapsed :: Number }
tenSecondFreshTimer = { "Duration": { current: 10.0, min: 0.0, max: 60.0, step: Just 1.0 }, elapsed: 0.0 }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }

nothingElapsed :: { elapsed :: Number }
nothingElapsed = { elapsed: 0.0 }

tick :: { "Duration" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, elapsed :: Number } -> Maybe { "Duration" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, elapsed :: Number }
tick t@{ "Duration": duration, elapsed } =
  if elapsed < duration.current then Just (t { elapsed = min duration.current (elapsed + 1.0) })
  else Nothing

fraction :: { "Duration" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, elapsed :: Number } -> Number
fraction { "Duration": duration, elapsed } = if duration.current <= 0.0 then 1.0 else min 1.0 (elapsed / duration.current)

wholeSeconds :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } -> String
wholeSeconds q = show q.current

elapsedLine :: { "Duration" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, elapsed :: Number } -> String
elapsedLine { "Duration": duration, elapsed } = show elapsed <> "s / " <> wholeSeconds duration <> "s"
