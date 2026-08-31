module TimerLogic (nothingElapsed, presentTimer, tenSecondFreshTimer, tick, tickPeriod) where

import Prelude ((+), (/), (<), (<=), min, show)

import Data.Maybe (Maybe(..))

tenSecondFreshTimer :: { "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, elapsed :: Number, fraction :: Number, elapsedText :: String, durationText :: String }
tenSecondFreshTimer = presentTimer { "Duration": { current: 10.0, min: 0.0, max: 60.0, step: .discrete 1.0 }, elapsed: 0.0, fraction: 0.0, elapsedText: "", durationText: "" }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }

nothingElapsed :: { elapsed :: Number }
nothingElapsed = { elapsed: 0.0 }

tick :: { "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, elapsed :: Number } -> Maybe { "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, elapsed :: Number }
tick t@{ "Duration": duration, elapsed } =
  if elapsed < duration.current then Just (t { elapsed = min duration.current (elapsed + 1.0) })
  else Nothing

presentTimer :: { "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, elapsed :: Number, fraction :: Number, elapsedText :: String, durationText :: String } -> { "Duration" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, elapsed :: Number, fraction :: Number, elapsedText :: String, durationText :: String }
presentTimer r = r
  { fraction = if r."Duration".current <= 0.0 then 1.0 else min 1.0 (r.elapsed / r."Duration".current)
  , elapsedText = show r.elapsed
  , durationText = show r."Duration".current
  }
