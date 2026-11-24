module Timer (Moment, Timed, Timer, TimerRow, elapsedFraction, progressLine, rerun, restarted, tenSecondFreshTimer, tickPeriod) where

import Prelude ((*), (+), (-), (/), (<=), (<>), (==), (>=), min, show)

import Data.Int (floor, toNumber)

type Timer = { | TimerRow () }

type Timed = { | TimerRow ( time :: Moment ) }

type TimerRow r =
  ( "Duration" ::
      { current :: Number
      , max :: Number
      , min :: Number
      , step :: [ continuous :: {}, discrete :: Number ]
      }
  , changed :: Moment
  , run :: { elapsed :: Number, limit :: Number, since :: Moment }
  | r
  )

type Moment = { ms :: Number }

tenSecondFreshTimer :: Timer
tenSecondFreshTimer =
  { "Duration": { current: 10.0, min: 0.0, max: 60.0, step: .discrete 1.0 }
  , changed: { ms: 0.0 }
  , run: { elapsed: 0.0, limit: 10.0, since: { ms: 0.0 } }
  }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 100.0 }

restarted :: { event :: Timed, model :: Timer } -> Timer
restarted { event: { time }, model } = model { run = { elapsed: 0.0, limit: model."Duration".current, since: time } }

rerun :: Timer -> Timer
rerun t@{ "Duration": duration, changed, run } =
  if run.limit == duration.current then t
  else t { run = { elapsed: elapsedAt changed run, limit: duration.current, since: changed } }

elapsedFraction :: Timed -> Number
elapsedFraction { run, time } =
  if run.limit <= 0.0 then 1.0 else min 1.0 (elapsedAt time run / (1000.0 * run.limit))

progressLine :: Timed -> String
progressLine { run, time } = show (toNumber (floor (elapsedAt time run / 100.0)) / 10.0) <> "s / " <> show run.limit <> "s"

elapsedAt :: Moment -> { elapsed :: Number, limit :: Number, since :: Moment } -> Number
elapsedAt moment { elapsed, limit, since } =
  if elapsed >= 1000.0 * limit then elapsed else min (1000.0 * limit) (elapsed + moment.ms - since.ms)
