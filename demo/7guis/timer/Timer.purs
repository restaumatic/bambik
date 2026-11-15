module Timer (Timer, elapsedFraction, progressLine, timed) where

import Prelude ((#), (*), (+), (/), (<), (<=), (<>), min, show)

import Data.Int (toNumber)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import PUI (class Scoped, PUI, capture, fold, looped, ticks, with)
import QualifiedDo.Semigroupoid as Semigroupoid

type Timer =
  { "Duration" ::
      { current :: Number
      , max :: Number
      , min :: Number
      , step :: [ continuous :: {}, discrete :: Number ]
      }
  , elapsedTenths :: Int
  }

timed
  :: forall m
   . Scoped m
  => PUI m Timer [ "Duration changed" :: Timer, "Reset" :: Timer ]
  -> PUI m {} Timer
timed view =
  Semigroupoid.do
    RecordToVariant.do
      view
      ticks @"Clock ticked" tickPeriod
    VariantToRecord.do
      capture @"Duration changed"
      fold @"Clock ticked" tick
      fold @"Reset" restarted
  # looped # with tenSecondFreshTimer

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
