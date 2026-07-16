module Main (main) where

import Prelude

import Data.Array (replicate)
import Data.Int (round, toNumber) as Int
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (completed)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import UI (every, looped, updates)
import Web (bodyWith, text)

type Timer =
  { duration :: Number
  , elapsed :: Number
  }

main :: Effect Unit
main = bodyWith { duration: 10.0, elapsed: 0.0 } $ MDC.elevation20 $ MDC.card { caption: Just "Timer" } $ looped Semigroupoid.do
  completed RecordToRecord.do
    MDC.headline6 $ lcmap gauge text
    MDC.body1 $ lcmap (\(t :: Timer) -> format t.elapsed <> "s / " <> format t.duration <> "s") text
    MDC.slider @"duration" { label: "Duration", min: 0.0, max: 60.0, step: Just 1.0 }
  every (Milliseconds 100.0) tick
  updates (\_ t -> t { elapsed = 0.0 }) $ MDC.button @"reset" { label: Just "Reset", icon: Just "replay" }

tick :: Timer -> Maybe Timer
tick t
  | t.elapsed < t.duration = Just (t { elapsed = min t.duration (t.elapsed + 0.1) })
  | otherwise = Nothing

gauge :: Timer -> String
gauge t =
  let cells = 20
      filled = if t.duration <= 0.0 then cells else min cells (Int.round (t.elapsed / t.duration * Int.toNumber cells))
  in joinWith "" (replicate filled "█") <> joinWith "" (replicate (cells - filled) "░")

format :: Number -> String
format = toStringWith (fixed 1)
