module Main (main) where

import Prelude

import Data.Array (replicate)
import Data.Int (round, toNumber) as Int
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap, lcmap)
import Data.Profunctor.Row.RecordToRecord (asField, completed, forField, projection)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import PUI (every, looped, updates, with)
import PUI.HTML (body, staticText, text) as HTML
import PUI.MDC (body1, button, card, elevation20, headline6, slider) as MDC
import QualifiedDo.Semigroupoid as Semigroupoid

type Timer =
  { duration :: Int
  , elapsed :: Int
  }

main :: Effect Unit
main =
  HTML.body $ MDC.elevation20 $ MDC.card { caption: Just "Timer" } $ ( Semigroupoid.do
      ( RecordToRecord.do
          MDC.headline6 (HTML.text # lcmap ({ value: _ } <<< gauge))
          MDC.body1 RecordToRecord.do
            HTML.text # projection show # forField @"elapsed"
            HTML.staticText "s / "
            HTML.text # projection show # forField @"duration"
            HTML.staticText "s"
          MDC.slider { label: "Duration", min: 0.0, max: 60.0, step: Just 1.0 }
            # dimap (\{ value: i } -> { value: Int.toNumber i }) (\{ value: n } -> { value: Int.round n })
            # asField @"duration"
      ) # completed
      every (Milliseconds 1000.0) tick
      MDC.button { label: Just "Reset", icon: Just "replay" } # updates reset
  ) # with { duration: 10, elapsed: 0 } # looped

reset :: forall click. click -> Timer -> Timer
reset _ t = t { elapsed = 0 }

tick :: Timer -> Maybe Timer
tick t
  | t.elapsed < t.duration = Just (t { elapsed = min t.duration (t.elapsed + 1) })
  | otherwise = Nothing

gauge :: Timer -> String
gauge t =
  let cells = 20
      filled = if t.duration <= 0 then cells else min cells (t.elapsed * cells / t.duration)
  in joinWith "" (replicate filled "█") <> joinWith "" (replicate (cells - filled) "░")
