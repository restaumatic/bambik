-- | 7GUIs task 4: **Timer** — an elapsed gauge, an elapsed label, a
-- | duration slider, and a reset button; the timer ticks continuously.
-- |
-- | The tick source is a custom leaf (`ticker`): an echo wire with a
-- | heartbeat — it retains the last model and, while `elapsed < duration`,
-- | emits an advanced copy every interval. Everything else is standard
-- | vocabulary: the gauge and label are display operands of a record
-- | merge, the duration slider edits its field live, the reset button is
-- | the `×→+` event citizen, and `looped` ties the self-trace so every
-- | tick, drag, and click re-renders the whole ensemble.
module Main (main) where

import Prelude

import Data.Foldable (for_)
import Data.Int (round, toNumber) as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord (field)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (recordToCase)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.String (joinWith)
import Data.Array (replicate)
import Data.Variant (case_, on) as Variant
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import Type.Proxy (Proxy(..))
import UI (UI, looped, silence)
import Web (Web, body, text)

type Timer =
  { duration :: Number
  , elapsed :: Number
  }

main :: Effect Unit
main = body @Unit $ MDC.elevation20 $ MDC.card { caption: Just "Timer" } Semigroupoid.do
  lcmap (const { duration: 10.0, elapsed: 0.0 }) $ looped Semigroupoid.do
    RecordToRecord.do
      MDC.headline6 $ lcmap gauge text
      MDC.body1 $ lcmap (\(t :: Timer) -> format t.elapsed <> "s / " <> format t.duration <> "s") text
      MDC.slider @"duration" { label: "Duration", min: 0.0, max: 60.0, step: Just 1.0 }
      -- elapsed has no editor; the echo wire carries it through the merge
      field @"elapsed" identity
    ticker 0.1
    RecordToVariant.do
      MDC.button @"reset" { label: Just "Reset", icon: Just "replay" }
      (recordToCase @"state" identity :: UI Web Timer [ state :: Timer ])
    rmap handle identity
  silence

handle :: [ reset :: Timer, state :: Timer ] -> Timer
handle = Variant.case_
  # Variant.on (Proxy @"reset") (_ { elapsed = 0.0 })
  # Variant.on (Proxy @"state") identity

gauge :: Timer -> String
gauge t =
  let cells = 20
      filled = if t.duration <= 0.0 then cells else min cells (Int.round (t.elapsed / t.duration * Int.toNumber cells))
  in joinWith "" (replicate filled "█") <> joinWith "" (replicate (cells - filled) "░")

format :: Number -> String
format n = show (Int.round (n * 10.0) / 10) <> "." <> show (Int.round (n * 10.0) `mod` 10)

-- | The tick source: an echo wire with a heartbeat. Retains the last model
-- | fed; while `elapsed < duration`, emits an advanced copy every
-- | `intervalSeconds`. A candidate library leaf (`Web`-monad-free — it
-- | builds no DOM).
ticker :: Number -> UI Web Timer Timer
ticker intervalSeconds = wrap $ liftEffect do
  lastRef <- Ref.new Nothing
  mPropRef <- Ref.new Nothing
  pure
    { toUser: \t -> do
        Ref.write (Just t) lastRef
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> void $ prop t
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        let
          loop = do
            delay (Milliseconds (intervalSeconds * 1000.0))
            liftEffect do
              mt <- Ref.read lastRef
              for_ mt \t ->
                when (t.elapsed < t.duration) do
                  let t' = t { elapsed = min t.duration (t.elapsed + intervalSeconds) }
                  Ref.write (Just t') lastRef
                  void $ prop t'
            loop
        launchAff_ loop
    }
