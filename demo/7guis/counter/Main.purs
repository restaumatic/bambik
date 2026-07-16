-- | 7GUIs task 1: **Counter** — a label and a button that increments it.
-- |
-- | The whole app is one `×`-diagonal self-trace: display → event button →
-- | increment, `looped` so every emission re-renders the display. The
-- | count display is a `tapped` stage (a live view that passes the value
-- | on), the button is the `×→+` event citizen, and the increment is a
-- | plain model function on its output case.
module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord (tapped)
import Data.Variant (case_, on) as Variant
import Effect (Effect)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import Type.Proxy (Proxy(..))
import UI (looped, silence)
import Web (body, text)

main :: Effect Unit
main = body @Unit $ MDC.elevation20 $ MDC.card { caption: Just "Counter" } Semigroupoid.do
  lcmap (const { count: 0 }) $ looped Semigroupoid.do
    tapped $ MDC.headline4 $ lcmap (\r -> show r.count) text
    MDC.button @"count" { label: Just "Count", icon: Nothing }
    rmap increment identity
  silence

increment :: [ count :: { count :: Int } ] -> { count :: Int }
increment = Variant.case_ # Variant.on (Proxy @"count") \r -> { count: r.count + 1 }
