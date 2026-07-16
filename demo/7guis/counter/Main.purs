module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord (completed, forField)
import Effect (Effect)
import PUI (looped, updates, with)
import PUI.HTML (body, text) as HTML
import PUI.MDC (button, card, elevation20, headline4) as MDC
import QualifiedDo.Semigroupoid as Semigroupoid

main :: Effect Unit
main =
  HTML.body $ MDC.elevation20 $ MDC.card { caption: Just "Counter" } $ ( Semigroupoid.do
      MDC.headline4 (HTML.text # forField @"count") # completed
      MDC.button { label: Just "Count", icon: Nothing } # updates increment
  ) # with { count: 0 } # looped

increment :: forall click. click -> { count :: Int } -> { count :: Int }
increment _ r = { count: r.count + 1 }
