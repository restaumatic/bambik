module Main (main) where

import Prelude ((#), ($), (+), Unit, show)

import Effect (Effect)
import PUI (completed, forField, mvu, projection, updates)
import PUI.HTML (body, text)
import PUI.MDC (button, card, elevation20, headline4)
import QualifiedDo.Semigroupoid as Semigroupoid

main :: Effect Unit
main =
  body $ elevation20 $ card { caption: "Counter" } $ ( Semigroupoid.do
      headline4 (text # projection show # forField @"count") # completed
      button { label: "Count" } # updates increment
  ) # mvu { count: 0 }

increment :: forall click. click -> { count :: Int } -> { count :: Int }
increment _ r = { count: r.count + 1 }
