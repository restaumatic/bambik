module CounterMD3 (counterMD3) where

import Prelude ((#), ($), (+), (<<<), Unit, const, show)

import Data.Variant (match)
import Effect (Effect)
import PUI (completed, forField, mvu, projection, updates)
import PUI.HTML (body, text)
import PUI.MDC3 (button, card, elevation5, headlineLarge)
import QualifiedDo.Semigroupoid as Semigroupoid

counterMD3 :: Effect Unit
counterMD3 =
  body $
    elevation5 $
      card { caption: "Counter" } $ ( Semigroupoid.do
          headlineLarge text # projection show # forField @"count" # completed
          button { label: "Count" } # updates (match { clicked: const <<< increment })
      ) # mvu freshCount

increment :: { count :: Int } -> { count :: Int }
increment { count } = { count: count + 1 }

freshCount :: { count :: Int }
freshCount = { count: 0 }
