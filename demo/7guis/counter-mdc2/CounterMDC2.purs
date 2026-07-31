module CounterMDC2 (counterMDC2) where

import Prelude ((#), ($), (+), (<<<), Unit, const, show)

import Data.Variant (match)
import Effect (Effect)
import PUI (completed, forField, mvu, projection, updates)
import PUI.HTML (body, text)
import PUI.MDC2 (button, card, elevation20, headline4)
import QualifiedDo.Semigroupoid as Semigroupoid

counterMDC2 :: Effect Unit
counterMDC2 =
  body $
    elevation20 $
      card { caption: "Counter" } $ ( Semigroupoid.do
          headline4 text # projection show # forField @"count" # completed
          button { label: "Count" } # updates (match { clicked: const <<< increment })
      ) # mvu freshCount

increment :: { count :: Int } -> { count :: Int }
increment { count } = { count: count + 1 }

freshCount :: { count :: Int }
freshCount = { count: 0 }
