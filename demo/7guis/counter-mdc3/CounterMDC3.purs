module CounterMDC3 (counterMDC3) where

import Prelude ((#), ($), (+), (<<<), Unit, const, show)

import Data.Variant (match)
import Effect (Effect)
import PUI (completed, forField, mvu, projected, updated)
import PUI.HTML (body, text)
import PUI.MDC3 (button, card, elevation5, headlineLarge)
import QualifiedDo.Semigroupoid as Semigroupoid

counterMDC3 :: Effect Unit
counterMDC3 =
  body $
    elevation5 $
      card { caption: "Counter" } $ ( Semigroupoid.do
          headlineLarge text # forField @"count" show # completed
          button { label: "Count" } # updated (match { clicked: const <<< increment })
      ) # mvu freshCount

increment :: { count :: Int } -> { count :: Int }
increment { count } = { count: count + 1 }

freshCount :: { count :: Int }
freshCount = { count: 0 }
