module Counter (counter) where

import Prelude ((#), ($), (+), Unit, show)

import Data.Variant (match)
import Effect (Effect)
import PUI (completed, forField, mvu, projection, updates)
import PUI.HTML (body, text)
import PUI.MDC (button, card, elevation20, headline4)
import QualifiedDo.Semigroupoid as Semigroupoid

counter :: Effect Unit
counter =
  body $
    elevation20 $
      card { caption: "Counter" } $ ( Semigroupoid.do
          headline4 (text # projection show # forField @"count") # completed
          button { label: "Count" } # updates (match { clicked: \m _ -> increment m })
      ) # mvu zeroCount

increment :: { count :: Int } -> { count :: Int }
increment r = { count: r.count + 1 }

zeroCount :: { count :: Int }
zeroCount = { count: 0 }
