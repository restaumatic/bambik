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
          button { label: "Count" } # updates (match { event: \m _ -> increment m })
      ) # mvu { count: 0 }

increment :: { count :: Int } -> { count :: Int }
increment r = { count: r.count + 1 }
