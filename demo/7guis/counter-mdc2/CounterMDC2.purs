module CounterMDC2 (counterMDC2) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, projection, mvu, updated)
import PUI.Web.HTML (body, text)
import PUI.Web.MDC2 (button, card, elevation20, headline4)
import QualifiedDo.Semigroupoid as Semigroupoid

counterMDC2 :: Effect Unit
counterMDC2 =
  body $
    elevation20 $
      card { caption: "Counter" } $ ( Semigroupoid.do
          headline4 (text @"count") # projection show # completed
          button @"Count" {} # updated (match { "Count": const <<< increment })
      ) # mvu freshCount
