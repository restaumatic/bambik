module CounterMDC3 (counterMDC3) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, projection, mvu, updated)
import PUI.Web.HTML (body, text)
import PUI.Web.MDC3 (button, card, elevation5, headlineLarge)
import QualifiedDo.Semigroupoid as Semigroupoid

counterMDC3 :: Effect Unit
counterMDC3 =
  body $
    elevation5 $
      card { caption: "Counter" } $ ( Semigroupoid.do
          headlineLarge (text @"count") # projection show # completed
          button @"increment" { label: "Count" } # updated (match { increment: const <<< increment })
      ) # mvu freshCount
