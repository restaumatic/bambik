module CounterMDC3 (counterMDC3) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, updated)
import PUI.Web.HTML (body, shown)
import PUI.Web.MDC3 (button, card, elevation5, headlineLarge)
import QualifiedDo.Semigroupoid as Pipeline

counterMDC3 :: Effect Unit
counterMDC3 =
  body $
    elevation5 $
      card $ ( Pipeline.do
          headlineLarge (shown @"count" show)
          button @"Count" {} # updated (match { "Count": const <<< increment })
      ) # mvu freshCount
