module CounterMDC2 (counterMDC2) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, updated)
import PUI.Web.HTML (body, shown)
import PUI.Web.MDC2 (button, card, elevation20, headline4)
import QualifiedDo.Semigroupoid as Semigroupoid

counterMDC2 :: Effect Unit
counterMDC2 =
  body $
    elevation20 $
      card $ ( Semigroupoid.do
          headline4 (shown @"count" show)
          button @"Count" {} # updated (match { "Count": const <<< increment })
      ) # mvu freshCount
