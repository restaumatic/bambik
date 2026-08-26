module CounterMDC2 (counterMDC2) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, updated, projection)
import PUI.Web.HTML (body, shown, text)
import PUI.Web.MDC2 (button, card, elevation20, headline4)
import QualifiedDo.Semigroupoid as Pipeline

counterMDC2 :: Effect Unit
counterMDC2 =
  body $
    elevation20 $
      card $ ( Pipeline.do
          headline4 (text @"count" # projection show) # shown
          button @"Count" {} # updated (match { "Count": const <<< increment })
      ) # mvu freshCount
