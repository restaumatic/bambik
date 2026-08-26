module CounterMDC3 (counterMDC3) where

import Prelude ((#), ($), Unit, show)

import CounterLogic (freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu, projection)
import PUI.Web.HTML (body, shown, text)
import PUI.Web.MDC3 (button, card, elevation5, headlineLarge)
import QualifiedDo.Category as Category

counterMDC3 :: Effect Unit
counterMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          headlineLarge (text @"count" # projection show) # shown
          button @"Count" {} # applied increment
      ) # mvu freshCount
