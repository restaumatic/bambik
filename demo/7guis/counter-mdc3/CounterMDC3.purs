module CounterMDC3 (counterMDC3) where

import Prelude ((#), ($), Unit)

import CounterLogic (freshCount, increment, presentCounter)
import Effect (Effect)
import PUI (applied, mvu, settled)
import PUI.Web.HTML (body, shown, text)
import PUI.Web.MDC3 (button, card, elevation5, headlineLarge)
import QualifiedDo.Category as Category

counterMDC3 :: Effect Unit
counterMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          headlineLarge (text @"countText") # shown
          button @"Count" {} # applied increment
      ) # settled presentCounter # mvu freshCount
