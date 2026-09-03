module CounterMDC3 (counterMDC3) where

import Prelude ((#), ($), Unit)

import CounterLogic (countLine, freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu)
import PUI.Web.HTML (body, shown, text)
import PUI.Web.MDC3 (button, card, elevation5, headlineLarge)
import QualifiedDo.Category as Category

counterMDC3 :: Effect Unit
counterMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          headlineLarge (text countLine) # shown
          button @"Count" {} # applied increment
      ) # mvu freshCount
