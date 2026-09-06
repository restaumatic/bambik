module CounterMDC3 (counterMDC3) where

import Prelude ((#), ($), Unit)

import CounterLogic (countLine, freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu)
import PUI.Web.HTML (shown, text)
import PUI.Web.MDC3 (body, button, card, headlineLarge)
import QualifiedDo.Category as Category

counterMDC3 :: Effect Unit
counterMDC3 =
  body $
    card $ ( Category.do
        headlineLarge (text countLine) # shown
        button @"Count" {} # applied increment
    ) # mvu freshCount
