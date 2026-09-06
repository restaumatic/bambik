module CounterMDC2 (counterMDC2) where

import Prelude ((#), ($), Unit)

import CounterLogic (countLine, freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu)
import PUI.Web.HTML (shown, text)
import PUI.Web.MDC2 (body, button, card, headline4)
import QualifiedDo.Category as Category

counterMDC2 :: Effect Unit
counterMDC2 =
  body $
    card $ ( Category.do
        headline4 (text countLine) # shown
        button @"Count" {} # applied increment
    ) # mvu freshCount
