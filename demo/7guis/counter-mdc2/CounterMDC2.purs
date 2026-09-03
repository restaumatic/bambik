module CounterMDC2 (counterMDC2) where

import Prelude ((#), ($), Unit)

import CounterLogic (countLine, freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu)
import PUI.Web.HTML (body, shown, text)
import PUI.Web.MDC2 (button, card, elevation20, headline4)
import QualifiedDo.Category as Category

counterMDC2 :: Effect Unit
counterMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          headline4 (text countLine) # shown
          button @"Count" {} # applied increment
      ) # mvu freshCount
