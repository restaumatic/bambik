module CounterBootstrap (counterBootstrap) where

import Prelude ((#), ($), Unit)

import CounterLogic (countLine, freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu)
import PUI.Web.Bootstrap (button, card)
import PUI.Web.HTML (body, h4, shown, text)
import QualifiedDo.Category as Category

counterBootstrap :: Effect Unit
counterBootstrap =
  body $
    card $ ( Category.do
        h4 (text countLine) # shown
        button @"Count" {} # applied increment
    ) # mvu freshCount
