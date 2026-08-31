module CounterBootstrap (counterBootstrap) where

import Prelude ((#), ($), Unit)

import CounterLogic (freshCount, increment, presentCounter)
import Effect (Effect)
import PUI (applied, mvu, settled)
import PUI.Web.Bootstrap (button, card)
import PUI.Web.HTML (body, h4, shown, text)
import QualifiedDo.Category as Category

counterBootstrap :: Effect Unit
counterBootstrap =
  body $
    card $ ( Category.do
        h4 (text @"countText") # shown
        button @"Count" {} # applied increment
    ) # settled presentCounter # mvu freshCount
