module CounterFluent (counterFluent) where

import Prelude ((#), ($), Unit)

import CounterLogic (freshCount, increment, presentCounter)
import Effect (Effect)
import PUI (applied, mvu, settled)
import PUI.Web.Fluent (button, card, title3)
import PUI.Web.HTML (body, shown, text)
import QualifiedDo.Category as Category

counterFluent :: Effect Unit
counterFluent =
  body $
    card $ ( Category.do
        title3 (text @"countText") # shown
        button @"Count" {} # applied increment
    ) # settled presentCounter # mvu freshCount
