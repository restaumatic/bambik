module CounterShoelace (counterShoelace) where

import Prelude ((#), ($), Unit)

import CounterLogic (freshCount, increment, presentCounter)
import Effect (Effect)
import PUI (applied, mvu, settled)
import PUI.Web.HTML (body, h4, shown, text)
import PUI.Web.Shoelace (button, card)
import QualifiedDo.Category as Category

counterShoelace :: Effect Unit
counterShoelace =
  body $
    card $ ( Category.do
        h4 (text @"countText") # shown
        button @"Count" {} # applied increment
    ) # settled presentCounter # mvu freshCount
