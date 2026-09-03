module CounterShoelace (counterShoelace) where

import Prelude ((#), ($), Unit)

import CounterLogic (countLine, freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu)
import PUI.Web.HTML (body, h4, shown, text)
import PUI.Web.Shoelace (button, card)
import QualifiedDo.Category as Category

counterShoelace :: Effect Unit
counterShoelace =
  body $
    card $ ( Category.do
        h4 (text countLine) # shown
        button @"Count" {} # applied increment
    ) # mvu freshCount
