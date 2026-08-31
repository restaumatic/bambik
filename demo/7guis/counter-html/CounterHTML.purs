module CounterHTML (counterHTML) where

import Prelude ((#), ($), Unit, identity)

import CounterLogic (freshCount, increment, presentCounter)
import Effect (Effect)
import PUI (applied, mvu, settled, toCase)
import PUI.Web.HTML (body, button, div, h4, shown, staticText, text)
import QualifiedDo.Category as Category

counterHTML :: Effect Unit
counterHTML =
  body $ div $ ( Category.do
      h4 (text @"countText") # shown
      button (staticText "Count") # toCase @"increment" identity # applied increment
  ) # settled presentCounter # mvu freshCount
