module CounterHTML (counterHTML) where

import Prelude ((#), ($), Unit, identity, show)

import CounterLogic (freshCount, increment)
import Effect (Effect)
import PUI (applied, mvu, toCase, projection)
import PUI.Web.HTML (body, button, div, h4, shown, staticText, text)
import QualifiedDo.Semigroupoid as Pipeline

counterHTML :: Effect Unit
counterHTML =
  body $ div $ ( Pipeline.do
      h4 (text @"count" # projection show) # shown
      button (staticText "Count") # toCase @"increment" identity # applied increment
  ) # mvu freshCount
