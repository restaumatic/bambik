module CounterHTML (counterHTML) where

import Prelude ((#), ($), (<<<), Unit, const, identity, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, toCase, updated, projection)
import PUI.Web.HTML (body, button, div, h4, shown, staticText, text)
import QualifiedDo.Semigroupoid as Pipeline

counterHTML :: Effect Unit
counterHTML =
  body $ div $ ( Pipeline.do
      h4 (text @"count" # projection show) # shown
      button (staticText "Count") # toCase @"increment" identity # updated (match { increment: const <<< increment })
  ) # mvu freshCount
