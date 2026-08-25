module CounterHTML (counterHTML) where

import Prelude ((#), ($), (<<<), Unit, const, identity, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, toCase, updated)
import PUI.Web.HTML (body, button, div, h4, shown, staticText)
import QualifiedDo.Semigroupoid as Semigroupoid

counterHTML :: Effect Unit
counterHTML =
  body $ div $ ( Semigroupoid.do
      h4 (shown @"count" show)
      button (staticText "Count") # toCase @"increment" identity # updated (match { increment: const <<< increment })
  ) # mvu freshCount
