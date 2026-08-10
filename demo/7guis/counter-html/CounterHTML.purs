module CounterHTML (counterHTML) where

import Prelude ((#), ($), (<<<), Unit, const, identity, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, forField, mvu, toCase, updated)
import PUI.Web.HTML (body, button, div, h4, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid

counterHTML :: Effect Unit
counterHTML =
  body $ div $ ( Semigroupoid.do
      h4 text # forField @"value" @"count" show # completed
      button (staticText "Count") # toCase @"clicked" identity # updated (match { clicked: const <<< increment })
  ) # mvu freshCount
