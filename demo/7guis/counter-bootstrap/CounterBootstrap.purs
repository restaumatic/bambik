module CounterBootstrap (counterBootstrap) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import CounterLogic (freshCount, increment)
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, projection, mvu, updated)
import PUI.Web.Bootstrap (button, card)
import PUI.Web.HTML (body, h4, text)
import QualifiedDo.Semigroupoid as Semigroupoid

counterBootstrap :: Effect Unit
counterBootstrap =
  body $
    card $ ( Semigroupoid.do
        h4 (text @"count") # projection show # completed
        button @"Count" {} # updated (match { "Count": const <<< increment })
    ) # mvu freshCount
