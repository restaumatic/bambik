module Demo where

import Prelude

import Data.Plus (plus)
import Effect (Effect)
import Specular.Dom.Builder (runMainBuilderInBody)
import Web (Component, inside, buildComponent, text)
import Web.MDC (filledText)

app ∷ Component String
app = (filledText "name" # inside "div" mempty mempty)
      `plus`
      (text # inside "div" mempty mempty)
      `plus` (text # inside "div" mempty mempty)

main :: Effect Unit
main = do
  update <- runMainBuilderInBody $ buildComponent app mempty
  update "ccc!"
  pure unit
