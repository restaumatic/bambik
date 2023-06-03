module Demo where

import Prelude

import Data.Identity (Identity)
import Data.Plus (plus)
import Effect (Effect)
import Effect.Class (liftEffect)
import Specular.Dom.Builder (runMainBuilderInBody)
import Web (ComponentWrapper, inside, renderComponent, text)
import Web.MDC (filledText)

app ∷ ComponentWrapper Identity String
app = (filledText "name" # inside "div" mempty mempty)
      `plus`
      (text # inside "div" mempty mempty)
      `plus` (text # inside "div" mempty mempty)

main :: Effect Unit
main = runMainBuilderInBody $ do
  update <- renderComponent app mempty
  _ <- liftEffect $ update "aaa!"
  pure unit
