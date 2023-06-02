module Demo where

import Data.Identity (Identity)
import Data.Plus (plus)
import Effect (Effect)
import Effect.Class (liftEffect)
import Prelude (Unit, bind, mempty, ($))
import Specular.Dom.Builder (runMainBuilderInBody)
import Web (ComponentWrapper, renderComponent, text)
import Web.MDC as MDC

app ∷ ComponentWrapper Identity String
app = MDC.filledText "Caption"
      `plus`
      text

main :: Effect Unit
main = runMainBuilderInBody $ do
  update <- renderComponent app mempty
  liftEffect $ update "aaa"
