module Demo2.Main (main) where

import Prelude (Unit)

import Effect (Effect)
import Demo2.View (view)
import Web (runWidgetInBody)

main :: Effect Unit
main = runWidgetInBody view
