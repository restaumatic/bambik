module Main (main) where

import Effect (Effect)
import Model (defaultOrder)
import Prelude (Unit)
import View (order)
import Web (runWidgetInBody)

main :: Effect Unit
main = runWidgetInBody order defaultOrder
