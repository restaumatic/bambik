module Main (main) where

import Prelude

import Effect (Effect)
import Web (runWidgetInBody)
import View (order)
import Model (defaultOrder)

main :: Effect Unit
main = runWidgetInBody order defaultOrder
