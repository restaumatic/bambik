module Main (main) where

import Prelude

import View (order)
import ViewModel (defaultOrder)
import Effect (Effect)
import Web (runWidgetInBody)

main :: Effect Unit
main = runWidgetInBody order defaultOrder
