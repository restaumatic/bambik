module Main (main) where

import Prelude

import Effect (Effect)
import Web (runWidgetInBody)
import View (order)

main :: Effect Unit
main = runWidgetInBody order
