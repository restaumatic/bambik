module Main (main) where

import Prelude

import Business
import UI
import Effect (Effect)
import Web (runWidgetInBody)

main :: Effect Unit
main = void $ runWidgetInBody order defaultOrder
