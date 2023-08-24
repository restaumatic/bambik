module Main (main) where

import Prelude

import View
import ViewModel
import Effect (Effect)
import Web (runWidgetInBody)

main :: Effect Unit
main = void $ runWidgetInBody order defaultOrder
