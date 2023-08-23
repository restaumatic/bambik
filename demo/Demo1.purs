module Demo1 (main) where

import Prelude

import Demo1Business
import Demo1WebView
import Effect (Effect)
import Web (runWidgetInBody)

main :: Effect Unit
main = runWidgetInBody order defaultOrder
