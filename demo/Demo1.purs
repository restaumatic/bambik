module Demo1 (main) where

import Prelude

import Demo1Business
import Demo1WebView
import Effect (Effect)
import Web (runMainComponent)

main :: Effect Unit
main = runMainComponent order defaultOrder
