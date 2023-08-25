module Main (main) where

import Prelude

import View (order)
import ViewModel (defaultOrder)
import Effect (Effect)
import Web (runMainWidget)

main :: Effect Unit
main = runMainWidget order defaultOrder
