module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Web (runWidgetInBody)
import View (order)
import ViewModel (defaultOrder)

main :: Effect Unit
main = runWidgetInBody order $ Just defaultOrder
