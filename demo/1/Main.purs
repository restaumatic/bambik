module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import SafeWeb (runWidgetInBody)
import View (order)
import ViewModel (defaultOrder)

main :: Effect Unit
main = runWidgetInBody order $ Just defaultOrder
