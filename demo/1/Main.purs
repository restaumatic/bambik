module Main (main) where

import Prelude (Unit, const, ($))

import Data.Profunctor (lcmap)
import Effect (Effect)
import View (order)
import Web (runWidgetInBody)

main :: Effect Unit
main = runWidgetInBody $ lcmap (const "45123519") order
