module Main (main) where

import Prelude

import Data.Profunctor (lcmap)
import Effect (Effect)
import View (order)
import Web (runWidgetInBody)

main :: Effect Unit
main = runWidgetInBody $ lcmap (const "451") order
