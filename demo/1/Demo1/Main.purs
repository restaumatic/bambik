module Demo1.Main (main) where

import Data.Profunctor (lcmap)
import Demo1.View (order)
import Effect (Effect)
import Prelude (Unit, const, ($))
import Web (runWidgetInSelectedNode)

main :: Effect Unit
main = runWidgetInSelectedNode "#main" $ lcmap (const "45123519") order
