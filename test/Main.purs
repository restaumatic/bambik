module Test.Main where

import Prelude

import Data.Invariant (invappend, invstatic)
import Effect (Effect)
import Test.ConsoleWidget (ConsoleWidget(..), consoleWidget, constant)

main :: Effect Unit
main = do
  let (ConsoleWidget w)
        = consoleWidget "widget1"
        `invappend` consoleWidget "widget2"
        `invappend` invstatic (constant "const")
  update <- w mempty
  update "abc"
  pure unit
