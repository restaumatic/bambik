module Test.Main where

import Prelude

import Data.Invariant (invappend, invimmutable, invstatic)
import Data.Invariant.Optics (projection)
import Effect (Effect)
import Test.ConsoleWidget (ConsoleWidget(..), consoleWidget, constant)

main :: Effect Unit
main = do
  let (ConsoleWidget w)
        = consoleWidget "widget1"
        `invappend` consoleWidget "widget2"
        `invappend` invstatic (constant "const")
        `invappend` (consoleWidget "widget3" # projection (_ <> "!"))
        `invappend` (invstatic (constant "reset") # invimmutable "new")
  update <- w mempty
  update "abc"
  update "abcd"
  pure unit
