module Test.Main where

import Prelude

import Data.Invariant (invappend)
import Data.Invariant.Optics (projection)
import Effect (Effect)
import Test.ConsoleWidget (ConsoleWidget(..), consoleWidget, immutable, static, text)

main :: Effect Unit
main = do
  let (ConsoleWidget w)
        = consoleWidget "widget1"
        `invappend` (consoleWidget "widget2")
        `invappend` (text "const" # static)
        `invappend` (consoleWidget "widget3" # projection (_ <> "!"))
        `invappend` (text "reset" # static # immutable "new")
  update <- w mempty
  update "abc"
  update "abcd"
  pure unit
