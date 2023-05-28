module Test.Main where

import Prelude

import Data.Invariant (invappend)
import Effect (Effect)
import Test.ConsoleWidget (ConsoleWidget(..), consoleWidget)


main :: Effect Unit
main = do
  let (ConsoleWidget w) = consoleWidget "widget1" `invappend` consoleWidget "widget2"
  _ <- w "value" mempty
  pure unit
