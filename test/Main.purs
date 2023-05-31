module Test.Main where

import Prelude hiding (zero)

import Data.Plus (plus, zero)
import Data.Invariant.Optics (projection)
import Effect (Effect)
import Test.ConsoleWidget (ConsoleWidget(..), dyntext, immutable, static, text, textInput)

main :: Effect Unit
main = do
  let (ConsoleWidget w)
        = textInput "input1"
        `plus` (textInput "input2")
        `plus` (text "some static content" # static)
        `plus` (dyntext "dyntext" # projection (_ <> "!"))
        `plus` (text "reset" # static # immutable "new")
        `plus` zero
  update <- w mempty
  update "abc"
  update "abcd"
  pure unit
