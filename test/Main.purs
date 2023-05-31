module Test.Main where

import Prelude

import Data.Invariant (invappend)
import Data.Invariant.Optics (projection)
import Effect (Effect)
import Test.ConsoleWidget (ConsoleWidget(..), dyntext, immutable, static, text, textInput)

main :: Effect Unit
main = do
  let (ConsoleWidget w)
        = textInput "input1"
        `invappend` (textInput "input2")
        `invappend` (text "some static content" # static)
        `invappend` (dyntext "dyntext" # projection (_ <> "!"))
        `invappend` (text "reset" # static # immutable "new")
  update <- w mempty
  update "abc"
  update "abcd"
  pure unit
