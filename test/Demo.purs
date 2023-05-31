module Demo where

import Prelude hiding (zero)

import Data.Invariant.Optics (projection)
import Data.Plus (plus, zero)
import Effect (Effect)
import Test.ConsoleWidget (dyntext, immutable, runConsoleWidget, static, text, textInput)

main :: Effect Unit
main = do
  let rootWidget
        = textInput "input1"
        `plus` (textInput "input2")
        `plus` (text "some static content" # static)
        `plus` (dyntext "dyntext" # projection (_ <> "!"))
        `plus` (text "reset" # static # immutable "new")
        `plus` zero
  update <- runConsoleWidget rootWidget mempty
  update "abc"
  update "abcd"
  pure unit
