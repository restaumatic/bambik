module Demo2.Main (main) where

import Prelude hiding (div)

import Effect (Effect)
import QualifiedDo.Alt as A
import Web (a, attr, body, div, li, p, text, ul)
import UI (constant)

main :: Effect Unit
main = body $
  div >>> attr "style" "border: 1px solid silver; padding: 30px;" $ A.do
    constant "Hello World!" $ p $ text
    ul $ A.do
      constant "One" $ li $ text
      constant "Two" $ li $ text
      constant "Three" $ li $ text
    constant "Link" $ a >>> attr "href" "https://www.google.com" >>> attr "target" "_blank" $ text
