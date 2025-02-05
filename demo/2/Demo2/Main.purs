module Demo2.Main (main) where

import Prelude hiding (div)

import Effect (Effect)
import QualifiedDo.Alt as A
import Web (a, attr, body, div, li, p, staticText, ul)

main :: Effect Unit
main = body $
  div >>> attr "style" "border: 1px solid silver; padding: 30px;" $ A.do
    p $ staticText "Hello World!"
    ul $ A.do
      li $ staticText "One"
      li $ staticText "Two"
      li $ staticText "Three"
    a >>> attr "href" "https://www.google.com" >>> attr "target" "_blank" $ staticText "Link"
