module Demo2.Main (main) where

import Prelude hiding (div)

import Effect (Effect)
import QualifiedDo.Alt as A
import Web (a, attr, body, div, li, p, text, ul)
import Widget (static)

main :: Effect Unit
main = body $
  div >>> attr "style" "border: 1px solid silver; padding: 30px;" $ A.do
    static "Hello World!" $ p $ text
    ul $ A.do
      static "One" $ li $ text
      static "Two" $ li $ text
      static "Three" $ li $ text
    static "Link" $ a >>> attr "href" "https://www.google.com" >>> attr "target" "_blank" $ text
