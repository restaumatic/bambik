module Demo2.View
  ( view
  ) where

import Prelude hiding (div)

import QualifiedDo.Semigroup as S
import Web (Web, a, attr, div, li, p, text, ul)
import Widget (Widget, constant)

view :: forall a. Widget Web a a
view =
  div >>> attr "style" "border: 1px solid silver; padding: 30px;" $ S.do
    p $ text # constant "Hello World!"
    ul $ S.do
      li $ text # constant "One"
      li $ text # constant "Two"
      li $ text # constant "Three"
    a >>> attr "href" "https://www.google.com" >>> attr "target" "_blank" $ text # constant "Link"

