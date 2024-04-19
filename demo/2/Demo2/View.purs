module Demo2.View
  ( view
  ) where

import Prelude hiding (div)

-- import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T
import Web (Web, a, attr, div, li, p, text, ul)
import Widget (Widget, constant)

view :: forall a b. Widget Web a b
view =
  div >>> attr "style" "border: 1px solid silver; padding: 30px;" $ T.do
    p $ text # constant "Hello World!"
    ul $ T.do
      li $ text # constant "One"
      li $ text # constant "Two"
      li $ text # constant "Three"
    a >>> attr "href" "https://www.google.com" >>> attr "target" "_blank" $ text # constant "Link"

