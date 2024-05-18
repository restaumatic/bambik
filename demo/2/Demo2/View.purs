module Demo2.View
  ( view
  ) where

import Prelude hiding (div)

import QualifiedDo.Semigroupoid as T
import Web (Web, a, attr, div, li, p, text, ul)
import Widget (Widget, static)

view :: forall a. Widget Web a a
view =
  div >>> attr "style" "border: 1px solid silver; padding: 30px;" $ T.do
    p $ text # static "Hello World!"
    ul $ T.do
      li $ text # static "One"
      li $ text # static "Two"
      li $ text # static "Three"
    a >>> attr "href" "https://www.google.com" >>> attr "target" "_blank" $ text # static "Link"
