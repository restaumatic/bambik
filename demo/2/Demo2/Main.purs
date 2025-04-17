module Demo2.Main (main) where

import Prelude hiding (div)

import Effect (Effect)
import QualifiedDo.Semigroup as S
import Web (a, body, div, li, p, staticHTML, staticText, ul, (:=))

main :: Effect Unit
main = body $
  div >>> "style" := "border: 1px solid silver; padding: 30px;" $ S.do
    p $ staticText "Hello World!"
    ul $ S.do
      li $ staticText "One"
      li $ staticText "Two"
      li $ staticText "Three"
    a >>> "href" := "https://www.google.com" >>> "target" := "_blank" >>> "title" := "Google Search" $ staticText "Link"
    staticHTML "<hr/>"
