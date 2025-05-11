module Demo2.Main (main) where

import Prelude hiding (div)

import Effect (Effect)
import QualifiedDo.Semigroupoid as Flow
import Web (a, body, div, li, p, staticHTML, staticText, ul, (:=))

main :: Effect Unit
main = body $ div >>> "style" := "border: 1px solid silver; padding: 30px;" $ Flow.do
  p $ staticText "Hello World!"
  ul $ Flow.do
    li $ staticText "One"
    li $ staticText "Two"
    li $ staticText "Three"
  a >>> "href" := "https://www.google.com" >>> "target" := "_blank" >>> "title" := "Google Search" $ staticText "Link"
  staticHTML "<hr/>"
