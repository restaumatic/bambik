module Main (main) where

import Prelude hiding (div)

import Effect (Effect)
import Data.Profunctor.Sum as View
import Web (a, body, div, li, p, staticHTML, staticText, ul, (:=))

main :: Effect Unit
main = body @Unit $ div View.do
  p $ staticText "Hello World!"
  ul View.do
    li $ staticText "One"
    li $ staticText "Two"
    li $ staticText "Three"
  a >>> "href" := "https://www.google.com" $ staticText "Search for me!"
  staticHTML "<hr/>"
