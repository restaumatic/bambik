module Main (main) where

import Prelude

import Effect (Effect)
import Web (body, staticText)

main :: Effect Unit
main = body @Unit $ staticText "Hello, World!"
