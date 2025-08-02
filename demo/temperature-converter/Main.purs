module Main where

import Prelude

import Data.Lens (Iso)
import Data.Profunctor (dimap)
import Data.Profunctor.Endo as Form
import Effect (Effect)
import UI (looped)
import Web (body, staticText, text)

main :: Effect Unit
main = body $ looped $ Form.do
  text
  staticText "C = "
  fahrenheit text
  staticText "F"

fahrenheit :: Iso Number Number Number Number 
fahrenheit = dimap (\c -> c * (9.0/5.0) + 32.0) (\f -> (f - 32.0) * (5.0/9.0))
