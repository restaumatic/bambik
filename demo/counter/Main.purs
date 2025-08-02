module Main where

import Prelude

import Data.Lens (Lens, lens)
import Data.Profunctor.Sum as View
import Effect (Effect)
import UI (looped)
import Web (body, button, staticText, text)

main :: Effect Unit
main = body $ looped $ View.do
  text
  count $ button $ staticText "Count"

count :: Lens Int Int Unit Unit 
count = lens (const unit) (\i _ -> i + 1)
