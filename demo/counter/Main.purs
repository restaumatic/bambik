module Main where

import Prelude

import Data.Lens (Lens, Iso, lens)
import Data.Lens.Extra.Commons (projection)
import Data.Profunctor.Sum as View
import Effect (Effect)
import UI (looped)
import Web (body, button, staticText, text)

main :: Effect Unit
main = body $ looped $ View.do
  counter text
  count $ button $ staticText "Count"

counter :: forall t. Iso Int t String Void
counter = projection show

count :: Lens Int Int Unit Unit 
count = lens (const unit) (\i _ -> i + 1)
