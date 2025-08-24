module Data.Whatever where

import Prelude

import Data.Default (class Default)
import Data.Lens (Lens', lens)
import Prim.TypeError (class Warn, Text)

data Whatever = Whatever

instance Default Whatever where
  default = Whatever

whateverFirstName :: Warn (Text "Use real instead of fake data.") => Lens' Whatever String
whateverFirstName = lens (\Whatever -> "Joe") (\Whatever _ -> Whatever) -- TODO: make it random
