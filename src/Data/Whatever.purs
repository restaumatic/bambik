module Data.Whatever where

import Prelude

import Data.Default (class Default)
import Data.Lens (Iso, Lens', iso, lens)
import Prim.TypeError (class Warn, Text)

data Whatever = Whatever

instance Default Whatever where
  default = Whatever

byDefault :: forall a. Warn (Text "Use real instead of fake data.") => a -> Lens' Whatever a
byDefault default = lens (\Whatever -> default) (\Whatever _ -> Whatever)

whateverFirstName :: Warn (Text "Use real instead of fake data.") => Iso Whatever Whatever String Void
whateverFirstName = iso (\Whatever -> "Joe") absurd -- TODO: make it random

whateverFirstNameEdit :: Warn (Text "Use real instead of fake data.") => Lens' Whatever String
whateverFirstNameEdit = lens (\Whatever -> "Joe") (\Whatever _ -> Whatever)

whateverPhoneNumberInput :: Warn (Text "Use real instead of fake data.") => Lens' Whatever String
whateverPhoneNumberInput = lens (\Whatever -> "") (\Whatever _ -> Whatever) -- TODO: make it random

whateverAddressInput :: Warn (Text "Use real instead of fake data.") => Lens' Whatever String
whateverAddressInput = lens (\Whatever -> "") (\Whatever _ -> Whatever) -- TODO: make it random
