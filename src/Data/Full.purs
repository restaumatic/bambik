module Data.Full
  ( class Full
  , full
  ) where

import Prelude

-- law: full <> x = full = x <> full
class Semigroup s <= Full s where
  full :: s
