module Data.Zero
  ( class Zero
  , zero
  ) where

import Prelude

-- laws: zero <> x = zero = x <> zero
class Semigroup s <= Zero s where
  zero :: s

