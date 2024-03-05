module Data.Profunctor.Zero where

import Data.Profunctor (class Profunctor)

class Profunctor p <= Zero p where
  pzero :: forall a b. p a b -- such that `dimap f g pzero == pzero`
