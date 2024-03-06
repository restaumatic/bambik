module Data.Profunctor.Zero where

import Data.Profunctor (class Profunctor)
import Data.Profunctor.Sum (class Sum)

-- generalization of Plus
class (Profunctor p, Sum p) <= Zero p where
  pzero :: forall a b. p a b -- such that `psum pzero p == p == psum p pzero`
