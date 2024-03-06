module Data.Profunctor.Zero where

import Data.Profunctor.Sum (class Sum)

-- generalization of `Control.Plus.Plus`
-- Having `pzero` is equivalent to having `p Unit Void`
class Sum p <= Zero p where
  pzero :: forall a b. p a b -- such that `psum pzero p == p == psum p pzero`
