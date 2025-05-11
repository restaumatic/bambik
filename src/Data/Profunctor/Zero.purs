module Data.Profunctor.Zero
  ( class Zero
  , pzero
  )
  where

import Control.Category (identity)
import Data.Either (Either(..), either)
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Profunctor.Sum (class Sum)
import Data.Void (absurd)

-- generalization of `Control.Plus.Plus`
-- Having `pzero` is equivalent to having `p Unit Void`
class Sum p <= Zero p where
  -- takes whatever, does noting
  pzero :: forall a b. p a b -- such that `psum pzero p == p == psum p pzero`

-- private

id :: forall p a. Choice p => Zero p => p a a
id = dimap Right (either absurd identity) (left pzero)
