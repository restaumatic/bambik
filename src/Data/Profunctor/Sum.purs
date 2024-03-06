module Data.Profunctor.Sum where

import Data.Profunctor (class Profunctor)

-- generalization of Alt
class Profunctor p <= Sum p where
  psum :: forall a b . p a b -> p a b -> p a b
