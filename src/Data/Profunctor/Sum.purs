module Data.Profunctor.Sum where

import Data.Profunctor (class Profunctor)

-- generalization of `Control.Plus.Alt`
class Profunctor p <= Sum p where
  psum :: forall a b . p a b -> p a b -> p a b -- such that `psum a (psum b c) == psum (psum a b) c
