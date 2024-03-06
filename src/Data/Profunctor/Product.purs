module Data.Profunctor.Product where

import Data.Profunctor (class Profunctor)
import Data.Tuple (Tuple)

class Profunctor p <= Product p where
  pproduct :: forall a b a' b'. p a b -> p a' b' -> p (Tuple a a') (Tuple b b')

