module Data.Profunctor.One where

import Data.Profunctor.Product (class Product)
import Data.Unit (Unit)

class Product p <= One p where
  pone :: p Unit Unit -- such that `pproduct pone p ~ p ~ pproduct p pone`
