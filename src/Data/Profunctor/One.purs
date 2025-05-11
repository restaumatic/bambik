module Data.Profunctor.One
  ( class One
  , pone
  )
  where

import Data.Profunctor (dimap)
import Data.Profunctor.Product (class Product)
import Data.Profunctor.Strong (class Strong, first)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)

class Product p <= One p where
  pone :: p Unit Unit -- such that `pproduct pone p ~ p ~ pproduct p pone`


-- private

id :: forall p a. Strong p => One p => p a a
id = dimap (\a -> Tuple unit a) (\(Tuple _ a) -> a) (first pone)
