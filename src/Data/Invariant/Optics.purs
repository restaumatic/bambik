-- Polymorphic invariant transformers - invariant optics 
module Data.Invariant.Optics
  ( InvAdapter
  , InvLens
  , InvOptic
  , InvPrism
  )
  where

import Data.Invariant (class CartesianInvariant, class CoCartesianInvariant, class Invariant)

type InvOptic :: forall k. (k -> Type) -> k -> k -> Type
type InvOptic i a b = i a -> i b

type InvAdapter a b = forall i. Invariant i => InvOptic i a b

type InvLens a b = forall i. CartesianInvariant i => InvOptic i a b

type InvPrism a b = forall i. CoCartesianInvariant i => InvOptic i a b

-- TODO: InvTraversal