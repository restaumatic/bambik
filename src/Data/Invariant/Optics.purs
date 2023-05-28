-- Polymorphic invariant transformers - invariant optics 
module Data.Invariant.Optics
  ( InvAdapter
  , InvLens
  , InvOptic
  , InvPrism
  , invLens
  , invLens'
  , propertyInvLens
  )
  where

import Control.Category (identity)
import Data.Either (Either(..), either)
import Data.Function (flip)
import Data.Invariant (class CartesianInvariant, class CoCartesianInvariant, class Invariant, invfirst, invmap, invright)
import Data.Maybe (Maybe, maybe)
import Data.Profunctor (rmap)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy)

type InvOptic :: forall k. (k -> Type) -> k -> k -> Type
type InvOptic i a b = i a -> i b

type InvAdapter a b = forall i. Invariant i => InvOptic i a b

type InvLens a b = forall i. CartesianInvariant i => InvOptic i a b

type InvPrism a b = forall i. CoCartesianInvariant i => InvOptic i a b

-- TODO: InvTraversal

propertyInvLens
  :: forall l r1 r a
   . IsSymbol l
  => Row.Cons l a r r1
  => Proxy l
  -> InvLens a (Record r1)
propertyInvLens l = invLens (get l) (flip (set l))

invLens :: forall a s. (s -> a) -> (s -> a -> s) -> InvLens a s
invLens get set = invLens' \s -> Tuple (get s) \b -> set s b

invLens' :: forall a s. (s -> Tuple a (a -> s)) -> InvLens a s
invLens' to ia = invmap (\(Tuple b f) -> f b) to (invfirst ia)

invPrism :: forall a s. (a -> s) -> (s -> Either s a) -> InvPrism a s
invPrism to fro ia = invmap fro (either identity identity) (invright (rmap to ia))

invPrism' :: forall s a. (a -> s) -> (s -> Maybe a) -> InvPrism a s
invPrism' to fro = invPrism to (\s -> maybe (Left s) Right (fro s))