-- Polymorphic invariant transformers - invariant optics 
module Data.Invariant.Optics
  ( InvAdapter
  , InvLens
  , InvOptic
  , InvPrism
  , constructorInvPrism
  , invLens
  , invPrism
  , propertyInvLens
  )
  where

import Prelude

import Control.Category (identity)
import Data.Either (Either(..), either)
import Data.Function (flip)
import Data.Invariant (class CartesianInvariant, class CoCartesianInvariant, class Invariant, invfirst, invleft, invmap)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
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

invLens :: forall a s. (s -> a) -> (s -> a -> s) -> InvLens a s
invLens get set ia = invmap (\(Tuple a s) -> set s a) (\s -> Tuple (get s) s) (invfirst ia)

propertyInvLens
  :: forall l r1 r a
   . IsSymbol l
  => Row.Cons l a r r1
  => Proxy l
  -> InvLens a (Record r1)
propertyInvLens l = invLens (\s -> get l s) (\s a -> (set l) a s)

propertyInvLens'
  :: forall l r1 r a s
   . IsSymbol l
  => Row.Cons l a r r1
  => Newtype s (Record r1)
  => Proxy s
  -> Proxy l
  -> InvLens a s
propertyInvLens' _ l = invLens (\s -> get l (unwrap s)) (\s a -> wrap $ (set l) a (unwrap s))

invPrism :: forall a s. (a -> s) -> (s -> Either a s) -> InvPrism a s
invPrism review preview ia = invmap (\aors -> either review identity aors) preview (invleft ia)

constructorInvPrism :: forall a s. (a -> s) -> (s -> Maybe a) -> InvPrism a s
constructorInvPrism construct deconstruct ia = invmap (\(aors :: Either a s) -> either construct identity aors) (\s -> maybe (Right s) Left (deconstruct s)) (invleft ia)
