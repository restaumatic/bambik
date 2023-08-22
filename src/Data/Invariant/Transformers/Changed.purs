module Data.Invariant.Transformers.Changed
  ( invAdapter
  , invConstructor
  , invField
  , invProjection
  )
  where

import Prelude

import Data.Either (Either(..), either)
import Data.Invariant (class InvCartesian, class InvCocartesian, class Invariant, invfirst, invleft, invmap)
import Data.Maybe (Maybe, maybe)
import Data.Profunctor.Change (Changed(..), Scope(..), zoomIn, zoomOut)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy(..))

invField :: forall @l i r1 r a . InvCartesian i => IsSymbol l => Row.Cons l a r r1 => i (Changed a) -> i (Changed (Record r1))
invField = invField' (reflectSymbol (Proxy @l)) (flip (set (Proxy @l))) (get (Proxy @l))
  where
    invField' :: forall i a s. InvCartesian i => String -> (s -> a -> s) -> (s -> a) -> i (Changed a) -> i (Changed s)
    invField' scope setter getter = invfirst >>> invmap
      (\(Tuple (Changed c a) s) -> Changed (zoomOut (Part scope) c) (setter s a))
      (\(Changed c s) -> Tuple (Changed (zoomIn (Part scope) c) (getter s)) s)

invConstructor :: forall i a s. InvCocartesian i => String -> (a -> s) -> (s -> Maybe a) -> i (Changed a) -> i (Changed s)
invConstructor name construct deconstruct = invleft >>> invmap
  (\saors -> either (\(Changed c a) -> Changed (zoomOut (Part name) c) (construct a)) identity saors)
  (\(Changed c s) -> maybe (Right (Changed c s)) (\a -> Left (Changed (zoomIn (Part name) c) a)) (deconstruct s))

invProjection :: forall i a s . InvCartesian i => String -> (s -> a) -> i (Changed a) -> i (Changed s)
invProjection name f = invfirst >>> invmap
  (\(Tuple (Changed c _) s) -> Changed (zoomOut (Part name) c) s)
  (\(Changed c s) -> Tuple (Changed (zoomIn (Part name) c) (f s)) s)

invAdapter :: forall i a b. Invariant i => String -> (a -> b) -> (b -> a) -> i (Changed a) -> i (Changed b)
invAdapter name f g = invmap
  (\(Changed c a) -> Changed (zoomOut (Variant name) c) (f a))
  (\(Changed c b) -> Changed (zoomIn (Variant name) c) (g b))
