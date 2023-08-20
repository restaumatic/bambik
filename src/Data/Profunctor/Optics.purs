module Data.Profunctor.Optics
  ( Adapter
  , Constant
  , Constructor
  , Field
  , Null
  , Projection
  , adapter
  , constant
  , constructor
  , field
  , module Data.Profunctor
  , null
  , projection
  )
  where

import Prelude

import Data.Either (Either(..), either)
import Data.Invariant.Transformers.Changed (Scope(..), Changed(..), zoomIn, zoomOut)
import Data.Maybe (Maybe, maybe)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Profunctor.Plus (class ProfunctorZero, pzero)
import Data.Profunctor.Strong (class Strong, first)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy(..))

type Adapter a s = forall p. Profunctor p => p (Changed a) (Changed a) -> p (Changed s) (Changed s)
type Field a s = Unit -- TODO
type Projection a s = forall p. Profunctor p => p (Changed a) (Changed Void) -> p (Changed s) (Changed s)
type Constant a = forall p s. Profunctor p => p (Changed a) (Changed Void) -> p (Changed s) (Changed s)
type Constructor a s = forall p. Choice p => p (Changed a) (Changed a) -> p (Changed s) (Changed s)
type Null = forall p a b s t. ProfunctorZero p => p a b -> p s t

null :: Null
null = const pzero

field :: forall @l i r1 r a . Strong i => IsSymbol l => Row.Cons l a r r1 => i (Changed a) (Changed a)-> i (Changed (Record r1)) (Changed (Record r1))
field = field' (reflectSymbol (Proxy @l)) (flip (set (Proxy @l))) (get (Proxy @l))
  where
    field' partName setter getter = first >>> dimap
      (\(Changed c s) -> Tuple (Changed (zoomIn (Part partName) c) (getter s)) s)
      (\(Tuple (Changed c a) s) -> Changed (zoomOut (Part partName) c) (setter s a))

constructor :: forall a s. String -> (a -> s) -> (s -> Maybe a) -> Constructor a s
constructor name construct deconstruct = left >>> dimap
  (\(Changed c s) -> maybe (Right (Changed c s)) (\a -> Left (Changed (zoomIn (Part name) c) a)) (deconstruct s))
  (\saors -> either (\(Changed c a) -> Changed (zoomOut (Part name) c) (construct a)) identity saors)

projection :: forall a s. (s -> a) -> Projection a s
projection f = dimap
  (\(Changed c s) -> Changed c (f s))
  (\(Changed c a) -> Changed c (absurd a))

constant :: forall a. a -> Constant a
constant a = dimap
  (\(Changed c s) -> Changed c a)
  (\(Changed c s) -> Changed c (absurd s))


adapter :: forall a s. String -> (a -> s) -> (s -> a) -> Adapter a s
adapter name outside inside = dimap
  (\(Changed c b) -> Changed (zoomIn (Variant name) c) (inside b))
  (\(Changed c a) -> Changed (zoomOut (Variant name) c) (outside a))
