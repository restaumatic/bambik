module Data.Profunctor.Optics
  ( Adapter
  , Constructor
  , Field
  , Null
  , Projection
  , adapter
  , constructor
  , field
  , module Data.Profunctor
  , null
  , projection
  )
  where

import Prelude

import Data.Either (Either(..), either)
import Data.Invariant.Transformers.Scoped (PartName(..), Scoped(..), zoomIn, zoomOut)
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

type Adapter a s = forall p. Profunctor p => p (Scoped a) (Scoped a) -> p (Scoped s) (Scoped s)
type Field a s = Unit -- TODO
type Projection a s = forall p. Strong p => p (Scoped a) (Scoped a) -> p (Scoped s) (Scoped s)
type Constructor a s = forall p. Choice p => p (Scoped a) (Scoped a) -> p (Scoped s) (Scoped s)
type Null = forall p a b s t. ProfunctorZero p => p a b -> p s t

null :: Null
null = const pzero

field :: forall @l i r1 r a . Strong i => IsSymbol l => Row.Cons l a r r1 => i (Scoped a) (Scoped a)-> i (Scoped (Record r1)) (Scoped (Record r1))
field = field' (reflectSymbol (Proxy @l)) (flip (set (Proxy @l))) (get (Proxy @l))
  where
    field' partName setter getter = first >>> dimap
      (\(Scoped c s) -> Tuple (Scoped (zoomIn (PartName partName) c) (getter s)) s)
      (\(Tuple (Scoped c a) s) -> Scoped (zoomOut (PartName partName) c) (setter s a))

constructor :: forall a s. String -> (a -> s) -> (s -> Maybe a) -> Constructor a s
constructor name construct deconstruct = left >>> dimap
  (\(Scoped c s) -> maybe (Right (Scoped c s)) (\a -> Left (Scoped (zoomIn (PartName name) c) a)) (deconstruct s))
  (\saors -> either (\(Scoped c a) -> Scoped (zoomOut (PartName name) c) (construct a)) identity saors)

projection :: forall a s . String -> (s -> a) -> Projection a s
projection name f = first >>> dimap
  (\(Scoped c s) -> Tuple (Scoped (zoomIn (PartName name) c) (f s)) s)
  (\(Tuple (Scoped c _) s) -> Scoped (zoomOut (PartName name) c) s)

adapter :: forall a s. String -> (a -> s) -> (s -> a) -> Adapter a s
adapter name outside inside = dimap
  (\(Scoped c b) -> Scoped (zoomIn (TwistName name) c) (inside b))
  (\(Scoped c a) -> Scoped (zoomOut (TwistName name) c) (outside a))
