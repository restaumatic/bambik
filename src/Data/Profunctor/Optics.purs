module Data.Profunctor.Optics
  ( Constructor
  , Iso
  , Projection
  , class ProCartesian
  , class ProClosed
  , class ProCocartesian
  , closed
  , constructor
  , field
  , iso
  , module Data.Profunctor
  , nothing
  , profirst
  , projection
  , proleft
  , promap
  , proright
  , prosecond
  )
  where

import Prelude

import Data.Either (Either(..), either)
import Data.Invariant.Transformers.Scoped (PartName(..), Scoped(..), zoomIn, zoomOut)
import Data.Maybe (Maybe, maybe)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Plus (class ProPlus, prozero)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy(..))

-- Functor class hierarchy

promap :: forall f i o a b . Profunctor f => (a -> i) -> (o -> b) -> f i o -> f a b
promap = dimap

class Profunctor f <= ProCartesian f where
    profirst :: forall i o a. f i o -> f (Tuple i a) (Tuple o a)
    prosecond :: forall i o a. f i o -> f (Tuple a i) (Tuple a o)

class Profunctor f <= ProCocartesian f where
    proleft :: forall i o a. f i o -> f (Either i a) (Either o a)
    proright :: forall i o a. f i o -> f (Either a i) (Either a o)

class Profunctor f <= ProClosed f where
    closed :: forall i o a . f i o -> f (a -> i) (a -> o)

type Field a s = Unit -- TODO
type Constructor a s = forall p. ProCocartesian p => p (Scoped a) (Scoped a) -> p (Scoped s) (Scoped s)
type Projection a s = forall p. ProCartesian p => p (Scoped a) (Scoped a) -> p (Scoped s) (Scoped s)
type Iso a s = forall p. Profunctor p => p (Scoped a) (Scoped a) -> p (Scoped s) (Scoped s)

nothing :: forall p a b s t. Profunctor p => ProPlus p => p a b -> p s t
nothing = const prozero

field :: forall @l i r1 r a . ProCartesian i => IsSymbol l => Row.Cons l a r r1 => i (Scoped a) (Scoped a)-> i (Scoped (Record r1)) (Scoped (Record r1))
field = field' (reflectSymbol (Proxy @l)) (flip (set (Proxy @l))) (get (Proxy @l))
  where
    field' partName setter getter = profirst >>> promap
      (\(Scoped c s) -> Tuple (Scoped (zoomIn (PartName partName) c) (getter s)) s)
      (\(Tuple (Scoped c a) s) -> Scoped (zoomOut (PartName partName) c) (setter s a))

constructor :: forall a s. String -> (a -> s) -> (s -> Maybe a) -> Constructor a s
constructor name construct deconstruct = proleft >>> promap
  (\(Scoped c s) -> maybe (Right (Scoped c s)) (\a -> Left (Scoped (zoomIn (PartName name) c) a)) (deconstruct s))
  (\saors -> either (\(Scoped c a) -> Scoped (zoomOut (PartName name) c) (construct a)) identity saors)

projection :: forall a s . String -> (s -> a) -> Projection a s
projection name f = profirst >>> promap
  (\(Scoped c s) -> Tuple (Scoped (zoomIn (PartName name) c) (f s)) s)
  (\(Tuple (Scoped c _) s) -> Scoped (zoomOut (PartName name) c) s)

iso :: forall a s. String -> (a -> s) -> (s -> a) -> Iso a s
iso name outside inside = promap
  (\(Scoped c b) -> Scoped (zoomIn (TwistName name) c) (inside b))
  (\(Scoped c a) -> Scoped (zoomOut (TwistName name) c) (outside a))

-- adapter :: forall a b s t. String -> (b -> t) -> (s -> a) -> Adapter a b s t
-- adapter name outside inside = promap
--   (\(Scoped c b) -> Scoped (zoomIn (TwistName name) c) (inside b))
--   (\(Scoped c a) -> Scoped (zoomOut (TwistName name) c) (outside a))

-- common projections

-- invand a b = profirst a ^ prosecond b

-- invandwith :: forall i a b c . ProCartesian i => InvPlus i => (Tuple a b -> c) -> (c -> Tuple a b) -> i a -> i b -> i c
-- invandwith f g a b = promap f g $ invand a b

-- invor :: forall i a b . ProCocartesian i => InvPlus i => i a -> i b -> i (Either a b)
-- invor a b = proleft a ^ proright b

-- invorwith :: forall i a b c . ProCocartesian i => InvPlus i => (Either a b -> c) -> (c -> Either a b) -> i a -> i b -> i c
-- invorwith f g a b = promap f g $ invor a b
