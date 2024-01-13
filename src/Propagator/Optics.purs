module Propagator.Optics
  ( Adapter
  , Constant
  , Constructor
  , Field
  , Iso
  , Lens
  , Prism
  , Projection
  , PropOptic
  , PropOptic'
  , constructor
  , field
  , iso
  , iso'
  , lens
  , lens'
  , prism
  , prism'
  , projection
  )
  where

import Prelude

import Data.Profunctor (dimap)
import Data.Profunctor.Choice (left)
import Data.Profunctor.Strong (first)

import Data.Either (Either(..), either)
import Data.Maybe (Maybe, maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import SafePropagator (SafePropagator, scopemap)
import Propagator (Scope(..))
import Record (get, set)
import Type.Proxy (Proxy(..))

-- indentation to emphasize hierarchy
type PropOptic a b s t = forall m. Applicative m => SafePropagator m a b -> SafePropagator m s t
type PropOptic' a b = forall m. Applicative m => SafePropagator m a a -> SafePropagator m b b
type   Adapter a b s t = PropOptic a b s t
type     Iso a s = PropOptic' a s
type     Projection a s = forall b. Adapter a b s b
type       Constant a = forall s. Projection a s
type   Lens a s = PropOptic' a s
type     Field a s = PropOptic' a (Record s)
type   Prism a b s t = PropOptic a b s t
type     Constructor a s = PropOptic' a s -- TODO find better signature

iso :: forall a s. String -> (s -> a) -> (a -> s) -> Iso a s
iso name mapin mapout = dimap mapin mapout >>> scopemap (Variant name)

iso' :: forall a s. (s -> Maybe a) -> (Maybe a -> s) -> Iso (Maybe a) s
iso' mapin mapout = dimap mapin mapout

projection :: forall a s. (s -> a) -> Projection a s
projection f = dimap f identity

lens :: forall a b s t. String -> (s -> a) -> (s -> b -> t) -> PropOptic a b s t
lens name getter setter = first >>> dimap (\s -> Tuple (getter s) s) (\(Tuple b s) ->setter s b) >>> scopemap (Variant name)

lens' :: forall a s. String -> (s -> a) -> (s -> a -> s) -> Lens a s
lens' = lens

field :: forall @l s r a . IsSymbol l => Row.Cons l a r s => Field a s
field = field' (reflectSymbol (Proxy @l)) (flip (set (Proxy @l))) (get (Proxy @l))
  where
    field' name setter getter = scopemap (Part name) >>> first >>> dimap (\s -> Tuple (getter s) s) (\(Tuple a s) -> setter s a)

prism :: forall a b s t. String -> (b -> t) -> (s -> Either a t) -> Prism a b s t
prism name construct deconstruct = left >>> dimap deconstruct (either construct identity)
  >>> scopemap (Variant name) -- TODO not sure about it

prism' :: forall a s. String -> (a -> s) -> (s -> Either a s) -> PropOptic a a s s
prism' = prism

constructor :: forall a s. String -> (a -> s) -> (s -> Maybe a) -> Constructor a s
constructor name construct deconstruct = left >>> dimap (\s -> maybe (Right s) Left (deconstruct s)) (\aors -> either construct identity aors) >>> scopemap (Part name)
