module Propagator.Optics
  ( PropOptic
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

type PropOptic a b s t = forall m. Applicative m => SafePropagator m a b -> SafePropagator m s t
type PropOptic' a s = forall m. Applicative m => SafePropagator m a a -> SafePropagator m s s

iso :: forall a s. String -> (s -> a) -> (a -> s) -> PropOptic' a s
iso name mapin mapout = dimap mapin mapout >>> scopemap (Variant name)

iso' :: forall a s. (s -> Maybe a) -> (Maybe a -> s) -> PropOptic' (Maybe a) s
iso' mapin mapout = dimap mapin mapout

projection :: forall a b s. (s -> a) -> PropOptic a b s b
projection f = dimap f identity

lens :: forall a b s t. String -> (s -> a) -> (s -> b -> t) -> PropOptic a b s t
lens name getter setter = first >>> dimap (\s -> Tuple (getter s) s) (\(Tuple b s) ->setter s b) >>> scopemap (Variant name)

lens' :: forall a s. String -> (s -> a) -> (s -> a -> s) -> PropOptic' a s
lens' = lens

field :: forall @l s r a . IsSymbol l => Row.Cons l a r s => PropOptic' a (Record s)
field = field' (reflectSymbol (Proxy @l)) (flip (set (Proxy @l))) (get (Proxy @l))
  where
    field' name setter getter = scopemap (Part name) >>> first >>> dimap (\s -> Tuple (getter s) s) (\(Tuple a s) -> setter s a)

prism :: forall a b s t. String -> (b -> t) -> (s -> Either a t) -> PropOptic a b s t
prism name construct deconstruct = left >>> dimap deconstruct (either construct identity)
  >>> scopemap (Variant name) -- TODO not sure about it

prism' :: forall a s. String -> (a -> s) -> (s -> Either a s) -> PropOptic a a s s
prism' = prism

constructor :: forall a s. String -> (a -> s) -> (s -> Maybe a) -> PropOptic' a s
constructor name construct deconstruct = left >>> dimap (\s -> maybe (Right s) Left (deconstruct s)) (\aors -> either construct identity aors) >>> scopemap (Part name)
