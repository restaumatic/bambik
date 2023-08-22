module Data.Profunctor.Optics
  ( Adapter
  , Constant
  , Constructor
  , Field
  , Iso
  , Lens
  , Lens'
  , Null
  , Prism
  , Projection
  , constant
  , constructor
  , field
  , iso
  , lens
  , lens'
  , module Data.Profunctor
  , null
  , projection
  )
  where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe, maybe)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Change (class ChProfunctor, Change(..), Scope(..), chmap, zoomIn, zoomOut)
import Data.Profunctor.Choice (class Choice, left)
import Data.Profunctor.Plus (class ProfunctorZero, pzero)
import Data.Profunctor.Strong (class Strong, first)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy(..))

-- identation to emphasize hierarchy
type Adapter a b s t = forall p. ChProfunctor p => p a b -> p s t
type   Iso a s = Adapter a a s s
type   Projection a s = Adapter a  Void s s
type     Constant a = forall s. Projection a s
type Lens a b s t = forall p. ChProfunctor p => Strong p => p a b -> p s t
type   Lens' a s = Lens a a s s
type     Field a s = Lens' a (Record s)
type Prism a b s t = forall p. ChProfunctor p => Choice p => p a b -> p s t
type   Constructor a s = Prism a a s s
type Null = forall p a b s t. ProfunctorZero p => p a b -> p s t

iso :: forall a s. String -> (a -> s) -> (s -> a) -> Iso a s
iso name outside inside = dimap inside outside >>> chmap (zoomIn (Variant name)) (zoomOut (Variant name))

projection :: forall a s. (s -> a) -> Projection a s
projection f = dimap f absurd

constant :: forall a. a -> Constant a
constant a = dimap (const a) absurd >>> chmap (const None) identity

lens :: forall a b s t. String -> (s -> a) -> (s -> b -> t) -> Lens a b s t
lens name getter setter = first >>> dimap (\s -> Tuple (getter s) s) (\(Tuple b s) ->setter s b) >>> chmap (zoomIn (Variant name)) (zoomOut (Variant name))

lens' :: forall a s. String -> (s -> a) -> (s -> a -> s) -> Lens' a s
lens' = lens

field :: forall @l s r a . IsSymbol l => Row.Cons l a r s => Field a s
field = field' (reflectSymbol (Proxy @l)) (flip (set (Proxy @l))) (get (Proxy @l))
  where
    field' name setter getter = first >>> dimap (\s -> Tuple (getter s) s) (\(Tuple a s) -> setter s a) >>> chmap (zoomIn (Part name)) (zoomOut (Part name))

constructor :: forall a s. String -> (a -> s) -> (s -> Maybe a) -> Constructor a s
constructor name construct deconstruct = left >>> dimap (\s -> maybe (Right s) Left (deconstruct s)) (\aors -> either (\a -> construct a) identity aors) >>> chmap (zoomIn (Part name)) (zoomOut (Part name))

null :: Null
null = const pzero

