module Data.Profunctor.Optics
  ( Adapter
  , ChOptic
  , Constant
  , Constructor
  , Field
  , Iso
  , Lens
  , Lens'
  , Null
  , Prism
  , Prism'
  , Projection
  , constructor
  , field
  , iso
  , iso'
  , lens
  , lens'
  , module Data.Profunctor
  , module Data.Profunctor.Change
  , module Data.Profunctor.Choice
  , module Data.Profunctor.Plus
  , module Data.Profunctor.Strong
  , null
  , prism
  , prism'
  , projection
  )
  where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe, maybe)
import Data.Profunctor
import Data.Profunctor.Change
import Data.Profunctor.Choice
import Data.Profunctor.Plus
import Data.Profunctor.Strong
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy(..))

-- identation to emphasize hierarchy
type ChOptic a b s t = forall p. ChProfunctor p => p a b -> p s t
type   Adapter a b s t = ChOptic a b s t -- TODO rename to ChOptic?
type     Iso a s = Adapter a a s s
type     Projection a s = forall b. Adapter a b s b
type       Constant a = forall s. Projection a s
type   Lens a b s t = forall p. ChProfunctor p => Strong p => p a b -> p s t
type     Lens' a s = Lens a a s s
type       Field a s = Lens' a (Record s)
type   Prism a b s t = forall p. ChProfunctor p => Choice p => p a b -> p s t
type     Prism' a s = Prism a a s s
type       Constructor a s = Prism' a s -- TODO find better signature
type Null = forall p a b s. ProfunctorZero p => p a b -> p s s

iso :: forall a s. String -> (s -> a) -> (a -> s) -> Iso a s
iso name mapin mapout = dimap mapin mapout >>> scopemap (Variant name)

iso' :: forall a s. (s -> Maybe a) -> (Maybe a -> s) -> Iso (Maybe a) s
iso' mapin mapout = dimap mapin mapout


projection :: forall a s. (s -> a) -> Projection a s
projection f = dimap f identity

-- depracated
-- TODO EC: (const None) is not right
constant :: forall a b s. a -> ChOptic a b s b -- Constant a
constant a = dimap (const a) identity >>> chmap (const None) identity

lens :: forall a b s t. String -> (s -> a) -> (s -> b -> t) -> Lens a b s t
lens name getter setter = first >>> dimap (\s -> Tuple (getter s) s) (\(Tuple b s) ->setter s b) >>> scopemap (Variant name)

lens' :: forall a s. String -> (s -> a) -> (s -> a -> s) -> Lens' a s
lens' = lens

field :: forall @l s r a . IsSymbol l => Row.Cons l a r s => Field a s
field = field' (reflectSymbol (Proxy @l)) (flip (set (Proxy @l))) (get (Proxy @l))
  where
    field' name setter getter = first >>> dimap (\s -> Tuple (getter s) s) (\(Tuple a s) -> setter s a) >>> scopemap (Part name)

prism :: forall a b s t. String -> (b -> t) -> (s -> Either a t) -> Prism a b s t
prism name construct deconstruct = left >>> dimap deconstruct (either construct identity)
  >>> scopemap (Variant name) -- TODO not sure about it

prism' :: forall a s. String -> (a -> s) -> (s -> Either a s) -> Prism' a s
prism' = prism

constructor :: forall a s. String -> (a -> s) -> (s -> Maybe a) -> Constructor a s
constructor name construct deconstruct = left >>> dimap (\s -> maybe (Right s) Left (deconstruct s)) (\aors -> either (\a -> construct a) identity aors) >>> scopemap (Part name)

null :: Null
null = const prozero

