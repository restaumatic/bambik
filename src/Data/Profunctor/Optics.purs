module Data.Profunctor.Optics
  ( class ProCartesian
  , class ProClosed
  , class ProCocartesian
  , closed
  , nothing
  , profirst
  , proleft
  , promap
  , proright
  , prosecond
  )
  where

import Prelude

import Data.Either (Either)
import Data.Plus (class ProPlus, prozero)
import Data.Profunctor (class Profunctor, dimap)
import Data.Tuple (Tuple)

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

nothing :: forall p a b s t. Profunctor p => ProPlus p => p a b -> p s t
nothing = const prozero

-- TODO are these below needed?
-- invand a b = profirst a ^ prosecond b

-- invandwith :: forall i a b c . ProCartesian i => InvPlus i => (Tuple a b -> c) -> (c -> Tuple a b) -> i a -> i b -> i c
-- invandwith f g a b = promap f g $ invand a b

-- invor :: forall i a b . ProCocartesian i => InvPlus i => i a -> i b -> i (Either a b)
-- invor a b = proleft a ^ proright b

-- invorwith :: forall i a b c . ProCocartesian i => InvPlus i => (Either a b -> c) -> (c -> Either a b) -> i a -> i b -> i c
-- invorwith f g a b = promap f g $ invor a b
