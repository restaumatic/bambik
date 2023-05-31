-- There exists Data.Functor.Invariant module in purescript-invariant (https://pursuit.purescript.org/packages/purescript-invariant/6.0.0) already,
-- yet here we tweak class hierarchy of invariant, covariant and contravariant functors according to observation made in [1]: 
-- "you could argue that in an ideal world the definition for Functor should change to class ExpFunctor f => Functor f".
--
-- References
-- 1. Edward Kmett: Rotten Bananas, http://comonad.com/reader/2008/rotten-bananas/  

module Data.Invariant
  ( class CartesianInvariant
  , class CoCartesianInvariant
  , class Contravariant
  , class Covariant
  , class EffInvariant
  , class Invariant
  , conmap
  , covmap
  , invand
  , invandwith
  , inveff
  , invfirst
  , invleft
  , invmap
  , invor
  , invorwith
  , invright
  , invsecond
  )
  where


import Prelude

import Data.Either (Either)
import Data.Plus (class Plus, plus)
import Data.Tuple (Tuple)
import Effect (Effect)

-- Functor class hierarchy

class Invariant f where
    invmap :: forall a b . (a -> b) -> (b -> a) -> f a -> f b
    -- aka exponential functor

class Invariant f <= Covariant f where
    covmap :: forall a b . (a -> b) -> f a -> f b
    -- law: invmap = const <<< covmap

class Invariant f <= Contravariant f where
    conmap :: forall a b . (b -> a) -> f a -> f b
    -- law: invmap = const conmap

class Invariant f <= CartesianInvariant f where
    invfirst :: forall a b. f a -> f (Tuple a b)
    invsecond :: forall a b. f b -> f (Tuple a b)

class Invariant f <= CoCartesianInvariant f where
    invleft :: forall a b. f a -> f (Either a b)
    invright :: forall a b. f b -> f (Either a b)

-- TODO: MonoidalInvariant

invand :: forall i a b . CartesianInvariant i => Plus i => i a -> i b -> i (Tuple a b)
invand a b = invfirst a `plus` invsecond b

invandwith :: forall i a b c . CartesianInvariant i => Plus i => (Tuple a b -> c) -> (c -> Tuple a b) -> i a -> i b -> i c
invandwith f g a b = invmap f g $ invand a b

invor :: forall i a b . CoCartesianInvariant i => Plus i => i a -> i b -> i (Either a b)
invor a b = invleft a `plus` invright b

invorwith :: forall i a b c . CoCartesianInvariant i => Plus i => (Either a b -> c) -> (c -> Either a b) -> i a -> i b -> i c
invorwith f g a b = invmap f g $ invor a b

class EffInvariant i where
    inveff :: forall a . (a -> Effect Unit) -> i a -> i a
