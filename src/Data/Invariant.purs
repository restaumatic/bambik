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
  , class FooInvariant
  , class Invariant
  , conmap
  , covmap
  , invappend
  , invempty
  , invfirst
  , invleft
  , invmap
  , invright
  , invsecond
  , invand
  , invor
  )
  where


import Data.Either (Either)
import Data.Tuple (Tuple)

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

class Invariant i <= FooInvariant i where
    invappend :: forall a . i a -> i a -> i a
    invempty :: forall a . i a
    -- laws: 
    --  invappend a invempty == a = invappend invempty a
    --  invappend a (invappend b c) == invappend (invappend a b) c

invand :: forall i a b . Invariant i => CartesianInvariant i => FooInvariant i => i a -> i b -> i (Tuple a b)
invand a b = invfirst a `invappend` invsecond b

invor :: forall i a b . Invariant i => CoCartesianInvariant i => FooInvariant i => i a -> i b -> i (Either a b)
invor a b = invleft a `invappend` invright b
