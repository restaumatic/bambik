-- There exists Data.Functor.Invariant module in purescript-invariant (https://pursuit.purescript.org/packages/purescript-invariant/6.0.0) already,
-- yet here we tweak class hierarchy of invariant, covariant and contravariant functors according to observation made in [1]: 
-- "you could argue that in an ideal world the definition for Functor should change to class ExpFunctor f => Functor f".
--
-- References
-- 1. Edward Kmett: Rotten Bananas, http://comonad.com/reader/2008/rotten-bananas/  

module Data.Invariant
  ( class InvCartesian
  , class Closed
  , class InvCocartesian
  , class InvContravariant
  , class InvCovariant
  , class Invariant
  , closed
  , conmap
  , covmap
  , invfirst
  , invleft
  , invmap
  , invright
  , invsecond
  )
  where


import Data.Either (Either)
import Data.Tuple (Tuple)

-- Functor class hierarchy

class Invariant f where
    invmap :: forall a b . (a -> b) -> (b -> a) -> f a -> f b
    -- aka exponential functor

class Invariant f <= InvCovariant f where
    covmap :: forall a b . (a -> b) -> f a -> f b
    -- law: invmap = const <<< covmap

class Invariant f <= InvContravariant f where
    conmap :: forall a b . (b -> a) -> f a -> f b
    -- law: invmap = const conmap

class Invariant f <= InvCartesian f where
    invfirst :: forall a b. f a -> f (Tuple a b)
    invsecond :: forall a b. f b -> f (Tuple a b)

class Invariant f <= InvCocartesian f where
    invleft :: forall a b. f a -> f (Either a b)
    invright :: forall a b. f b -> f (Either a b)

class Invariant f <= Closed f where
    closed :: forall a x. f a -> f (x -> a)

-- TODO are these below needed?

-- invand :: forall i a b . InvCartesian i => InvPlus i => i a -> i b -> i (Tuple a b)
-- invand a b = invfirst a ^ invsecond b

-- invandwith :: forall i a b c . InvCartesian i => InvPlus i => (Tuple a b -> c) -> (c -> Tuple a b) -> i a -> i b -> i c
-- invandwith f g a b = invmap f g $ invand a b

-- invor :: forall i a b . InvCocartesian i => InvPlus i => i a -> i b -> i (Either a b)
-- invor a b = invleft a ^ invright b

-- invorwith :: forall i a b c . InvCocartesian i => InvPlus i => (Either a b -> c) -> (c -> Either a b) -> i a -> i b -> i c
-- invorwith f g a b = invmap f g $ invor a b
