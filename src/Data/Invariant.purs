-- There exists Data.Functor.Invariant module in purescript-invariant (https://pursuit.purescript.org/packages/purescript-invariant/6.0.0) already,
-- yet here we tweak class hierarchy of invariant, covariant and contravariant functors according to observation made in [1]: 
-- "you could argue that in an ideal world the definition for Functor should change to class ExpFunctor f => Functor f".
--
-- References
-- 1. Edward Kmett: Rotten Bananas, http://comonad.com/reader/2008/rotten-bananas/  

module Data.Invariant
  ( class Cartesian
  , class CoCartesian
  , class Contravariant
  , class Covariant
  , class EffInvariant
  , class Invariant
  , class Closed
  , closed
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

import Data.Either (Either(..), either)
import Data.Functor.Compose (Compose)
import Data.CoApplicative (class CoApply, cozip)
import Data.Newtype (unwrap, wrap)
import Data.Plus (class Plus, plus)
import Data.Tuple (Tuple(..), fst, snd)
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

class Invariant f <= Cartesian f where
    invfirst :: forall a b. f a -> f (Tuple a b)
    invsecond :: forall a b. f b -> f (Tuple a b)

class Invariant f <= CoCartesian f where
    invleft :: forall a b. f a -> f (Either a b)
    invright :: forall a b. f b -> f (Either a b)

class Invariant f <= Closed f where
    closed :: forall a x. f a -> f (x -> a)

-- TODO: MonoidalInvariant:
-- class Monoidal f where
--     par :: forall a b. f a -> f b -> f (Tuple a b)
--     empty :: f Unit
-- is this really needed, once we have invand and zero?

invand :: forall i a b . Cartesian i => Plus i => i a -> i b -> i (Tuple a b)
invand a b = invfirst a `plus` invsecond b

invandwith :: forall i a b c . Cartesian i => Plus i => (Tuple a b -> c) -> (c -> Tuple a b) -> i a -> i b -> i c
invandwith f g a b = invmap f g $ invand a b

invor :: forall i a b . CoCartesian i => Plus i => i a -> i b -> i (Either a b)
invor a b = invleft a `plus` invright b

invorwith :: forall i a b c . CoCartesian i => Plus i => (Either a b -> c) -> (c -> Either a b) -> i a -> i b -> i c
invorwith f g a b = invmap f g $ invor a b

class EffInvariant i where
    inveff :: forall a . (a -> Effect Unit) -> i a -> i a


--

instance (Functor f, Invariant i) => Invariant (Compose i f) where
  invmap f g = wrap <<< invmap (map f) (map g) <<< unwrap

-- If `i _` can be lifted with lenses, `i (f _)` can be lifted with lenses too as long as `Apply f`
instance (Apply f, Cartesian i) => Cartesian (Compose i f) where
  invfirst = wrap <<< invmap (\(Tuple fa fb) -> Tuple <$> fa <*> fb) (\fab -> Tuple (fst <$> fab) (snd <$> fab)) <<< invfirst <<< unwrap
  invsecond = wrap <<< invmap (\(Tuple fa fb) -> Tuple <$> fa <*> fb) (\fab -> Tuple (fst <$> fab) (snd <$> fab)) <<< invsecond <<< unwrap

-- If `i _` can be lifted with prisms, `i (f _)` can be lifted with prisms too as long as `CoApply f`
instance (CoApply f, CoCartesian i) => CoCartesian (Compose i f) where
  invleft = wrap <<< invmap (\efafb -> either (map Left) (map Right) efafb) cozip <<< invleft <<< unwrap
  invright = wrap <<< invmap (\efafb -> either (map Left) (map Right) efafb) cozip <<< invright <<< unwrap

--

instance (Functor f, Invariant i) => Invariant (Compose f i) where
  invmap f g = wrap <<< map (invmap f g) <<< unwrap

instance (Functor f, Cartesian i) => Cartesian (Compose f i) where
  invfirst  = wrap <<< map invfirst <<< unwrap
  invsecond = wrap <<< map invsecond <<< unwrap

instance (Functor f, CoCartesian i) => CoCartesian (Compose f i) where
  invleft   = wrap <<< map invleft <<< unwrap
  invright  = wrap <<< map invright <<< unwrap

