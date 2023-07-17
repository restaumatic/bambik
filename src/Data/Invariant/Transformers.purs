module Data.Invariant.Transformers
  ( ComposeInvInside(..)
  , ComposeInvOutside(..)
  , invlift
  )
  where

import Prelude hiding (zero)

import Data.CoApplicative (class CoApply, cozip)
import Data.Either (Either(..), either)
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, invfirst, invleft, invmap, invright, invsecond)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Plus (class Plus, class Plusoid, plus, zero)
import Data.Tuple (Tuple(..), fst, snd)

newtype ComposeInvInside :: forall k1 k2. (k1 -> Type) -> (k2 -> k1) -> k2 -> Type
newtype ComposeInvInside f i a = ComposeInvInside (f (i a))
derive instance Newtype (ComposeInvInside f i a) _

instance (Functor f, Invariant i) => Invariant (ComposeInvInside f i) where
  invmap f g = wrap <<< map (invmap f g) <<< unwrap

instance (Functor f, Cartesian i) => Cartesian (ComposeInvInside f i) where
  invfirst  = wrap <<< map invfirst <<< unwrap
  invsecond = wrap <<< map invsecond <<< unwrap

instance (Functor f, CoCartesian i) => CoCartesian (ComposeInvInside f i) where
  invleft   = wrap <<< map invleft <<< unwrap
  invright  = wrap <<< map invright <<< unwrap

instance (Apply f, Plus i) => Plusoid (ComposeInvInside f i) where
  plus c1 c2 = wrap $ plus <$> unwrap c1 <*> unwrap c2

instance (Applicative f, Plus i) => Plus (ComposeInvInside f i) where
  zero = wrap $ pure zero


-- Notice `ComposeInvOutside i f a → ComposeInvOutside i f b` parameter is an optic as `i` is an `Invariant` and `f` is a `Functor`.
-- Lens can be passed if `ComposeInvOutside i f` is `Cartesian` thus if `i` is `Cartesian` and `f` is an `Apply`.
-- Prism can be passed if `ComposeInvOutside i f` is `CoCartesian` thus if `i` is `CoCartesian and `f` is a `CoApply`.
-- Composed lens(es) and prism(s) can be passed if `i` is both `Cartesian and `CoCartesian` and `f` is both `Apply` and `CoApply` (e.g. `Identity`).
invlift ∷ ∀ i f a b. Invariant i => Functor f => (ComposeInvOutside i f a → ComposeInvOutside i f b) → i (f a) → i (f b)
invlift optic = unwrap <<< optic <<< wrap

-- allows for e.g.: (private functions just to prove it typechecks)
liftAdapter :: forall i f a b. Invariant i => Functor f => (forall j. Invariant j => j a -> j b) -> i (f a) -> i (f b)
liftAdapter adapter = invlift adapter

liftLens :: forall i f a b. Cartesian i => Apply f => (forall j. Cartesian j => j a -> j b) -> i (f a) -> i (f b)
liftLens lens = invlift lens

liftPrism :: forall i f a b. CoCartesian i => CoApply f => (forall j. CoCartesian j => j a -> j b) -> i (f a) -> i (f b)
liftPrism prism = invlift prism

liftCustom :: forall i f a b c d
  . CoCartesian i
  => Invariant i
  => Cartesian i
  => Functor f
  => Apply f
  => CoApply f
  => (forall j. CoCartesian j => j a -> j b)
  -> (forall j. Cartesian j => j b -> j c)
  -> (forall j. Invariant j => j c -> j d)
  -> i (f a) -> i (f d)
liftCustom prism lens adapter = invlift (prism >>> lens >>> adapter)


newtype ComposeInvOutside :: forall k1 k2. (k1 -> Type) -> (k2 -> k1) -> k2 -> Type
newtype ComposeInvOutside i f a = ComposeInvOutside (i (f a))
derive instance Newtype (ComposeInvOutside i f a) _

instance (Functor f, Invariant i) => Invariant (ComposeInvOutside i f) where
  invmap f g = wrap <<< invmap (map f) (map g) <<< unwrap

-- If `i _` can be lifted with lenses, `i (f _)` can be lifted with lenses too as long as `Apply f`
instance (Apply f, Cartesian i) => Cartesian (ComposeInvOutside i f) where
  invfirst = wrap <<< invmap (\(Tuple fa fb) -> Tuple <$> fa <*> fb) (\fab -> Tuple (fst <$> fab) (snd <$> fab)) <<< invfirst <<< unwrap
  invsecond = wrap <<< invmap (\(Tuple fa fb) -> Tuple <$> fa <*> fb) (\fab -> Tuple (fst <$> fab) (snd <$> fab)) <<< invsecond <<< unwrap

-- If `i _` can be lifted with prisms, `i (f _)` can be lifted with prisms too as long as `CoApply f`
instance (CoApply f, CoCartesian i) => CoCartesian (ComposeInvOutside i f) where
  invleft = wrap <<< invmap (\efafb -> either (map Left) (map Right) efafb) cozip <<< invleft <<< unwrap
  invright = wrap <<< invmap (\efafb -> either (map Left) (map Right) efafb) cozip <<< invright <<< unwrap

instance Plusoid i => Plusoid (ComposeInvOutside i f) where
  plus c1 c2 = wrap $ plus (unwrap c1) (unwrap c2)

instance Plus i => Plus (ComposeInvOutside i f) where
  zero = wrap zero
