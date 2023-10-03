module Data.Invariant.Transformers.Tunneling
  ( Tunneling(..)
  )
  where

import Prelude

import Data.CoApplicative (class CoApplicative, class CoApply, copure, cozip)
import Data.Either (Either(..), either)
import Data.Invariant (class InvCartesian, class InvCocartesian, class Invariant, invfirst, invleft, invmap, invright, invsecond)
import Data.Invariant.Plus (class InvPlus, class InvPlusoid, invplus, invzero)
import Data.Invariant.Transformers (class InvTrans)
import Data.Newtype (class Newtype, modify, unwrap, wrap)
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy)

newtype Tunneling :: forall k1 k2. (k2 -> k1) -> (k1 -> Type) -> k2 -> Type
newtype Tunneling f i a = Tunneling (i (f a))
derive instance Newtype (Tunneling f i a) _

instance (Applicative f, CoApplicative f) => InvTrans (Tunneling f) where
  invlift = wrap <<< invmap pure copure
  invliftmap f = modify f

-- Tunneling as an invariant using underlying invariant `i a` to convey `f a`s instead of `a`s, where f is a functor that is transparent to `i`.
-- Explanation inspired by a nice definition of tunneling from polish wikipedia (translated to english):
--   tunneling – setting up a connection between remote hosts through a network that does not know the protocol that these hosts communicate with
instance (Functor f, Invariant i) => Invariant (Tunneling f i) where
  invmap f g = wrap <<< invmap (map f) (map g) <<< unwrap

-- If `i _` can be lifted with lenses, `i (f _)` can be lifted with lenses too as long as `Apply f`
instance (Apply f, InvCartesian i) => InvCartesian (Tunneling f i) where
  invfirst = wrap <<< invmap (\(Tuple fa fb) -> spy "tuple" $ Tuple <$> spy "fa" fa <*> spy "fb" fb) (\fab -> Tuple (fst <$> fab) (snd <$> fab)) <<< invfirst <<< unwrap
  invsecond = wrap <<< invmap (\(Tuple fa fb) -> Tuple <$> fa <*> fb) (\fab -> Tuple (fst <$> fab) (snd <$> fab)) <<< invsecond <<< unwrap

-- If `i _` can be lifted with prisms, `i (f _)` can be lifted with prisms too as long as `CoApply f`
instance (CoApply f, InvCocartesian i) => InvCocartesian (Tunneling f i) where
  invleft = wrap <<< invmap (\efafb -> either (map Left) (map Right) efafb) cozip <<< invleft <<< unwrap
  invright = wrap <<< invmap (\efafb -> either (map Left) (map Right) efafb) cozip <<< invright <<< unwrap

instance InvPlusoid i => InvPlusoid (Tunneling f i) where
  invplus c1 c2 = wrap $ invplus (unwrap c1) (unwrap c2)

instance InvPlus i => InvPlus (Tunneling f i) where
  invzero = wrap invzero

-- Notice `Tunneling f i a → Tunneling f i b` parameter is an optic as `i` is an `Invariant` and `f` is a `Functor`.
-- Lens can be passed if `Tunneling f i` is `InvCartesian` thus if `i` is `InvCartesian` and `f` is an `Apply`.
-- Prism can be passed if `Tunneling f i` is `InvCocartesian` thus if `i` is `InvCocartesian and `f` is a `CoApply`.
-- Composed lens(es) and prism(s) can be passed if `i` is both `InvCartesian and `InvCocartesian` and `f` is both `Apply` and `CoApply` (e.g. `Identity`).
-- TODO EC what is really is?
invlift' ∷ ∀ i f a b. (Tunneling f i a → Tunneling f i b) → i (f a) → i (f b)
invlift' optic = unwrap <<< optic <<< wrap

foo :: forall i f a b. Invariant i => Functor f => i (f a) -> (Tunneling f i a -> Tunneling f i b) -> i (f b)
foo inv optic = inv # invlift' optic


infixl 1 foo as #*

-- allows for e.g.: (private functions just to prove it typechecks)
liftAdapter :: forall i f a b. Invariant i => Functor f => (forall j. Invariant j => j a -> j b) -> i (f a) -> i (f b)
liftAdapter adapter = invlift' adapter

liftLens :: forall i f a b. InvCartesian i => Apply f => (forall j. InvCartesian j => j a -> j b) -> i (f a) -> i (f b)
liftLens lens = invlift' lens

liftPrism :: forall i f a b. InvCocartesian i => CoApply f => (forall j. InvCocartesian j => j a -> j b) -> i (f a) -> i (f b)
liftPrism prism = invlift' prism

liftCustom :: forall i f a b c d
  . InvCocartesian i
  => Invariant i
  => InvCartesian i
  => Functor f
  => Apply f
  => CoApply f
  => (forall j. InvCocartesian j => j a -> j b)
  -> (forall j. InvCartesian j => j b -> j c)
  -> (forall j. Invariant j => j c -> j d)
  -> i (f a) -> i (f d)
liftCustom prism lens adapter = invlift' (prism >>> lens >>> adapter)
