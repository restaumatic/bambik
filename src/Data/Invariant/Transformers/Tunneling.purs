module Data.Invariant.Transformers.Tunneling
  ( Tunneling(..)
  )
  where

import Prelude

import Debug (spy)
import Data.CoApplicative (class CoApplicative, class CoApply, copure, cozip)
import Data.Either (Either(..), either)
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, invfirst, invleft, invmap, invright, invsecond)
import Data.Invariant.Transformers (class InvTrans)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Plus (class Plus, class Plusoid, plus, pzero)
import Data.Tuple (Tuple(..), fst, snd)

newtype Tunneling :: forall k1 k2. (k2 -> k1) -> (k1 -> Type) -> k2 -> Type
newtype Tunneling f i a = Tunneling (i (f a))
derive instance Newtype (Tunneling f i a) _

instance (Applicative f, CoApplicative f) => InvTrans (Tunneling f) where
  invlift = wrap <<< invmap pure copure

-- Tunneling as an invariant using underlying invariant `i a` to convey `f a`s instead of `a`s, where f is a functor that is transparent to `i`.
-- Explanation inspired by a nice definition of tunneling from polish wikipedia (translated to english):
--   tunneling – setting up a connection between remote hosts through a network that does not know the protocol that these hosts communicate with
instance (Functor f, Invariant i) => Invariant (Tunneling f i) where
  invmap f g = wrap <<< invmap (map f) (map g) <<< unwrap

-- If `i _` can be lifted with lenses, `i (f _)` can be lifted with lenses too as long as `Apply f`
instance (Apply f, Cartesian i) => Cartesian (Tunneling f i) where
  invfirst = wrap <<< invmap (\(Tuple fa fb) -> spy "tuple" $ Tuple <$> spy "fa" fa <*> spy "fb" fb) (\fab -> Tuple (fst <$> fab) (snd <$> fab)) <<< invfirst <<< unwrap
  invsecond = wrap <<< invmap (\(Tuple fa fb) -> Tuple <$> fa <*> fb) (\fab -> Tuple (fst <$> fab) (snd <$> fab)) <<< invsecond <<< unwrap

-- If `i _` can be lifted with prisms, `i (f _)` can be lifted with prisms too as long as `CoApply f`
instance (CoApply f, CoCartesian i) => CoCartesian (Tunneling f i) where
  invleft = wrap <<< invmap (\efafb -> either (map Left) (map Right) efafb) cozip <<< invleft <<< unwrap
  invright = wrap <<< invmap (\efafb -> either (map Left) (map Right) efafb) cozip <<< invright <<< unwrap

instance Plusoid i => Plusoid (Tunneling f i) where
  plus c1 c2 = wrap $ plus (unwrap c1) (unwrap c2)

instance Plus i => Plus (Tunneling f i) where
  pzero = wrap pzero

-- Notice `Tunneling f i a → Tunneling f i b` parameter is an optic as `i` is an `Invariant` and `f` is a `Functor`.
-- Lens can be passed if `Tunneling f i` is `Cartesian` thus if `i` is `Cartesian` and `f` is an `Apply`.
-- Prism can be passed if `Tunneling f i` is `CoCartesian` thus if `i` is `CoCartesian and `f` is a `CoApply`.
-- Composed lens(es) and prism(s) can be passed if `i` is both `Cartesian and `CoCartesian` and `f` is both `Apply` and `CoApply` (e.g. `Identity`).
-- TODO what is really is?
invlift' ∷ ∀ i f a b. (Tunneling f i a → Tunneling f i b) → i (f a) → i (f b)
invlift' optic = unwrap <<< optic <<< wrap

foo :: forall i f a b. Invariant i => Functor f => i (f a) -> (Tunneling f i a -> Tunneling f i b) -> i (f b)
foo inv optic = inv # invlift' optic


infixl 1 foo as #*

-- allows for e.g.: (private functions just to prove it typechecks)
liftAdapter :: forall i f a b. Invariant i => Functor f => (forall j. Invariant j => j a -> j b) -> i (f a) -> i (f b)
liftAdapter adapter = invlift' adapter

liftLens :: forall i f a b. Cartesian i => Apply f => (forall j. Cartesian j => j a -> j b) -> i (f a) -> i (f b)
liftLens lens = invlift' lens

liftPrism :: forall i f a b. CoCartesian i => CoApply f => (forall j. CoCartesian j => j a -> j b) -> i (f a) -> i (f b)
liftPrism prism = invlift' prism

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
liftCustom prism lens adapter = invlift' (prism >>> lens >>> adapter)
