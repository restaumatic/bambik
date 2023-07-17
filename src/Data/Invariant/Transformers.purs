module Data.Invariant.Transformers
  ( invlift
  )
  where

import Prelude

import Data.CoApplicative (class CoApply)
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, ComposeInvOutside)
import Data.Newtype (unwrap, wrap)

-- Notice `ComposeInvOutside i f a → ComposeInvOutside i f b` parameter is an optic as `i` is an `Invariant` and `f` is a `Functor`.
-- Lens can be passed if `ComposeInvOutside i f` is `Cartesian` thus if `i` is `Cartesian` and `f` is an `Apply`.
-- Prism can be passed if `ComposeInvOutside i f` is `CoCartesian` thus if `i` is `CoCartesian and `f` is a `CoApply`.
-- Composed lens(es) and prism(s) can be passed if `i` is both `Cartesian and `CoCartesian` and `f` is both `Apply` and `CoApply` (e.g. `Identity`).
invlift ∷ ∀ i f a b. Invariant i => Functor f => (ComposeInvOutside i f a → ComposeInvOutside i f b) → i (f a) → i (f b)
invlift optic ifa = unwrap $ (wrap ifa) # optic

-- allows for e.g.: (private functions just to prove it typechecks)
liftAdapter :: forall i f a b. Invariant i => Functor f => (forall j. Invariant j => j a -> j b) -> i (f a) -> i (f b)
liftAdapter adapter ifa = invlift adapter ifa

liftLens :: forall i f a b. Cartesian i => Apply f => (forall j. Cartesian j => j a -> j b) -> i (f a) -> i (f b)
liftLens lens ifa = invlift lens ifa

liftPrism :: forall i f a b. CoCartesian i => CoApply f => (forall j. CoCartesian j => j a -> j b) -> i (f a) -> i (f b)
liftPrism prism ifa = invlift prism ifa

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
liftCustom prism lens adapter ifa = invlift (prism >>> lens >>> adapter) ifa
