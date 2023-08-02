module Data.Invariant.Transformers
  ( foo, (#*)
  , Tunneled(..)
  , Tunneling(..)
  , invlift
  )
  where

import Prelude hiding (zero)

import Data.CoApplicative (class CoApply, cozip)
import Data.Either (Either(..), either)
import Data.Group (class Group, ginverse)
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, invfirst, invleft, invmap, invright, invsecond)
import Data.Invariant.Optics (invLens)
import Data.Invariant.Optics.Tagged (class Tagged, getPath, setPath)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Plus (class Plus, class Plusoid, plus, zero)
import Data.Tuple (Tuple(..), fst, snd)

-- Tunneled

newtype Tunneled :: forall k1 k2. (k1 -> Type) -> (k2 -> k1) -> k2 -> Type
newtype Tunneled f i a = Tunneled (f (i a))
derive instance Newtype (Tunneled f i a) _

-- Tunneled as an invariant using underlying invariant `i a` to tunnel `f (i a)`, where i is transparent to `f`.
-- Explanation inspired by a nice definition of tunneling from polish wikipedia (translated to english):
--   tunneling – setting up a connection between remote hosts through a network that does not know the protocol that these hosts communicate with
instance (Functor f, Invariant i) => Invariant (Tunneled f i) where
  invmap f g = wrap <<< map (invmap f g) <<< unwrap

instance (Functor f, Cartesian i) => Cartesian (Tunneled f i) where
  invfirst  = wrap <<< map invfirst <<< unwrap
  invsecond = wrap <<< map invsecond <<< unwrap

instance (Functor f, CoCartesian i) => CoCartesian (Tunneled f i) where
  invleft   = wrap <<< map invleft <<< unwrap
  invright  = wrap <<< map invright <<< unwrap

instance (Apply f, Plus i) => Plusoid (Tunneled f i) where
  plus c1 c2 = wrap $ plus <$> unwrap c1 <*> unwrap c2

instance (Applicative f, Plus i) => Plus (Tunneled f i) where
  zero = wrap $ pure zero

-- Tunneling

newtype Tunneling :: forall k1 k2. (k2 -> k1) -> (k1 -> Type) -> k2 -> Type
newtype Tunneling f i a = Tunneling (i (f a))
derive instance Newtype (Tunneling f i a) _

-- Tunneling as an invariant using underlying invariant `i a` to convey `f a`s instead of `a`s, where f is a functor that is transparent to `i`.
-- Explanation inspired by a nice definition of tunneling from polish wikipedia (translated to english):
--   tunneling – setting up a connection between remote hosts through a network that does not know the protocol that these hosts communicate with
instance (Functor f, Invariant i) => Invariant (Tunneling f i) where
  invmap f g = wrap <<< invmap (map f) (map g) <<< unwrap

-- If `i _` can be lifted with lenses, `i (f _)` can be lifted with lenses too as long as `Apply f`
instance (Apply f, Cartesian i) => Cartesian (Tunneling f i) where
  invfirst = wrap <<< invmap (\(Tuple fa fb) -> Tuple <$> fa <*> fb) (\fab -> Tuple (fst <$> fab) (snd <$> fab)) <<< invfirst <<< unwrap
  invsecond = wrap <<< invmap (\(Tuple fa fb) -> Tuple <$> fa <*> fb) (\fab -> Tuple (fst <$> fab) (snd <$> fab)) <<< invsecond <<< unwrap

-- If `i _` can be lifted with prisms, `i (f _)` can be lifted with prisms too as long as `CoApply f`
instance (CoApply f, CoCartesian i) => CoCartesian (Tunneling f i) where
  invleft = wrap <<< invmap (\efafb -> either (map Left) (map Right) efafb) cozip <<< invleft <<< unwrap
  invright = wrap <<< invmap (\efafb -> either (map Left) (map Right) efafb) cozip <<< invright <<< unwrap

instance Plusoid i => Plusoid (Tunneling f i) where
  plus c1 c2 = wrap $ plus (unwrap c1) (unwrap c2)

instance Plus i => Plus (Tunneling f i) where
  zero = wrap zero

instance Tagged i => Tagged (Tunneling f i) where
  getPath i = getPath $ unwrap i
  setPath p i = wrap $ setPath p $ unwrap i

-- Notice `Tunneling f i a → Tunneling f i b` parameter is an optic as `i` is an `Invariant` and `f` is a `Functor`.
-- Lens can be passed if `Tunneling f i` is `Cartesian` thus if `i` is `Cartesian` and `f` is an `Apply`.
-- Prism can be passed if `Tunneling f i` is `CoCartesian` thus if `i` is `CoCartesian and `f` is a `CoApply`.
-- Composed lens(es) and prism(s) can be passed if `i` is both `Cartesian and `CoCartesian` and `f` is both `Apply` and `CoApply` (e.g. `Identity`).
invlift ∷ ∀ i f a b. (Tunneling f i a → Tunneling f i b) → i (f a) → i (f b)
invlift optic = unwrap <<< optic <<< wrap

foo :: forall i733 f734 a735 b736. Invariant i733 => Functor f734 => i733 (f734 a735) -> (Tunneling f734 i733 a735 -> Tunneling f734 i733 b736) -> i733 (f734 b736)
foo inv optic = inv # invlift optic

infixl 1 foo as #*

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

-- Indexed

-- If an index of `i` prefixes or is prefixed by an index of `a` then `i` should be changed according to `a`
--                            index of `i`          index of `a`
--                                   v
--                                                        v
type Indexed x i a = Tunneled (Tuple x) (Tunneling (Tuple x) i) a
-- Tuple is Functor, Apply and CoApply so `Indexed x i` preserves Invariant, Cartesian, CoCartesian, Plusoid instances of `i`
-- Note: as Tuple is not Applicative, `Indexed x i` does not preserve Plus instance of `i`,
--       yet if `x` is a Monoid then `Tuple x` is an Applicative thus `Indexed x i` does preserve Plus instance of `i`.

mkIndexed :: forall x i a. Cartesian i => Monoid x => i a -> Indexed x i a
mkIndexed ia = wrap (Tuple mempty (wrap (invLens snd (\(Tuple p _) a  -> Tuple p a) ia)))

concatIndex :: forall x i a. Invariant i => Group x => x -> Indexed x i a -> Indexed x i a
concatIndex indexToPrepend ia =
  let
    Tuple index i = unwrap ia
    i' = invmap (\(Tuple index a) -> Tuple (indexToPrepend <> index) a) (\(Tuple index a) -> Tuple (ginverse indexToPrepend <> index) a) $ unwrap i
  in wrap $ Tuple (indexToPrepend <> index) i
