module Data.Invariant.Transformers
  ( foo, (#*)
  , Static(..)
  , Tunnel(..)
  , invlift
  )
  where

import Prelude hiding (zero)

import Data.Array (cons)
import Data.CoApplicative (class CoApply, cozip)
import Data.Either (Either(..), either)
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, invfirst, invleft, invmap, invright, invsecond)
import Data.Invariant.Optics (invLens)
import Data.Invariant.Optics.Tagged (class Tagged, getPath, setPath)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Plus (class Plus, class Plusoid, plus, zero)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Exception.Unsafe (unsafeThrow)

newtype Static :: forall k1 k2. (k1 -> Type) -> (k2 -> k1) -> k2 -> Type
newtype Static f i a = Static (f (i a))
derive instance Newtype (Static f i a) _

instance (Functor f, Invariant i) => Invariant (Static f i) where
  invmap f g = wrap <<< map (invmap f g) <<< unwrap

instance (Functor f, Cartesian i) => Cartesian (Static f i) where
  invfirst  = wrap <<< map invfirst <<< unwrap
  invsecond = wrap <<< map invsecond <<< unwrap

instance (Functor f, CoCartesian i) => CoCartesian (Static f i) where
  invleft   = wrap <<< map invleft <<< unwrap
  invright  = wrap <<< map invright <<< unwrap

instance (Apply f, Plus i) => Plusoid (Static f i) where
  plus c1 c2 = wrap $ plus <$> unwrap c1 <*> unwrap c2

instance (Applicative f, Plus i) => Plus (Static f i) where
  zero = wrap $ pure zero

newtype Tunnel :: forall k1 k2. (k2 -> k1) -> (k1 -> Type) -> k2 -> Type
newtype Tunnel f i a = Tunnel (i (f a))
derive instance Newtype (Tunnel f i a) _

-- Tunnel as an invariant uses underlying invariant `i a` to convey `f a`s instead of `a`s, where f is a functor that is transparent to `i`.
-- Explanation inspired by a nice definition of tunneling from polish wikipedia (translated to english):
--   tunneling – setting up a connection between remote hosts through a network that does not know the protocol that these hosts communicate with
instance (Functor f, Invariant i) => Invariant (Tunnel f i) where
  invmap f g = wrap <<< invmap (map f) (map g) <<< unwrap

-- If `i _` can be lifted with lenses, `i (f _)` can be lifted with lenses too as long as `Apply f`
instance (Apply f, Cartesian i) => Cartesian (Tunnel f i) where
  invfirst = wrap <<< invmap (\(Tuple fa fb) -> Tuple <$> fa <*> fb) (\fab -> Tuple (fst <$> fab) (snd <$> fab)) <<< invfirst <<< unwrap
  invsecond = wrap <<< invmap (\(Tuple fa fb) -> Tuple <$> fa <*> fb) (\fab -> Tuple (fst <$> fab) (snd <$> fab)) <<< invsecond <<< unwrap

-- If `i _` can be lifted with prisms, `i (f _)` can be lifted with prisms too as long as `CoApply f`
instance (CoApply f, CoCartesian i) => CoCartesian (Tunnel f i) where
  invleft = wrap <<< invmap (\efafb -> either (map Left) (map Right) efafb) cozip <<< invleft <<< unwrap
  invright = wrap <<< invmap (\efafb -> either (map Left) (map Right) efafb) cozip <<< invright <<< unwrap

instance Plusoid i => Plusoid (Tunnel f i) where
  plus c1 c2 = wrap $ plus (unwrap c1) (unwrap c2)

instance Plus i => Plus (Tunnel f i) where
  zero = wrap zero

instance Tagged i => Tagged (Tunnel f i) where
  getPath i = getPath $ unwrap i
  setPath p i = wrap $ setPath p $ unwrap i

-- Notice `Tunnel f i a → Tunnel f i b` parameter is an optic as `i` is an `Invariant` and `f` is a `Functor`.
-- Lens can be passed if `Tunnel f i` is `Cartesian` thus if `i` is `Cartesian` and `f` is an `Apply`.
-- Prism can be passed if `Tunnel f i` is `CoCartesian` thus if `i` is `CoCartesian and `f` is a `CoApply`.
-- Composed lens(es) and prism(s) can be passed if `i` is both `Cartesian and `CoCartesian` and `f` is both `Apply` and `CoApply` (e.g. `Identity`).
invlift ∷ ∀ i f a b. (Tunnel f i a → Tunnel f i b) → i (f a) → i (f b)
invlift optic = unwrap <<< optic <<< wrap

foo :: forall i733 f734 a735 b736. Invariant i733 => Functor f734 => i733 (f734 a735) -> (Tunnel f734 i733 a735 -> Tunnel f734 i733 b736) -> i733 (f734 b736)
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


-- if an index of `i` prefixes or is prefixed by an index of `a` then `i` should be changed according to `a`
--                                        index of `i`          index of `a`
--                                           vvvvv
--                                                                 vvvvv
newtype Indexed i a = Indexed (Static (Tuple Index) (Tunnel (Tuple Index) i) a)
type Index = Array Hop
type Hop = String

derive instance Newtype (Indexed i a) _


-- Tuple is Functor, Apply and CoApply so `Indexed i` preserves Invariant, Cartesian, CoCartesian, Plusoid instances of `i`
-- Note: as Tuple is not Applicative, `Indexed i` does not preserve Plus instance of `i`,
--       yet if the Tuple's first component is a Monoid then Tuple is Applicative thus `Indexed i` does preserve Plus instance of `i`.

mkIndexed :: forall i a. Cartesian i => i a -> Indexed i a
mkIndexed ia = wrap $ wrap (Tuple [] (wrap (invLens snd (\(Tuple p _) a  -> Tuple p a) ia)))

prependIndex :: forall i a. Invariant i => Index -> Indexed i a -> Indexed i a
prependIndex indexToPrepend ia =
  let
    Tuple index i = unwrap $ unwrap ia
    i' = invmap (\(Tuple index a) -> Tuple (indexToPrepend <> index) a) identity $ unwrap i
  in wrap $ wrap $ Tuple (indexToPrepend <> index) i

instance Plus i => Plusoid (Indexed i) where
  plus p1 p2 = wrap $ unsafeThrow "!"
