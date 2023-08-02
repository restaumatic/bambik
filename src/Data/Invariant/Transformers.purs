module Data.Invariant.Transformers
  ( (#*)
  , Tunneled(..)
  , Tunneling(..)
  , concatIndex
  , foo
  , invField
  , invlift
  , mkIndexed
  )
  where

import Prelude hiding (zero)

import Data.Array (cons, head, mapMaybe, tail, takeWhile, uncons, zipWith)
import Data.CoApplicative (class CoApply, cozip)
import Data.Either (Either(..), either)
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, invfirst, invleft, invmap, invright, invsecond)
import Data.Invariant.Optics (invLens)
import Data.Invariant.Optics.Tagged (class Tagged, getPath, setPath)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Plus (class Plus, class Plusoid, plus, zero)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy)

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
-- For Semigroup `x`, `Tuple x`, i Functor, Apply and CoApply so `Indexed x i` preserves Invariant, Cartesian, CoCartesian, Plusoid of of `i`.
-- If, additionally, Monoid `x` then `Tuple x` instantiates Applicative thus `Indexed x i` does preserve Plus instance of `i`.

data Scope = Scope (Array String)

-- finds least common scope
instance Semigroup Scope where
  append (Scope a1) (Scope a2) = Scope $ mapMaybe identity $ takeWhile isJust $ zipWith (\e1 e2 -> if e1 == e2 then Just e1 else Nothing) a1 a2

-- laws full <> x = full = x <> full
class Semigroup s <= Full s where
  full :: s

instance Full Scope where
  full = Scope []

zoomOut :: String -> Scope -> Scope
zoomOut hop (Scope hops) = Scope $ hop `cons` hops  -- zooming out full in not full

zoomIn :: Scope -> Scope
zoomIn s@(Scope hops) = case tail hops of
  Just tail -> Scope tail
  Nothing -> s -- zooming in into full is full

zoomIn' :: String -> Scope -> Maybe Scope
zoomIn' hop s@(Scope hops) = case uncons hops of
  Nothing -> Just s -- zooming in into full is full
  Just { head, tail }
    | head == hop -> Just $ Scope tail
    | otherwise -> Nothing -- cannot zooming in


mkIndexed :: forall x i a. Cartesian i => Monoid x => i a -> Indexed x i a
mkIndexed ia = wrap (Tuple mempty (wrap (invLens snd (\(Tuple p _) a  -> Tuple p a) ia)))

concatIndex :: forall x i a. Invariant i => String -> Indexed Scope i a -> Indexed Scope i a
concatIndex hop ia =
  let
    Tuple scope i = unwrap ia
    i' = invmap (\(Tuple scope a) -> Tuple (zoomOut hop scope) a) (\(Tuple scope a) -> Tuple (zoomIn' hop scope) a) $ unwrap i
  in wrap $ Tuple (zoomOut hop scope) i
-- problem: how to conditionally make invariant being indexed not update itself

-- then we can come up with an optic:
invField
  :: forall i l r1 r a
  . Cartesian i
  => Tagged i
  => IsSymbol l
  => Row.Cons l a r r1
  => Proxy l
  -> Indexed Scope i a -> Indexed Scope i (Record r1)
invField l = invLens (get l) (flip (set l)) >>> concatIndex [reflectSymbol l]
