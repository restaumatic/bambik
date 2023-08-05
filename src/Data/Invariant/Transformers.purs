module Data.Invariant.Transformers
  ( (#*)
  , Scope
  , Scoped(..)
  , Tunneled(..)
  , Tunneling(..)
  , class InvTrans
  , foo
  , invConstructor
  , invField
  , invField'
  , invlift
  )
  where

import Prelude hiding (zero)

import Data.Array (cons, uncons)
import Data.CoApplicative (class CoApplicative, class CoApply, copure, cozip)
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Invariant (class Cartesian, class CoCartesian, class Filtered, class Invariant, invfirst, invfright, invleft, invmap, invright, invsecond)
import Data.Invariant.Optics (invLens, invPrism)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Plus (class Plus, class Plusoid, plus, pzero)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Zero (class Zero, zero)
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy)

class InvTrans t where
  invlift :: forall i a. Invariant i => i a -> t i a

-- Tunneled

newtype Tunneled :: forall k1 k2. (k1 -> Type) -> (k2 -> k1) -> k2 -> Type
newtype Tunneled f i a = Tunneled (f (i a))
derive instance Newtype (Tunneled f i a) _

instance Applicative f => InvTrans (Tunneled f) where
  invlift = wrap <<< pure

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
  pzero = wrap $ pure pzero

-- Tunneling

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
  invfirst = wrap <<< invmap (\(Tuple fa fb) -> Tuple <$> fa <*> fb) (\fab -> Tuple (fst <$> fab) (snd <$> fab)) <<< invfirst <<< unwrap
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

-- Scoped: carrying additional info about the scope of a change
-- `Scope` is a Monoid
--   => `Tuple Scope` is a Functor, Apply, CoApply
--     => `Scoped i` preserves Invariant, Cartesian, CoCartesian, Plusoid and Plus instances of of `i`.
--   => `Tuple Scope` is CoApplicative and Applicative so
--     => `Scoped i` is InvTransformer.
type Scoped i a = Tunneling (Tuple Scope) i a

data Scope = Scope (Array Hop) | AnyScope -- this is actually a lattice (bottom, singleton vales, top), TODO find already existing data type for it

type Hop = String

instance Show Scope where
  show AnyScope = "*"
  show (Scope hops) = "'" <> intercalate "/" hops <> "'"

-- finds least common scope
instance Semigroup Scope where
  append AnyScope s = s
  append s AnyScope = s
  append _ _ = zero

instance Monoid Scope where
  mempty = AnyScope

instance Zero Scope where
  zero = Scope []

scopedOut :: forall i a. Filtered i => Hop -> Scoped i a -> Scoped i a
scopedOut hop = wrap <<< invmap (\e -> let (Tuple subScope a) = either identity identity e in Tuple (zoomOut subScope) a) (\(Tuple scope a) -> maybe (Left (Tuple scope a)) (\subScope -> Right (Tuple subScope a)) (zoomIn scope)) <<< invfright <<< unwrap
  where
  zoomOut :: Scope -> Scope
  zoomOut AnyScope = Scope [hop]
  zoomOut (Scope hops) = Scope (hop `cons` hops)  -- zooming out zero is not zero
  zoomIn :: Scope -> Maybe Scope
  zoomIn AnyScope = Just AnyScope
  zoomIn s@(Scope hops) = case uncons hops of
    Nothing -> Just s -- zooming in zero is zero
    Just { head, tail }
      | head == hop -> Just $ Scope tail
      | otherwise -> Nothing -- cannot zoom in

-- then we can come up with an optic:

invField :: forall i a s. Filtered i => Cartesian i => String -> (s -> a -> s) -> (s -> a) -> Scoped i a -> Scoped i s
invField fieldName setter getter = invLens getter setter >>> scopedOut fieldName

invConstructor :: forall i a s. Filtered i => CoCartesian i => String -> (a -> s) -> (s -> Maybe a) -> Scoped i a -> Scoped i s
invConstructor constructorName construct deconstruct = invPrism construct (\s -> maybe (Right s) Left (deconstruct s)) >>> scopedOut constructorName

---

invField'
  :: forall i l r1 r a
  . Filtered i
  => Cartesian i
  => IsSymbol l
  => Row.Cons l a r r1
  => Proxy l
  -> Scoped i a -> Scoped i (Record r1)
invField' l = invLens (get l) (flip (set l)) >>> scopedOut (reflectSymbol l)
