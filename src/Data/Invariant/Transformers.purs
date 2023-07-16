module Data.Invariant.Transformers where

import Prelude hiding (zero)

import Data.Array (mapMaybe)
import Data.Either (Either(..), either)
import Data.Function (on)
import Data.Identity (Identity(..))
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, invfirst, invleft, invmap, invright, invsecond)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (NonEmpty, (:|), head, tail)
import Data.Plus (class Plus, class Plusoid, plus, zero)
import Data.Tuple (Tuple(..), fst, snd)

-- IOContext
-- For arbitrary monoid c, i (Tuple c _) preserves invariance, cartesian invariance, co-cartesian invariance of i as well as plus instance of i.
newtype IOContext c i a = IOContext (i (Tuple c a))

derive instance Newtype (IOContext c i a) _

instance Invariant i => Invariant (IOContext f i) where
  invmap pre post foo = wrap $ invmap (map pre) (map post) $ unwrap foo

instance (Invariant i, Cartesian i) => Cartesian (IOContext c i) where
  invfirst foo = wrap $ let icab = invfirst $ unwrap foo in invmap (\(Tuple (Tuple c a) b) -> Tuple c (Tuple a b)) (\(Tuple c (Tuple a b)) -> Tuple (Tuple c a) b) icab
  invsecond foo = wrap $ let ibca = invsecond $ unwrap foo in invmap (\(Tuple b (Tuple c a)) -> Tuple c (Tuple b a)) (\(Tuple c (Tuple b a)) -> Tuple b (Tuple c a)) ibca

instance (Invariant i, CoCartesian i, Monoid c) => CoCartesian (IOContext c i) where
  invleft foo = wrap $ let icab = invleft $ unwrap foo in invmap (\caorb -> case caorb of
    Left (Tuple c a) -> Tuple c (Left a)
    Right b -> Tuple mempty (Right b)) (\(Tuple c aorb) -> case aorb of
    Left a -> Left (Tuple c a)
    Right b -> Right b) icab
  invright foo = wrap $ let ibca = invright $ unwrap foo in invmap (\borca -> case borca of
    Right (Tuple c a) -> Tuple c (Right a)
    Left b -> Tuple mempty (Left b)) (\(Tuple c aorb) -> case aorb of
    Right a -> Right (Tuple c a)
    Left b -> Left b) ibca

instance Plusoid i => Plusoid (IOContext c i) where
  plus foo1 foo2 = wrap $ (plus `on` unwrap) foo1 foo2

instance Plus i => Plus (IOContext c i) where
  zero = wrap zero


--

newtype Foo :: forall k1 k2. (k1 -> k2) -> (k2 -> Type) -> k1 -> Type
newtype Foo f i a = Foo (i (f a))

derive instance Newtype (Foo f i a) _

instance (Functor f, Invariant i) => Invariant (Foo f i) where
  invmap f g = wrap <<< invmap (map f) (map g) <<< unwrap

-- If `i _` can be lifted with lenses, `i (f _)` can be lifted with lenses too as long as `Apply f`
instance (Apply f, Invariant i, Cartesian i) => Cartesian (Foo f i) where
  invfirst = wrap <<< invmap (\(Tuple fa fb) -> Tuple <$> fa <*> fb) (\fab -> Tuple (fst <$> fab) (snd <$> fab)) <<< invfirst <<< unwrap
  invsecond = wrap <<< invmap (\(Tuple fa fb) -> Tuple <$> fa <*> fb) (\fab -> Tuple (fst <$> fab) (snd <$> fab)) <<< invsecond <<< unwrap

-- If `i _` can be lifted with prisms, `i (f _)` can be lifted with prisms too as long as `CoApply f`
instance (CoApply f, Invariant i, CoCartesian i) => CoCartesian (Foo f i) where
  invleft = wrap <<< invmap (\efafb -> either (map Left) (map Right) efafb) cozip <<< invleft <<< unwrap
  invright = wrap <<< invmap (\efafb -> either (map Left) (map Right) efafb) cozip <<< invright <<< unwrap

-- If `i _` can be appended, `i (f _)` can be appended too
instance Plusoid i => Plusoid (Foo f i) where
  plus c1 c2 = wrap $ plus (unwrap c1) (unwrap c2)

-- If `i _` has zero, `i (f _)` has zero too
instance Plus i => Plus (Foo f i) where
  zero = wrap zero

liftAdapter :: forall i f a b. Invariant i => Functor f => (forall j. Invariant j => j a -> j b) -> i (f a) -> i (f b)
liftAdapter adapter ifa = unwrap (Foo ifa # adapter)

liftLens :: forall i f a b. Cartesian i => Apply f => (forall j. Cartesian j => j a -> j b) -> i (f a) -> i (f b)
liftLens lens ifa = unwrap (Foo ifa # lens)

liftPrism :: forall i f a b. CoCartesian i => CoApply f => (forall j. CoCartesian j => j a -> j b) -> i (f a) -> i (f b)
liftPrism prism ifa = unwrap (Foo ifa # prism)


-- foo :: forall i f a b. Invariant i => Cartesian i => Apply f =>(i a -> i b) -> i (f a) -> i (f b)
-- foo o ifa = unwrap (Foo ifa # o)


-- Cayley
-- For arbitrary functor f, f (i _)  preserves invariance, cartesian invariance, co-cartesian invariance of i.
-- For arbitrary applicative functor f, f (i _) preserves plus instance of i.
newtype Cayley :: forall k1 k2. (k1 -> Type) -> (k2 -> k1) -> k2 -> Type
newtype Cayley f i a = Cayley (f (i a))

derive instance Newtype (Cayley f i a) _

instance (Functor f, Invariant p) => Invariant (Cayley f p) where
  invmap f g = wrap <<< map (invmap f g) <<< unwrap

instance (Functor f, Cartesian i) => Cartesian (Cayley f i) where
  invfirst  = wrap <<< map invfirst <<< unwrap
  invsecond = wrap <<< map invsecond <<< unwrap

instance (Functor f, CoCartesian i) => CoCartesian (Cayley f i) where
  invleft   = wrap <<< map invleft <<< unwrap
  invright  = wrap <<< map invright <<< unwrap

instance (Apply f, Plus i) => Plusoid (Cayley f i) where
  plus c1 c2 = wrap $ plus <$> unwrap c1 <*> unwrap c2

instance (Applicative f, Plus i) => Plus (Cayley f i) where
  zero = wrap $ pure zero


--

class Functor f <= CoApply f where
  cozip :: forall a b. f (Either a b) -> Either (f a) (f b)

class CoApply f <= CoApplicative f where
  copure :: forall a. f a -> a

instance CoApply Identity where
  cozip (Identity (Left x)) = Left (Identity x)
  cozip (Identity (Right x)) = Right (Identity x)

instance CoApplicative Identity where
  copure (Identity x) = x

instance CoApply (NonEmpty Array) where
  cozip ne = case head ne of
    Left x -> Left $ x :| mapMaybe (either Just (const Nothing)) (tail ne)
    Right y -> Right $ y :| mapMaybe (either (const Nothing) Just) (tail ne)

instance CoApplicative (NonEmpty Array) where
  copure = head

-- instance CoApplicative w => CoApplicative (StoreT s w) where
--   copure (StoreT (Tuple wsa s)) = (copure wsa) s
--   cozip (StoreT (Tuple wsa s))  = case (copure wsa) s of
--     Left  a -> Left  (StoreT (Tuple (map $ either identity (const a) <<< wsa) s))
--     Right b -> Right (StoreT (Tuple (map $ either (const b) identity <<< wsa) s))

-- instance CoApplicative Costate where
--   copure (Costate _ a) = a
--   cozip (Costate f (Left  a)) = Left  (Costate (f <<< Left ) a)
--   cozip (Costate f (Right a)) = Right (Costate (f <<< Right) a)

instance CoApply (Tuple s) where
  cozip (Tuple c (Left x)) = Left $ Tuple c x
  cozip (Tuple c (Right x)) = Right $ Tuple c x

instance CoApplicative (Tuple s) where
  copure (Tuple _  x) = x
