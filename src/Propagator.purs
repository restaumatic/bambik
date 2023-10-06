module Propagator
  ( Change(..)
  , Occurrence(..)
  , Propagation
  , Propagator(..)
  , Scope(..)
  , attachable
  , bracket
  , class MonadGUI
  , debounced
  , fixed
  , followedByEffect
  , hush
  , precededByEffect
  , (^)
  , scopemap
  )
  where

import Prelude

import Data.Array (uncons, (:))
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds, delay, error, forkAff, joinFiber, killFiber, launchAff_, runAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (read, write)
import Effect.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)

newtype Propagator m i o = Propagator (Propagation o -> m (Propagation i))
--                                     -- outward --       -- inward ---
--                                                      ^ artefact effect
-- Important: outward propagation should never be trigerred by inward propagation (TODO: how to encode it on type level? By allowing
-- inward to perform only a subset of effects?) otherwise w1 ^ w2 for w1 and w2 doing so would cause propagation infinite loop
-- of mutual updates.

type Propagation a = Occurrence a -> Effect Unit

data Occurrence a = Occurrence Change a

data Change = Some | Scoped (NonEmptyArray.NonEmptyArray Scope) | None -- TODO: find already existing data type for it

data Scope = Part String | Variant String

derive instance Newtype (Propagator m i o) _

derive instance Functor Occurrence

instance Semigroup Change where -- finds least common scope
  append None s = s
  append s None = s
  append Some _ = Some
  append _ Some = Some
  append (Scoped hops1) (Scoped hops2) = case NonEmptyArray.fromArray $ commonPrefix hops1 hops2 of
    Nothing -> Some -- no common prefix
    Just prefix -> Scoped prefix -- common prefix
    where
      commonPrefix :: forall a. Eq a => NonEmptyArray.NonEmptyArray a -> NonEmptyArray.NonEmptyArray a -> Array a
      commonPrefix a1 a2 = let
        {head: h1, tail: t1} = NonEmptyArray.uncons a1
        {head: h2, tail: t2} = NonEmptyArray.uncons a2
        in if h1 == h2 then h1:fromMaybe [] (commonPrefix <$> NonEmptyArray.fromArray t1 <*> NonEmptyArray.fromArray t2)
        else []

instance Monoid Change where
  mempty = None

derive instance Eq Scope

instance Monad m => Profunctor (Propagator m) where
  dimap pre post w = Propagator \outward -> do
    inward <- unwrap w $ outward <<< map post
    pure $ inward <<< map pre

instance MonadEffect m => Strong (Propagator m) where
  first w = Propagator \outward -> do
    maandbRef <- liftEffect $ Ref.new Nothing
    inward <- unwrap w \cha -> do
      maandb <- Ref.read maandbRef
      for_ maandb \(Tuple _ b) -> outward $ (\a -> Tuple a b) <$> cha
    pure \chab@(Occurrence _ aandb) -> do
      Ref.write (Just aandb) maandbRef
      case chab of
        Occurrence None _ -> mempty
        Occurrence _ _ -> inward $ fst <$> chab
  second w = Propagator \outward -> do
    maandbRef <- liftEffect $ Ref.new Nothing
    inward <- unwrap w \chb -> do
      maandb <- Ref.read maandbRef
      for_ maandb \(Tuple a _) -> outward $ (\b -> Tuple a b) <$> chb
    pure \chab@(Occurrence _ aandb) -> do
      Ref.write (Just aandb) maandbRef
      case chab of
        Occurrence None _ -> mempty
        Occurrence _ _ -> inward $ snd <$> chab

class MonadEffect m <= MonadGUI m where
  attachable :: forall a. m (a -> Effect Unit) -> m { update :: a -> Effect Unit, attach :: Effect Unit, detach :: Effect Unit}

instance MonadGUI m => Choice (Propagator m) where
  left w = Propagator \outward -> do
    maorbRef <- liftEffect $ Ref.new Nothing
    { attach, detach, update } <- attachable $ unwrap w \cha -> do
      maorb <- Ref.read maorbRef
      case maorb of
        Just (Left _) -> outward $ Left <$> cha
        _ -> mempty
    pure \chaorb@(Occurrence _ aorb) -> do
      moldaorb <- Ref.modify' (\oldState -> { state: Just aorb, value: oldState}) maorbRef
      case chaorb of
        Occurrence None _ -> mempty
        Occurrence _ (Left a) -> do
          update $ a <$ chaorb -- first update and only then possibly attach
          case moldaorb of
            (Just (Left _)) -> mempty
            _ -> attach
        Occurrence _ (Right _) -> do
          case moldaorb of
            (Just (Left _)) -> detach
            _ -> mempty
  right w = Propagator \outward -> do
    maorbRef <- liftEffect $ Ref.new Nothing
    { attach, detach, update } <- attachable $ unwrap w \chb -> do
      maorb <- Ref.read maorbRef
      case maorb of
        Just (Right _) -> outward $ Right <$> chb
        _ -> mempty
    pure \chaorb@(Occurrence _ aorb) -> do
      moldaorb <- Ref.modify' (\oldState -> { state: Just aorb, value: oldState}) maorbRef
      case chaorb of
        Occurrence None _ -> mempty
        Occurrence _ (Right b) -> do
          update $ b <$ chaorb  -- first inward and only then possibly attach
          case moldaorb of
            (Just (Right _)) -> mempty
            _ -> attach
        Occurrence _ (Left _) -> do
          case moldaorb of
            (Just (Right _)) -> detach
            _ -> mempty

instance MonadEffect m => Semigroupoid (Propagator m) where
  compose w2 w1 = Propagator \outward -> do
    update2Ref <- liftEffect $ Ref.new $ unsafeCoerce unit
    inward1 <- unwrap w1 \(Occurrence _ b) -> do
      inward2 <- Ref.read update2Ref
      inward2 $ Occurrence Some b -- we force w2 to inward cause w2 is updated only via outward by w1
    inward2 <- unwrap w2 outward
    liftEffect $ Ref.write inward2 update2Ref
    pure inward1

instance MonadEffect m => Semigroup (Propagator m a a) where
  append c1 c2 = Propagator \updateParent -> do
    -- TODO how to get rid of thess refs?
    mUpdate1Ref <- liftEffect $ Ref.new Nothing
    mUpdate2Ref <- liftEffect $ Ref.new Nothing
    inward1 <- unwrap c1 \cha@(Occurrence _ a) -> do
      mUpdate2 <- Ref.read mUpdate2Ref
      let inward2 = maybe mempty identity mUpdate2
      mUpdate1 <- Ref.read mUpdate1Ref
      let inward1 = maybe mempty identity mUpdate1
      inward1 (Occurrence None a)
      inward2 cha
      updateParent cha
    liftEffect $ Ref.write (Just inward1) mUpdate1Ref
    inward2 <- unwrap c2 \cha@(Occurrence _ a) -> do
      mUpdate2 <- Ref.read mUpdate2Ref
      let inward2 = maybe mempty identity mUpdate2
      inward2 (Occurrence None a)
      inward1 cha
      updateParent cha
    liftEffect $ Ref.write (Just inward2) mUpdate2Ref
    pure $ inward1 <> inward2

infixr 0 append as ^ -- to lower precedence from 5 (<>) to 0 (^)

instance MonadEffect m => Monoid (Propagator m a a) where
  mempty = hush

precededByEffect :: forall m i i' o. MonadEffect m => (i' → Aff i) -> Propagator m i o -> Propagator m i' o
precededByEffect f = bracket (pure unit) (\_ (Occurrence _ i') -> f i' <#> Occurrence Some) (const pure)

followedByEffect :: forall m i o o'. MonadEffect m => (o -> Aff o') -> Propagator m i o -> Propagator m i o'
followedByEffect f = bracket (pure unit) (const pure) (\_ (Occurrence _ o) -> f o <#> Occurrence Some)

-- Makes `Widget a b` fixed on `a` - no matter what `s` from the context of `Widget s t` is, so the `s`s are not listened to at all
fixed :: forall m a b s t. MonadEffect m => a -> Propagator m a b -> Propagator m s t
fixed a w = Propagator \_ -> do
  inward <- unwrap w mempty
  liftEffect $ inward $ Occurrence Some a
  pure mempty -- inward is never called again, outward is never called

-- Suppresses outward propagation
hush :: forall m i o. Applicative m => Propagator m i o
hush = Propagator \_ -> pure mempty

debounced :: forall m i o. MonadEffect m => Milliseconds -> Propagator m i o -> Propagator m i o
debounced millis = bracket (liftEffect $ Ref.new Nothing) (const $ pure) (\mFiberRef occur -> do
  mFiber <- liftEffect $ read mFiberRef
  case mFiber of
    Nothing -> pure unit
    Just fiber -> killFiber (error "Debounce") fiber
  newFiber <- forkAff do
    delay millis
    pure occur
  liftEffect $ write (Just newFiber) mFiberRef
  joinFiber newFiber
  )

bracket :: forall m c i o i' o'. MonadEffect m => m c -> (c -> Occurrence i' -> Aff (Occurrence i)) -> (c -> Occurrence o -> Aff (Occurrence o')) -> Propagator m i o -> Propagator m i' o'
bracket afterInit afterInward beforeOutward w = Propagator \outward -> do
  cRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  inward <- unwrap w \occurb -> do
    ctx <- Ref.read cRef
    runAff_ (const $ pure unit) do
      o' <- beforeOutward ctx occurb
      liftEffect $ outward o'
  ctx <- afterInit
  liftEffect $ Ref.write ctx cRef
  pure \occuri' -> launchAff_ do
      i <- afterInward ctx occuri'
      liftEffect $ inward i

scopemap :: forall m a b. Monad m => Scope -> Propagator m a b -> Propagator m a b
scopemap scope = chmap zoomIn zoomOut
  where
    chmap :: (Change -> Change) -> (Change -> Change) -> Propagator m a b -> Propagator m a b
    chmap mapin mapout w = Propagator \outward -> do
      inward <- unwrap w \(Occurrence c a) -> do
        outward $ Occurrence (mapout c) a
      pure \(Occurrence c a) -> inward $ Occurrence (mapin c) a

    zoomOut :: Change -> Change
    zoomOut Some = Scoped (scope `NonEmptyArray.cons'` [])
    zoomOut (Scoped scopes) = Scoped (scope `NonEmptyArray.cons` scopes)
    zoomOut None = None

    zoomIn :: Change -> Change
    zoomIn Some = Some
    zoomIn (Scoped scopes) = case NonEmptyArray.uncons scopes of
      { head, tail } | head == scope -> case uncons tail of -- matching head
        Just { head: headOtTail, tail: tailOfTail } -> Scoped $ NonEmptyArray.cons' headOtTail tailOfTail -- non empty tail
        Nothing -> Some -- empty tail
      { head: Variant _ } -> Some -- not matching head but head is twist
      _ -> case scope of
        Variant _ -> Some -- not matching head but scope is twist
        _ -> None -- otherwise
    zoomIn None = None

