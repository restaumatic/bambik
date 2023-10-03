module Propagator where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Change (class ChProfunctor, Change(..))
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Plus (class ProfunctorPlus, class ProfunctorZero)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)

data Occurrence a = Occurrence Change a

derive instance Functor Occurrence

type Propagation a = Occurrence a -> Effect Unit

newtype Propagator m i o = Propagator (Propagation o -> m (Propagation i))
--                                     -- outward --       -- inward ---
--                                                      ^ artefact effect
-- Important: outward propagation should never be trigerred by inward propagation (TODO: how to encode it on type level? By allowing
-- update to perform only a subset of effects?) otherwise w1 ^ w2 for w1 and w2 doing so would cause propagation infinite loop
-- of mutual updates.

derive instance Newtype (Propagator m i o) _

-- Capabilites

instance Monad m => Profunctor (Propagator m) where
  dimap pre post w = Propagator \outward -> do
    f <- unwrap w $ outward <<< map post
    pure $ f <<< map pre

instance MonadEffect m => Strong (Propagator m) where
  first w = Propagator \outward -> do
    maandbRef <- liftEffect $ Ref.new Nothing
    update <- unwrap w \cha -> do
      maandb <- Ref.read maandbRef
      for_ maandb \(Tuple _ b) -> outward $ (\a -> Tuple a b) <$> cha
    pure \chab@(Occurrence _ aandb) -> do
      Ref.write (Just aandb) maandbRef
      case chab of
        Occurrence None _ -> mempty
        Occurrence _ _ -> update $ fst <$> chab
  second w = Propagator \outward -> do
    maandbRef <- liftEffect $ Ref.new Nothing
    update <- unwrap w \chb -> do
      maandb <- Ref.read maandbRef
      for_ maandb \(Tuple a _) -> outward $ (\b -> Tuple a b) <$> chb
    pure \chab@(Occurrence _ aandb) -> do
      Ref.write (Just aandb) maandbRef
      case chab of
        Occurrence None _ -> mempty
        Occurrence _ _ -> update $ snd <$> chab

class MonadEffect m <= MonadAttach m where
  attachable :: forall a. m a -> m { result :: a, attach :: Effect Unit, detach :: Effect Unit}

instance MonadAttach m => Choice (Propagator m) where
  left w = Propagator \outward -> do
    maorbRef <- liftEffect $ Ref.new Nothing
    { attach, detach, result: update } <- attachable $ unwrap w \cha -> do
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
    { attach, detach, result: update } <- attachable $ unwrap w \chb -> do
      maorb <- Ref.read maorbRef
      case maorb of
        Just (Right _) -> outward $ Right <$> chb
        _ -> mempty
    pure \chaorb@(Occurrence _ aorb) -> do
      moldaorb <- Ref.modify' (\oldState -> { state: Just aorb, value: oldState}) maorbRef
      case chaorb of
        Occurrence None _ -> mempty
        Occurrence _ (Right b) -> do
          update $ b <$ chaorb  -- first update and only then possibly attach
          case moldaorb of
            (Just (Right _)) -> mempty
            _ -> attach
        Occurrence _ (Left _) -> do
          case moldaorb of
            (Just (Right _)) -> detach
            _ -> mempty

instance MonadEffect m => ProfunctorPlus (Propagator m) where
  proplus c1 c2 = Propagator \updateParent -> do
    -- TODO how to get rid of thess refs?
    mUpdate1Ref <- liftEffect $ Ref.new Nothing
    mUpdate2Ref <- liftEffect $ Ref.new Nothing
    update1 <- unwrap c1 \cha@(Occurrence _ a) -> do
      mUpdate2 <- Ref.read mUpdate2Ref
      let update2 = maybe mempty identity mUpdate2
      mUpdate1 <- Ref.read mUpdate1Ref
      let update1 = maybe mempty identity mUpdate1
      update1 (Occurrence None a)
      update2 cha
      updateParent cha
    liftEffect $ Ref.write (Just update1) mUpdate1Ref
    update2 <- unwrap c2 \cha@(Occurrence _ a) -> do
      mUpdate2 <- Ref.read mUpdate2Ref
      let update2 = maybe mempty identity mUpdate2
      update2 (Occurrence None a)
      update1 cha
      updateParent cha
    liftEffect $ Ref.write (Just update2) mUpdate2Ref
    pure $ update1 <> update2

instance MonadEffect m => ProfunctorZero (Propagator m) where
  prozero = Propagator \_ -> pure mempty

instance Monad m => ChProfunctor (Propagator m) where
  chmap mapin mapout w = Propagator \outward -> do
    update <- unwrap w \(Occurrence c a) -> do
      outward $ Occurrence (mapout c) a
    pure \(Occurrence c a) -> update $ Occurrence (mapin c) a

instance MonadEffect m => Semigroupoid (Propagator m) where
  compose w2 w1 = Propagator \outward -> do
    update2Ref <- liftEffect $ Ref.new $ unsafeCoerce unit
    update1 <- unwrap w1 \(Occurrence _ b) -> do
      update2 <- Ref.read update2Ref
      update2 $ Occurrence Some b -- we force w2 to update cause w2 is updated only via outward by w1
    update2 <- unwrap w2 outward
    liftEffect $ Ref.write update2 update2Ref
    pure update1

class ProductProfunctor p where
  purePP :: forall a b. b -> p a b

instance Monad m => ProductProfunctor (Propagator m) where
  purePP b = Propagator \outward -> pure case _ of
    Occurrence None _ -> pure unit
    _ -> outward (Occurrence Some b)

effect :: forall i o m. MonadEffect m => (i -> Effect Unit) -> Propagator m i o
effect f = Propagator \_ -> pure \(Occurrence _ a) -> liftEffect $ f a -- outward is never called

-- Makes `Widget a b` fixed on `a` - no matter what `s` from the context of `Widget s t` is, so the `s`s are not listened to at all
fixed :: forall m a b s t. MonadEffect m => a -> Propagator m a b -> Propagator m s t
fixed a w = Propagator \_ -> do
  update <- unwrap w mempty
  liftEffect $ update $ Occurrence Some a
  pure mempty -- inward is never called

-- Suppresses outward propagation
hush :: forall m a b c. Propagator m a b -> Propagator m a c
hush w = Propagator \_ -> unwrap w mempty -- outward is never called

bracket :: forall m a b c. MonadEffect m => m c -> (c -> Occurrence a -> Effect Unit) -> (c -> Occurrence b -> Effect Unit) -> Propagator m a b -> Propagator m a b
bracket afterInit afterInward beforeOutward w = Propagator \outward -> do
  ctxRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  update <- unwrap w $ (\chb -> do
    ctx <- Ref.read ctxRef
    beforeOutward ctx chb) <> outward
  ctx <- afterInit
  liftEffect $ Ref.write ctx ctxRef
  pure $ update <> afterInward ctx
