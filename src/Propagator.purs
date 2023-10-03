module Propagator where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Change (class ChProfunctor, Change(..), Changed(..))
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Plus (class ProfunctorPlus, class ProfunctorZero)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)

type Occurence a = Changed a -- new can be changed or not changed

type Propagation a = Occurence a -> Effect Unit

newtype Propagator m i o = Propagator (Propagation o -> m (Propagation i))
--                                     -- outward --       -- inward ---
--                                                      ^ artefact effect
-- Important: outward propagation should never be trigerred by inward propagation (TODO: how to encode it on type level? By allowing
-- update to perform only a subset of effects?) otherwise w1 ^ w2, where w1 and w2 call back on on input will enter infinite loop
-- of mutual updates.

unwrapPropagator :: forall m i o. Propagator m i o -> Propagation o -> m (Propagation i)
unwrapPropagator (Propagator w) = w

-- Capabilites

instance Monad m => Profunctor (Propagator m) where
  dimap pre post w = Propagator \outward -> do
    f <- unwrapPropagator w $ outward <<< map post
    pure $ f <<< map pre

instance MonadEffect m => Strong (Propagator m) where
  first w = Propagator \outward -> do
    maandbRef <- liftEffect $ Ref.new Nothing
    update <- unwrapPropagator w \cha -> do
      maandb <- Ref.read maandbRef
      for_ maandb \(Tuple _ b) -> outward $ (\a -> Tuple a b) <$> cha
    pure \chab@(Changed _ aandb) -> do
      Ref.write (Just aandb) maandbRef
      case chab of
        Changed None _ -> mempty
        Changed _ _ -> update $ fst <$> chab
  second w = Propagator \outward -> do
    maandbRef <- liftEffect $ Ref.new Nothing
    update <- unwrapPropagator w \chb -> do
      maandb <- Ref.read maandbRef
      for_ maandb \(Tuple a _) -> outward $ (\b -> Tuple a b) <$> chb
    pure \chab@(Changed _ aandb) -> do
      Ref.write (Just aandb) maandbRef
      case chab of
        Changed None _ -> mempty
        Changed _ _ -> update $ snd <$> chab

class MonadAttach m where
  attachable :: forall a. m a -> m { result :: a, attach :: Effect Unit, detach :: Effect Unit}

instance (MonadEffect m, MonadAttach m) => Choice (Propagator m) where
  left w = Propagator \outward -> do
    maorbRef <- liftEffect $ Ref.new Nothing
    { attach, detach, result: update } <- attachable $ unwrapPropagator w \cha -> do
      maorb <- Ref.read maorbRef
      case maorb of
        Just (Left _) -> outward $ Left <$> cha
        _ -> mempty
    pure \chaorb@(Changed _ aorb) -> do
      moldaorb <- Ref.modify' (\oldState -> { state: Just aorb, value: oldState}) maorbRef
      case chaorb of
        Changed None _ -> mempty
        Changed _ (Left a) -> do
          update $ a <$ chaorb -- first update and only then possibly attach
          case moldaorb of
            (Just (Left _)) -> mempty
            _ -> attach
        Changed _ (Right _) -> do
          case moldaorb of
            (Just (Left _)) -> detach
            _ -> mempty
  right w = Propagator \outward -> do
    maorbRef <- liftEffect $ Ref.new Nothing
    { attach, detach, result: update } <- attachable $ unwrapPropagator w \chb -> do
      maorb <- Ref.read maorbRef
      case maorb of
        Just (Right _) -> outward $ Right <$> chb
        _ -> mempty
    pure \chaorb@(Changed _ aorb) -> do
      moldaorb <- Ref.modify' (\oldState -> { state: Just aorb, value: oldState}) maorbRef
      case chaorb of
        Changed None _ -> mempty
        Changed _ (Right b) -> do
          update $ b <$ chaorb  -- first update and only then possibly attach
          case moldaorb of
            (Just (Right _)) -> mempty
            _ -> attach
        Changed _ (Left _) -> do
          case moldaorb of
            (Just (Right _)) -> detach
            _ -> mempty

instance MonadEffect m => ProfunctorPlus (Propagator m) where
  proplus c1 c2 = Propagator \updateParent -> do
    -- TODO how to get rid of thess refs?
    mUpdate1Ref <- liftEffect $ Ref.new Nothing
    mUpdate2Ref <- liftEffect $ Ref.new Nothing
    update1 <- unwrapPropagator c1 \cha@(Changed _ a) -> do
      mUpdate2 <- Ref.read mUpdate2Ref
      let update2 = maybe mempty identity mUpdate2
      mUpdate1 <- Ref.read mUpdate1Ref
      let update1 = maybe mempty identity mUpdate1
      update1 (Changed None a)
      update2 cha
      updateParent cha
    liftEffect $ Ref.write (Just update1) mUpdate1Ref
    update2 <- unwrapPropagator c2 \cha@(Changed _ a) -> do
      mUpdate2 <- Ref.read mUpdate2Ref
      let update2 = maybe mempty identity mUpdate2
      update2 (Changed None a)
      update1 cha
      updateParent cha
    liftEffect $ Ref.write (Just update2) mUpdate2Ref
    pure $ update1 <> update2

instance MonadEffect m => ProfunctorZero (Propagator m) where
  prozero = Propagator \_ -> pure mempty

instance Monad m => ChProfunctor (Propagator m) where
  chmap mapin mapout w = Propagator \outward -> do
    update <- unwrapPropagator w \(Changed c a) -> do
      outward $ Changed (mapout c) a
    pure \(Changed c a) -> update $ Changed (mapin c) a

instance MonadEffect m => Semigroupoid (Propagator m) where
  compose w2 w1 = Propagator \outward -> do
    update2Ref <- liftEffect $ Ref.new $ unsafeCoerce unit
    update1 <- unwrapPropagator w1 \(Changed _ b) -> do
      update2 <- Ref.read update2Ref
      update2 $ Changed Some b -- we force w2 to update cause w2 is updated only via outward by w1
    update2 <- unwrapPropagator w2 outward
    liftEffect $ Ref.write update2 update2Ref
    pure update1

class ProductProfunctor p where
  purePP :: forall a b. b -> p a b

instance Monad m => ProductProfunctor (Propagator m) where
  purePP b = Propagator \outward -> pure case _ of
    Changed None _ -> pure unit
    _ -> outward (Changed Some b)

effect :: forall i o m. MonadEffect m => (i -> Effect Unit) -> Propagator m i o
effect f = Propagator \_ -> pure \(Changed _ a) -> liftEffect $ f a -- outward is never called

-- Makes `Widget a b` fixed on `a` - no matter what `s` from the context of `Widget s t` is, so the `s`s are not listened to at all
fixed :: forall a b s t m. MonadEffect m => a -> Propagator m a b -> Propagator m s t
fixed a w = Propagator \_ -> do
  update <- unwrapPropagator w mempty
  liftEffect $ update $ Changed Some a
  pure mempty

hush :: forall a b c m. Propagator m a b -> Propagator m a c
hush w = Propagator \_ -> unwrapPropagator w mempty -- outward is never called
