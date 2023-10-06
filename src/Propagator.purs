module Propagator
  ( Occurrence(..)
  , Propagation
  , Propagator(..)
  , attachable
  , bracket
  , class MonadGUI
  , class ProductProfunctor
  , fixed
  , followedByEffect
  , precededByEffect
  , hush
  , purePP
  )
  where

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
-- inward to perform only a subset of effects?) otherwise w1 ^ w2 for w1 and w2 doing so would cause propagation infinite loop
-- of mutual updates.

derive instance Newtype (Propagator m i o) _

-- Capabilites

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

instance MonadEffect m => ProfunctorPlus (Propagator m) where
  proplus c1 c2 = Propagator \updateParent -> do
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

instance MonadEffect m => ProfunctorZero (Propagator m) where
  prozero = Propagator \_ -> pure mempty

instance Monad m => ChProfunctor (Propagator m) where
  chmap mapin mapout w = Propagator \outward -> do
    inward <- unwrap w \(Occurrence c a) -> do
      outward $ Occurrence (mapout c) a
    pure \(Occurrence c a) -> inward $ Occurrence (mapin c) a

instance MonadEffect m => Semigroupoid (Propagator m) where
  compose w2 w1 = Propagator \outward -> do
    update2Ref <- liftEffect $ Ref.new $ unsafeCoerce unit
    inward1 <- unwrap w1 \(Occurrence _ b) -> do
      inward2 <- Ref.read update2Ref
      inward2 $ Occurrence Some b -- we force w2 to inward cause w2 is updated only via outward by w1
    inward2 <- unwrap w2 outward
    liftEffect $ Ref.write inward2 update2Ref
    pure inward1

class ProductProfunctor p where
  purePP :: forall a b. b -> p a b

instance Monad m => ProductProfunctor (Propagator m) where
  purePP b = Propagator \outward -> pure case _ of
    Occurrence None _ -> pure unit
    _ -> outward (Occurrence Some b)

precededByEffect ∷ ∀ (m ∷ Type -> Type) (i ∷ Type) (i' ∷ Type) (o ∷ Type). MonadEffect m ⇒ (i' → Effect i) → Propagator m i o → Propagator m i' o
precededByEffect f = bracket (pure unit) (\_ (Occurrence _ i') -> f i' <#> Occurrence Some) (const pure)

followedByEffect :: forall m i o o'. MonadEffect m => (o -> Effect o') -> Propagator m i o -> Propagator m i o'
followedByEffect f = bracket (pure unit) (const pure) (\_ (Occurrence _ o) -> f o <#> Occurrence Some)

-- Makes `Widget a b` fixed on `a` - no matter what `s` from the context of `Widget s t` is, so the `s`s are not listened to at all
fixed :: forall m a b s t. MonadEffect m => a -> Propagator m a b -> Propagator m s t
fixed a w = Propagator \_ -> do
  inward <- unwrap w mempty
  liftEffect $ inward $ Occurrence Some a
  pure mempty -- inward is never called again

-- Suppresses outward propagation
hush :: forall m a b c. Propagator m a b -> Propagator m a c
hush w = Propagator \_ -> unwrap w mempty -- outward is never called

bracket :: forall m c i o i' o'. MonadEffect m => m c -> (c -> Occurrence i' -> Effect (Occurrence i)) -> (c -> Occurrence o -> Effect (Occurrence o')) -> Propagator m i o -> Propagator m i' o'
bracket afterInit afterInward beforeOutward w = Propagator \outward -> do
  cRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  inward <- unwrap w \occurb -> do
    ctx <- Ref.read cRef
    o' <- beforeOutward ctx occurb
    outward o'
  ctx <- afterInit
  liftEffect $ Ref.write ctx cRef
  pure \occuri' -> do
    i <- afterInward ctx occuri'
    inward i
