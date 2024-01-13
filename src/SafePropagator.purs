module SafePropagator
  ( SafePropagator(..)
  , bracket
  , fixed
  , preview
  , scopemap
  , view
  )
  where

import Prelude

import Control.Alt (class Alt)
import Data.Array (uncons)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (class MonadEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Propagator (class Plus, Change(..), Occurrence(..), Propagator, Scope(..))
import Unsafe.Coerce (unsafeCoerce)

newtype SafePropagator m i o = SafePropagator (m
  { speak :: Occurrence (Maybe i) -> m Unit
  , listen :: (Occurrence (Maybe o) -> m Unit) -> m Unit
  })

derive instance Newtype (SafePropagator m i o) _

-- SafePropagator is a generalization of Propagator:
downcast :: forall m i o. Monad m => SafePropagator m i o -> Propagator m i o
downcast p  = wrap \propagationo -> do
  {speak, listen} <- unwrap p
  listen case _ of
    Occurrence _ Nothing -> pure unit
    occurence@(Occurrence _ (Just o)) -> propagationo (occurence $> o)
  pure (map Just >>> speak)

upcast :: forall m i o. Monad m => Propagator m i o -> SafePropagator m i o
upcast p  = unsafeThrow "not implemented" -- TODO

preview :: forall m i o. Monad m => SafePropagator m i o -> m Unit
preview p = do
  { speak, listen } <- unwrap p
  listen (const $ pure unit)
  speak (Occurrence Some Nothing)
-- notice: this is not possible with Propagator

view :: forall m i o. Monad m => SafePropagator m i o -> i -> m Unit
view p i = do
  { speak, listen } <- unwrap p
  listen (const $ pure unit)
  speak (Occurrence Some (Just i))

instance Functor m => Profunctor (SafePropagator m) where
  dimap contraf cof p = wrap $ unwrap p <#> \p' ->
    { speak: (_ <<< map (map contraf)) $ p'.speak
    , listen: p'.listen <<< lcmap (map (map cof))
    }

instance Applicative m => Strong (SafePropagator m) where
  first p = wrap ado
    let lastomab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: \omab -> do
        let _ = unsafePerformEffect $ Ref.write omab lastomab
        case omab of
          Occurrence None _ -> pure unit -- should never happen
          _ -> p'.speak (map (map fst) omab)
      , listen: \propagationab -> do
        p'.listen \oa -> do
          let (Occurrence _ prevmab) = unsafePerformEffect $ Ref.read lastomab
          for_ prevmab \prevab -> propagationab (map (map (flip Tuple (snd prevab))) oa)
      }
  second p = wrap ado
    let lastomab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: \omab -> do
        let _ = unsafePerformEffect $ Ref.write omab lastomab
        case omab of
          Occurrence None _ -> pure unit -- should never happen
          _ -> p'.speak (map (map snd) omab)
      , listen: \propagationab -> do
        p'.listen \oa -> do
          let (Occurrence _ prevmab) = unsafePerformEffect $ Ref.read lastomab
          for_ prevmab \prevab -> propagationab (map (map (Tuple (fst prevab))) oa )
      }

instance Applicative m => Choice (SafePropagator m) where
  left p = wrap ado
    let lastomab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: \omab -> do
        let _ = unsafePerformEffect $ Ref.write omab lastomab
        case omab of
          Occurrence None _ -> pure unit
          Occurrence _ Nothing -> p'.speak $ omab $> Nothing
          Occurrence _ (Just (Right _)) -> p'.speak $ omab $> Nothing
          Occurrence _ (Just (Left a)) -> p'.speak $ omab $> Just a
      , listen: \propagationab -> do
        p'.listen \oa -> do -- should never happen
          -- prevoab <- liftST $ ST.read lastomab -- TODO what to do with previous occurence? it could contain info about the change of a or b
          propagationab (map Left <$> oa)
      }
  right p = wrap ado
    let lastomab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: \omab -> do
        let _ = unsafePerformEffect $ Ref.write omab lastomab
        case omab of
          Occurrence None _ -> pure unit
          Occurrence _ Nothing -> p'.speak $ omab $> Nothing
          Occurrence _ (Just (Left _)) -> p'.speak $ omab $> Nothing
          Occurrence _ (Just (Right a)) -> p'.speak $ omab $> Just a
      , listen: \propagationab -> do
        p'.listen \oa -> do -- should never happen
          -- prevoab <- liftST $ ST.read lastomab -- TODO what to do with previous occurence? it could contain info about the change of a or b
          propagationab (map Right <$> oa)
      }

instance Apply m => Semigroup (SafePropagator m a a) where
  append p1 p2 = wrap ado
    p1' <- unwrap p1
    p2' <- unwrap p2
    in
      { speak: \o -> ado
        p1'.speak o
        p2'.speak o
        in unit
      , listen: \propagation -> ado
        p1'.listen \o -> ado
          p2'.speak o
          propagation o
          in unit
        p2'.listen \o -> ado
          p1'.speak o
          propagation o
          in unit
        in unit
      }
-- compare to: instance MonadEffect m => Semigroup (Propagator m a a) where

instance Monad m => Semigroupoid (SafePropagator m) where
  compose p2 p1 = wrap do
    p1' <- unwrap p1
    p2' <- unwrap p2
    p1'.listen \o -> p2'.speak o -- TODO what does it mean if o is Nothing?
    pure
      { speak: p1'.speak -- TODO call p2.speak Nothing?
      , listen: p2'.listen
      }
-- compare to: instance MonadEffect m => Semigroupoid (Propagator m) where

-- impossible:
-- instance Monad m => Category (SafePropagator m) where
--   identity = wrap $ pure
--     { speak: unsafeThrow "impossible"
--     , listen: unsafeThrow "impossible"
--     }

instance Functor m => Functor (SafePropagator m a) where
  map f p = wrap $ unwrap p <#> \p' ->
    { speak: p'.speak
    , listen: p'.listen <<< lcmap (map (map f))
    }

instance Apply m => Alt (SafePropagator m a) where
  alt p1 p2 = wrap ado
    p1' <- unwrap p1
    p2' <- unwrap p2
    in
      { speak: p1'.speak *> p2'.speak
      , listen: \propagation -> ado
        p1'.listen propagation
        p2'.listen propagation
        in unit
      }

instance Applicative m => Plus (SafePropagator m a) where
  empty = wrap $ pure
    { speak: const $ pure unit
    , listen: const $ pure unit
    }

bracket :: forall m c i o i' o'. MonadEffect m => m c -> (c -> Occurrence (Maybe i') -> m (Occurrence (Maybe i))) -> (c -> Occurrence (Maybe o) -> m (Occurrence (Maybe o'))) -> SafePropagator m i o -> SafePropagator m i' o'
bracket afterInit afterInward beforeOutward w = wrap do
  w' <- unwrap w
  ctx <- afterInit
  pure
    { speak: \occur -> do
      occur' <- afterInward ctx occur
      w'.speak occur'
    , listen: \prop -> do
      w'.listen \occur -> do
        occur' <- beforeOutward ctx occur
        prop occur'
      }

fixed :: forall m a b s t. MonadEffect m => a -> SafePropagator m a b -> SafePropagator m s t
fixed a w = wrap do
  w' <- unwrap w
  w'.speak (Occurrence Some (Just a))
  pure
    { speak: const $ pure unit
    , listen: const $ pure unit
    }

scopemap :: forall m a b. Applicative m => Scope -> SafePropagator m a b -> SafePropagator m a b
scopemap scope p = wrap ado
  { speak, listen } <- unwrap p
  in
    { speak: \(Occurrence c a) -> speak $ Occurrence (zoomIn c) a
    , listen: \prop -> do
      listen \(Occurrence c a) -> prop $ Occurrence (zoomIn c) a
    }
  where
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
