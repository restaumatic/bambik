module SafePropagator where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.State (gets)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Propagator (class Plus, Change(..), Occurrence(..), Propagator)
import Unsafe.Coerce (unsafeCoerce)
import Web.Internal.DOM (setTextNodeValue)
import Web.Internal.DOMBuilder (DOMBuilder)
import Web.Internal.DOMBuilder as Web.Internal.DOMBuilder

newtype SafePropagator m i o = SafePropagator (m
  { speak :: Occurrence (Maybe i) -> m Unit
  , listen :: (Occurrence o -> m Unit) -> m Unit
  })

derive instance Newtype (SafePropagator m i o) _

-- SafePropagator is a special case of Propagator:
safePropagator :: forall m i o. Monad m => SafePropagator m i o -> Propagator m i o
safePropagator p  = wrap \propagationo -> do
  {speak, listen} <- unwrap p
  listen propagationo
  pure (map Just >>> speak)

preview :: forall m i o. Functor m => SafePropagator m i o -> m Unit
preview p = void $ unwrap p
-- notice: this is not possible with Propagator

view :: forall m i o. Monad m => SafePropagator m i o -> i -> m Unit
view p i = do
  { speak, listen } <- unwrap p
  listen (const $ pure unit)
  speak (Occurrence Some (Just i))

instance Functor m => Profunctor (SafePropagator m) where
  dimap contraf cof p = wrap $ unwrap p <#> \p' ->
    { speak: (_ <<< map (map contraf)) $ p'.speak
    , listen: p'.listen <<< lcmap (map cof)
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
          for_ prevmab \prevab -> propagationab (map (flip Tuple (snd prevab)) oa )
      }
  second p = unsafeCoerce unit

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
          propagationab (Left <$> oa)
      }
  right p = unsafeCoerce unit

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
          p2'.speak (Just <$> o)
          propagation o
          in unit
        p2'.listen \o -> ado
          p1'.speak (Just <$> o)
          propagation o
          in unit
        in unit
      }
-- compare to: instance MonadEffect m => Semigroup (Propagator m a a) where

instance Monad m => Semigroupoid (SafePropagator m) where
  compose p2 p1 = wrap do
    p1' <- unwrap p1
    p2' <- unwrap p2
    p1'.listen \o -> p2'.speak (Just <$> o)
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
    , listen: p'.listen <<< lcmap (map f)
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
