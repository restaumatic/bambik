module PropagatorNew where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Internal as ST
import Control.Monad.State (gets)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class.Console (debug)
import Propagator (class Plus, Change(..), Occurrence(..), Propagation, Propagator(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Internal.DOM (setTextNodeValue)
import Web.Internal.DOMBuilder (DOMBuilder)
import Web.Internal.DOMBuilder as Web.Internal.DOMBuilder

newtype SafePropagator m i o = SafePropagator (m
  { speak :: Propagation i
  , listen :: Propagation o -> m Unit
  })

derive instance Newtype (SafePropagator m i o) _

safePropagator :: forall m i o. Monad m => SafePropagator m i o -> Propagator m i o
safePropagator (SafePropagator p) = Propagator \propagationo -> do
  {speak, listen} <- p
  listen propagationo
  pure speak

preview :: forall m i o. Functor m => SafePropagator m i o -> m Unit
preview p = void $ unwrap p
-- notice: this is not possible with Propagator

view :: forall m i o. Monad m => SafePropagator m i o -> m (i -> Effect Unit)
view p = do
  { speak, listen } <- unwrap p
  listen (\(Occurrence ch _) -> debug $ show ch)
  pure \i -> speak (Occurrence Some i)

instance Monad m => Profunctor (SafePropagator m) where
  dimap contraf cof p = wrap do
    p' <- unwrap p
    pure
      { speak: (_ <<< map contraf) $ p'.speak
      , listen: p'.listen <<< lcmap (map cof)
      }

instance MonadST Global m => Strong (SafePropagator m) where
  first p = wrap do
    (oabref :: ST.STRef Global (Occurrence (Tuple _ _))) <- liftST $ ST.new (unsafeCoerce unit) -- last occurrence
    p' <- unwrap p
    pure
      { speak: \oab -> do
        void $ liftST $ ST.write oab oabref
        case oab of
          Occurrence None _ -> pure unit -- short-circuiting
          _ -> p'.speak (map fst oab)
      , listen: \propagationab -> do
        p'.listen \a -> do
          (Occurrence lastachange lastab) <- liftST $ ST.read oabref -- TODO what to do with last ab occurence? it contain info about the last change of a
          case lastachange of
            None -> pure unit -- last occurrence was without change, should never happen?
            Some -> propagationab (map (flip Tuple (snd lastab)) a ) -- last occurrence was with some change
            Scoped _ -> propagationab (map (flip Tuple (snd lastab)) a ) -- last occurence was with scoped change
      }
  second p = unsafeCoerce unit

instance MonadST Global m => Choice (SafePropagator m) where
  left p = wrap do
    (abref :: ST.STRef Global (Occurrence (Either _ _))) <- liftST $ ST.new (unsafeCoerce unit) -- last occurrence
    p' <- unwrap p
    pure
      { speak: \ab -> do
        lastab <- liftST $ ST.read abref -- TODO what to do with last occurence? notice: abref can be not initialized
        case ab of
          o@(Occurrence _ (Left a)) -> p'.speak $ o $> a
          (Occurrence _ (Right _)) -> pure unit -- TODO what should happen here? detach?
        void $ liftST $ ST.write ab abref
      , listen: \propagationab -> do
        p'.listen \a -> do
          ab <- liftST $ ST.read abref -- TODO what to do with last occurence? it could contain info about the change of a or b
          case ab of
            (Occurrence _ (Left _)) -> propagationab (map Left a)
            (Occurrence _ (Right _)) -> pure unit
      }
  right p = unsafeCoerce unit

instance Monad m => Semigroup (SafePropagator m a a) where
  append p1 p2 = wrap do
    p1' <- unwrap p1
    p2' <- unwrap p2
    pure
      { speak: p1'.speak <> p2'.speak
      , listen: \propagation -> do
        p1'.listen $ p2'.speak <> propagation
        p2'.listen $ p1'.speak <> propagation
      }
-- compare to: instance MonadEffect m => Semigroup (Propagator m a a) where

instance Monad m => Semigroupoid (SafePropagator m) where
  compose p2 p1 = wrap do
    p1' <- unwrap p1
    p2' <- unwrap p2
    p1'.listen p2'.speak
    pure
      { speak: p1'.speak
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
      { speak: p1'.speak <> p2'.speak
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

--

type SafeWidget i o = SafePropagator DOMBuilder i o

text :: SafeWidget String Void
text = SafePropagator do
  Web.Internal.DOMBuilder.text
  node <- gets (_.sibling)
  pure
    { speak: case _ of
      Occurrence None _ -> mempty
      Occurrence _ string -> setTextNodeValue node string
    , listen: \_ -> pure unit
    }
