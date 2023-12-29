module SafePropagator where

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
import Effect.Class (liftEffect)
import Propagator (class Plus, Change(..), Occurrence(..), Propagator(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Internal.DOM (setTextNodeValue)
import Web.Internal.DOMBuilder (DOMBuilder)
import Web.Internal.DOMBuilder as Web.Internal.DOMBuilder

newtype SafePropagator m i o = SafePropagator (m
  { attach :: m Unit
  , detach :: m Unit
  , speak :: Occurrence i -> m Unit
  , listen :: (Occurrence o -> m Unit) -> m Unit
  })

derive instance Newtype (SafePropagator m i o) _

-- SafePropagator is a special case of Propagator:
safePropagator :: forall m i o. Monad m => SafePropagator m i o -> Propagator m i o
safePropagator p  = wrap \propagationo -> do
  {speak, listen} <- unwrap p
  listen propagationo
  pure speak

preview :: forall m i o. Functor m => SafePropagator m i o -> m Unit
preview p = void $ unwrap p
-- notice: this is not possible with Propagator

view :: forall m i o. Monad m => SafePropagator m i o -> i -> m Unit
view p i = do
  { attach, speak, listen } <- unwrap p
  listen (const $ pure unit)
  attach
  speak (Occurrence Some i)

instance Monad m => Profunctor (SafePropagator m) where
  dimap contraf cof p = wrap do
    p' <- unwrap p
    pure
      { attach: p'.attach
      , detach: p'.detach
      , speak: (_ <<< map contraf) $ p'.speak
      , listen: p'.listen <<< lcmap (map cof)
      }

instance MonadST Global m => Strong (SafePropagator m) where
  first p = wrap do
    lastoab <- liftST $ ST.new (unsafeCoerce unit)
    p' <- unwrap p
    pure
      { attach: p'.attach
      , detach: p'.detach
      , speak: \oab -> do
        void $ liftST $ ST.write oab lastoab
        case oab of
          Occurrence None _ -> pure unit
          _ -> p'.speak (map fst oab)
      , listen: \propagationab -> do
        p'.listen \oa -> do
          (Occurrence _ prevab) <- liftST $ ST.read lastoab
          propagationab (map (flip Tuple (snd prevab)) oa )
      }
  second p = unsafeCoerce unit

instance MonadST Global m => Choice (SafePropagator m) where
  left p = wrap do
    lastoab <- liftST $ ST.new (unsafeCoerce unit)
    p' <- unwrap p
    pure
      { attach: p'.detach
      , detach: p'.attach
      , speak: \oab -> do
        void $ liftST $ ST.write oab lastoab
        case oab of
          Occurrence None _ -> pure unit
          Occurrence _ (Left a) -> p'.speak $ oab $> a
          Occurrence _ (Right _) -> p'.detach
      , listen: \propagationab -> do
        p'.listen \oa -> do
          prevoab <- liftST $ ST.read lastoab -- TODO what to do with previous occurence? it could contain info about the change of a or b
          propagationab (Left <$> oa)
      }
  right p = unsafeCoerce unit

instance Monad m => Semigroup (SafePropagator m a a) where
  append p1 p2 = wrap do
    p1' <- unwrap p1
    p2' <- unwrap p2
    pure
      { attach: p1'.attach *> p2'.attach
      , detach: p1'.detach *> p2'.detach
      , speak: p1'.speak *> p2'.speak
      , listen: \propagation -> do
        p1'.listen $ p2'.speak *> propagation
        p2'.listen $ p1'.speak *> propagation
      }
-- compare to: instance MonadEffect m => Semigroup (Propagator m a a) where

instance Monad m => Semigroupoid (SafePropagator m) where
  compose p2 p1 = wrap do
    p1' <- unwrap p1
    p2' <- unwrap p2
    p1'.listen p2'.speak
    pure
      { attach: p1'.attach
      , detach: p1'.detach
      , speak: p1'.speak
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
    { attach: p'.attach
    , detach: p'.detach
    , speak: p'.speak
    , listen: p'.listen <<< lcmap (map f)
    }

instance Apply m => Alt (SafePropagator m a) where
  alt p1 p2 = wrap ado
    p1' <- unwrap p1
    p2' <- unwrap p2
    in
      { attach: p1'.attach *> p2'.attach
      , detach: p1'.detach *> p2'.detach
      , speak: p1'.speak *> p2'.speak
      , listen: \propagation -> ado
        p1'.listen propagation
        p2'.listen propagation
        in unit
      }

instance Applicative m => Plus (SafePropagator m a) where
  empty = wrap $ pure
    { attach: pure unit
    , detach: pure unit
    , speak: const $ pure unit
    , listen: const $ pure unit
    }

--

type SafeWidget i o = SafePropagator DOMBuilder i o

text :: SafeWidget String Void
text = SafePropagator do
  Web.Internal.DOMBuilder.text
  node <- gets (_.sibling)
  pure
    { attach: pure unit --TODO
    , detach: pure unit --TODO
    , speak: case _ of
      Occurrence None _ -> pure unit
      Occurrence _ string -> liftEffect $ setTextNodeValue node string
    , listen: \_ -> pure unit
    }
