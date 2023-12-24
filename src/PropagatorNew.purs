module PropagatorNew where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Internal as ST
import Control.Monad.State (gets)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)
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

instance Monad m => Profunctor (SafePropagator m) where
  dimap contraf cof p = wrap do
    p' <- unwrap p
    pure
      { speak: (_ <<< map contraf) $ p'.speak
      , listen: p'.listen <<< lcmap (map cof)
      }

instance MonadST Global m => Strong (SafePropagator m) where
  first p = wrap do
    bref <- liftST $ ST.new (unsafeCoerce unit)
    p' <- unwrap p
    pure
      { speak: \ab -> do
        void $ liftST $ ST.write (map snd ab) bref
        p'.speak (map fst ab)
      , listen: \propagationab -> do
        (Occurrence _ b) <- liftST $ ST.read bref
        p'.listen \a -> propagationab (map (flip Tuple b) a )
      }
  second p = wrap do
    aref <- liftST $ ST.new (unsafeCoerce unit)
    p' <- unwrap p
    pure
      { speak: \ab -> do
        void $ liftST $ ST.write (map fst ab) aref
        p'.speak (map snd ab)
      , listen: \propagationab -> do
        (Occurrence _ a) <-liftST $ ST.read aref
        p'.listen \b -> propagationab (map (Tuple a) b)
      }

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
