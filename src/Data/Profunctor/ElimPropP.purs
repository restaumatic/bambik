module Data.Profunctor.ElimPropP where

import Prelude

import Data.Lens (Optic)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Profunctor.Cont (Cont(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst)
import Effect.Exception.Unsafe (unsafeThrow)
import Prim.Row (class Cons, class Lacks)
import Record (delete, get)
import Type.Prelude (Proxy(..))

class Profunctor p <= ElimPropP p where
  liftElimProp :: forall s o. p o Unit -> p (Tuple s o) s -- o output, s preseved

-- `forall p. ElimPropP p => Optic p s t o Unit` encodes `s -> (Tuple t o)`?

elimProp :: forall s t o. (s -> Tuple t o) -> (forall p. ElimPropP p => Optic p s t o Unit)
elimProp eliminate = liftElimProp >>> lcmap eliminate

-- TODO:
elimPropInv :: forall s t o. (forall p. ElimPropP p => Optic p s t o Unit) -> (s -> Tuple t o)
elimPropInv f s = unsafeThrow "TODO"


output :: forall @l o s t. IsSymbol l => Cons l o t s => Lacks l t => (forall p. ElimPropP p => Optic p (Record s) (Record t) o (Record ()))
output = rmap (const unit) >>> elimProp \s -> Tuple (delete (Proxy @l) s) (get (Proxy @l) s)

instance ElimPropP (->) where
  liftElimProp _ (Tuple s _) = s

instance ElimPropP (Cont r) where
  liftElimProp _ =  wrap \f (Tuple s _) -> f s

-- ElimPropP is not a subclass of Strong
-- ElimPropP is a superclass of Strong:
strongToElimPropP :: forall p. Profunctor p => (forall a b c. p a b -> p (Tuple c a) (Tuple c b)) -> (forall s o. p o Unit -> p (Tuple s o) s)
strongToElimPropP second = second >>> rmap fst

-- utils

-- doesn't eliminate but reads
function :: forall p s a . ElimPropP p => (s -> a) -> Optic p s s a Unit
function f = elimProp \s -> Tuple s (f s)

-- doesn't eliminate but reads
constant :: forall p s a. ElimPropP p => a -> Optic p s s a Unit
constant a = function (const a)

-- so it's "user read" with `p a Unit`
