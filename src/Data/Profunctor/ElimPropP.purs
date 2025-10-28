module Data.Profunctor.ElimPropP where

import Prelude

import Data.Lens (Optic)
import Data.Newtype (wrap)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Profunctor.Cont (Cont)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons, class Lacks)
import Record (delete, get)
import Type.Prelude (Proxy(..))

class Profunctor p <= ElimPropP p where
  liftElimProp :: forall s a. p a s -> p (Tuple a s) s -- a eliminated, s preserved

-- TODO: relation with Strong

-- `ElimPropO s t a` encodes `s -> Tuple a t`
type ElimPropO s t a = forall p. ElimPropP p => Optic p s t a t

elimProp :: forall s t a. (s -> Tuple a t) -> ElimPropO s t a
elimProp eliminate = liftElimProp >>> lcmap eliminate

-- TODO: inverse
-- eliminatePropertyInverse :: forall s t a. ElimPropO s t a -> s -> Tuple a t
-- eliminatePropertyInverse f s = ...

elimProp' :: forall @l t s a. IsSymbol l => Cons l a s t => Lacks l s => ElimPropO (Record t) (Record s) a
elimProp' = elimProp (\s -> Tuple (get (Proxy @l) s) (delete (Proxy @l) s))

instance ElimPropP (->) where
  liftElimProp f (Tuple a _) = f a

instance ElimPropP (Cont r) where
  liftElimProp r = wrap \s2r (Tuple a s) -> s2r s
