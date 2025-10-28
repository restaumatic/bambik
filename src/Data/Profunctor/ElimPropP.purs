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
  liftElimProp :: forall s e. p e s -> p (Tuple e s) s -- e eliminated, s preserved

-- TODO: relation with Strong

-- `ElimPropO s t e` encodes `s -> Tuple e t`
type ElimPropO s t e = forall p. ElimPropP p => Optic p s t e t

elimProp :: forall s t e. (s -> Tuple e t) -> ElimPropO s t e
elimProp eliminate = liftElimProp >>> lcmap eliminate

-- TODO: elimPropInv

elimProp' :: forall @l t s a. IsSymbol l => Cons l a s t => Lacks l s => ElimPropO (Record t) (Record s) a
elimProp' = elimProp (\s -> Tuple (get (Proxy @l) s) (delete (Proxy @l) s))

instance ElimPropP (->) where
  liftElimProp f (Tuple a _) = f a

instance ElimPropP (Cont r) where
  liftElimProp r = wrap \s2r (Tuple a s) -> s2r s
