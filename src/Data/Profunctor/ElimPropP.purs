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

-- `forall p. ElimPropP p => Optic p s t e t` encodes `s -> Tuple e t`

elimProp :: forall s t e. (s -> Tuple e t) -> (forall p. ElimPropP p => Optic p s t e t)
elimProp eliminate = liftElimProp >>> lcmap eliminate

-- TODO: elimPropInv

elimProp' :: forall @l t s e. IsSymbol l => Cons l e t s => Lacks l t => (forall p. ElimPropP p => Optic p (Record s) (Record t) e (Record t))
elimProp' = elimProp (\s -> Tuple (get (Proxy @l) s) (delete (Proxy @l) s))

instance ElimPropP (->) where
  liftElimProp f (Tuple a _) = f a

instance ElimPropP (Cont r) where
  liftElimProp r = wrap \s2r (Tuple a s) -> s2r s
