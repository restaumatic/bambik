module Data.Profunctor.IntroPropP where

import Prelude

import Data.Lens (Optic)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Profunctor.Cont (Cont(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons, class Lacks)
import Record (insert)
import Type.Proxy (Proxy(..))

class Profunctor p <= IntroPropP p where
  liftIntroProp :: forall s i. p s i -> p s (Tuple s i) -- i introduced, s preserved

-- IntroPropP is not a subclass of Strong:
-- strongLikeToStrong :: (forall p s b. Profunctor p => p s b -> p s (Tuple b s)) -> (forall p a b c. Profunctor p => p a b -> p (Tuple a c) (Tuple b c))
-- strongLikeToStrong introProp pab = impossible (introProp pab :: p a (Tuple b a))
-- IntroPropP is a superclass of Strong:
strongToStrongLike :: forall p. Profunctor p => (forall a b c. p a b -> p (Tuple a c) (Tuple b c)) -> (forall s a. p s a -> p s (Tuple a s))
strongToStrongLike first = \psa -> lcmap (\s -> Tuple s s) (first psa)

-- `IntroPropO s t i` encodes `Tuple s i -> t`
type IntroPropO s t i = forall p. IntroPropP p => Optic p s t s i

introProp :: forall s t i. (Tuple s i -> t) -> IntroPropO s t i
introProp introduce = liftIntroProp >>> rmap introduce

introPropInv :: forall s t i. IntroPropO s t i -> Tuple i s -> t
introPropInv f (Tuple b s) = unwrap (f (Cont (\g _ -> g b))) identity s

introProp' :: forall @l t s a. IsSymbol l => Cons l a s t => Lacks l s => IntroPropO (Record s) (Record t) a
introProp' = introProp (\(Tuple s b) -> insert (Proxy @l) b s)

instance IntroPropP (->) where
  liftIntroProp f s = Tuple s (f s)

-- Useful instance for decoding half-lenses
instance IntroPropP (Cont r) where
  liftIntroProp callbacksb = wrap $ \bs2r s -> unwrap callbacksb (\b -> bs2r (Tuple s b)) s


