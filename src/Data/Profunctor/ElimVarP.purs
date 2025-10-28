module Data.Profunctor.ElimVarP where

import Prelude

import Data.Either (Either(..), either)
import Data.Lens (Optic)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Profunctor.Cont (Cont(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, on)
import Prim.Row (class Cons, class Lacks)
import Type.Proxy (Proxy(..))

class Profunctor p <= ElimVarP p where
  liftElimVar :: forall s e. p e s -> p (Either e s) s -- e eliminated, s preserved

-- ElimVarP is not a subclass of Choice:
-- choiceLikeToChoice :: forall p. Profunctor p => (forall a t. p a t -> p (Either a t) t)) -> (forall a b c. p a b -> p (Either a c) (Either b c))
-- choiceLikeToChoice = impossible
-- ElimVarP is a superclass of Choice:
-- choiceToChoiceLike :: forall p. Profunctor p => (forall s b. a b c -> p (Either a c) (Either b c)) -> (forall a t. p a t -> p (Either a t) t)
-- choiceToChoiceLike = TODO...

-- Half-prism (a.k.a. eliminator) is similar to a prism but it only eliminates a variant, so it's only one function: `s -> Either a t`
-- Half-prism does not encode a full prism (a constructor in particular) as it does not allow to set variant b of t.

-- `ElimVarO s t e` encodes `s -> Either e t`
type ElimVarO s t e = forall p. ElimVarP p => Optic p s t e t

elimVar :: forall s t e. (s -> Either e t) -> ElimVarO s t e
elimVar eliminate = liftElimVar >>> lcmap eliminate

elimVarInv :: forall s t e. ElimVarO s t e -> s -> Either e t
elimVarInv f = unwrap (f (Cont (const Left))) Right

elimVar' :: forall @l s t a. IsSymbol l => Cons l a t s => Lacks l t => ElimVarO (Variant s) (Variant t) a
elimVar' = elimVar (on (Proxy @l) Left Right)

instance ElimVarP (->) where
  liftElimVar f = either f identity

-- Useful instance for decoding half-prisms
instance ElimVarP (Cont r) where
  liftElimVar r = wrap $ \t2r aort -> either (unwrap r t2r) t2r aort

