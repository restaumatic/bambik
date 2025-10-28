module Data.Profunctor.IntroVarO where

import Prelude

import Data.Either (Either(..))
import Data.Lens (Optic)
import Data.Newtype (wrap)
import Data.Profunctor (class Profunctor, rmap)
import Data.Profunctor.Cont (Cont)

class Profunctor p <= IntroVarP p where
  liftIntroVar :: forall s a. p s a -> p s (Either s a) -- s must not be Void

-- `IntroVarO s t a` encodes `Either s a -> t`
type IntroVarO s t a = forall p. IntroVarP p => Optic p s t s a

introVar :: forall s t a. (Either s a -> t) -> IntroVarO s t a
introVar introduce = liftIntroVar >>> rmap introduce

instance IntroVarP (Cont r) where
  -- :: [(b -> r) -> (Unit -> r)] -> (Either b s -> r) -> s -> r
  -- s may be Void
  liftIntroVar r = wrap \bs2r s -> bs2r (Left s)
