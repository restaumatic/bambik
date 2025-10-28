module Data.Profunctor.IntroVarO where

import Prelude

import Data.Either (Either(..))
import Data.Lens (Optic)
import Data.Newtype (wrap)
import Data.Profunctor (class Profunctor, rmap)
import Data.Profunctor.Cont (Cont)
import Web (i)

class Profunctor p <= IntroVarP p where
  liftIntroVar :: forall s i. p s i -> p s (Either s i) -- i introduced, s preserved

-- `IntroVarO s t i` encodes `Either s i -> t`
type IntroVarO s t i = forall p. IntroVarP p => Optic p s t s i

introVar :: forall s t i. (Either s i -> t) -> IntroVarO s t i  
introVar introduce = liftIntroVar >>> rmap introduce

instance IntroVarP (Cont r) where
  -- :: [(b -> r) -> (Unit -> r)] -> (Either b s -> r) -> s -> r
  -- s may be Void
  liftIntroVar r = wrap \bs2r s -> bs2r (Left s)
