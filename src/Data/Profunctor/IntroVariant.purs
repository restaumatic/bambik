module Data.Profunctor.IntroVariant where

import Prelude

import Data.Either (Either(..))
import Data.Lens (Optic)
import Data.Newtype (wrap)
import Data.Profunctor (class Profunctor, rmap)
import Data.Profunctor.Cont (Cont)

-- IntroVariantP

class Profunctor p <= IntroVariantP p where
  introVariantP :: forall s b. p Unit b -> p s (Either b s) -- s must not be Void

type IntroVariant s t b = forall p. IntroVariantP p => Optic p s t Unit b

introVariant :: forall s t b. (Either b s -> t) -> IntroVariant s t b
introVariant introduce = introVariantP >>> rmap introduce

instance IntroVariantP (Cont r) where
  -- :: [(b -> r) -> (Unit -> r)] -> (Either b s -> r) -> s -> r
  -- s may be Void
  introVariantP r = wrap \bs2r s -> bs2r (Right s)
