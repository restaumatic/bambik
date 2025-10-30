module Data.Profunctor.IntroVarO where

import Prelude

import Data.Either (Either(..))
import Data.Lens (Optic)
import Data.Newtype (wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Profunctor.Cont (Cont)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, expand, inj)
import Prim.Row (class Cons, class Union)
import Type.Proxy (Proxy(..))

class Profunctor p <= IntroVarP p where
  liftIntroVar :: forall s i. p s i -> p s (Either s i) -- i introduced, s preserved

-- `forall p. IntroVarP p => Optic p s t s i` encodes `Either s i -> t`

introVar :: forall s t i. (Either s i -> t) -> (forall p. IntroVarP p => Optic p s t s i)
introVar introduce = liftIntroVar >>> rmap introduce

-- TODO: introVarInv

introVar' :: forall p @l t s i r. IsSymbol l => Cons l i s t => Union s r t => IntroVarP p => Optic p (Variant s) (Variant t) (Variant s) i
introVar' = introVar (\sori -> case sori of
  Left vars -> expand vars
  Right i -> inj (Proxy @l) i)

introVar'' :: forall p @l t s i r. IsSymbol l => Cons l i s t => Union s r t => IntroVarP p => (Variant s -> i) -> Optic p (Variant s) (Variant t) i i
introVar'' default = lcmap default >>> introVar (\sori -> case sori of
  Left vars -> expand vars
  Right i -> inj (Proxy @l) i)


instance IntroVarP (->) where
  liftIntroVar f s = Right (f s)

instance IntroVarP (Cont r) where
  -- :: [(b -> r) -> (Unit -> r)] -> (Either b s -> r) -> s -> r
  -- s may be Void
  liftIntroVar _ = wrap \bs2r s -> bs2r (Left s)
