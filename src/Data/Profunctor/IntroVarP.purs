module Data.Profunctor.IntroVarO where

import Prelude

import Data.Either (Either(..), either)
import Data.Lens (Optic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, expand, inj)
import Prim.Row (class Cons, class Union)
import Type.Proxy (Proxy(..))

class Profunctor p <= IntroVarP p where
  liftIntroVar :: forall s r. p Void r -> p s (Either s r) -- r read, s preserved

-- TODO: check IntroVarP relation to Choice

-- useful IntroVarP instance
newtype IntroVar r a b = IntroVar (Either a r -> b)

derive instance Newtype (IntroVar r a b) _

instance Profunctor (IntroVar r) where
  dimap f g h = wrap \ear -> g (unwrap h (either (Left <<< f) Right ear))

instance IntroVarP (IntroVar r) where
  liftIntroVar h = wrap $ either Left (Right <<< unwrap h <<< Right)

-- `forall p. IntroVarP p => Optic p s t Void r` encodes `Either s r -> t` using `instance IntroVarP (IntroVar r)`:
introVar :: forall s t r. (Either s r -> t) -> (forall p. IntroVarP p => Optic p s t Void r)
introVar f = liftIntroVar >>> rmap f

introVarInv :: forall s t r. (forall p. IntroVarP p => Optic p s t Void r) -> (Either s r -> t)
introVarInv optic = unwrap (optic (IntroVar (either absurd identity)))

pick :: forall p @l t s r rest. IsSymbol l => Cons l r s t => Union s rest t => IntroVarP p => Optic p (Variant s) (Variant t) (Variant ()) r
pick = lcmap absurd >>> introVar (\sori -> case sori of
  Left vars -> expand vars
  Right i -> inj (Proxy @l) i)
