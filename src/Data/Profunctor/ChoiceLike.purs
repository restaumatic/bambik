module Data.Profunctor.ChoiceLike where

import Prelude

import Data.Either (Either(..), either)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Lens (Optic)
import Data.Profunctor.Cont (Cont(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, on)
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))

-- ChoiceLike

class Profunctor p <= ChoiceLike p where
  leftlike :: forall t a. p a Void -> p (Either a t) t -- a eliminated, t preserved
  rightlike :: forall t a. p a Void -> p (Either t a) t -- a eliminated, t preserved

-- Is ChoiceLike a generalization of Choice?

-- Half-prism (a.k.a. eliminator) is similar to a prism but it only eliminates a variant, so it's only one function: `s -> Either a t`
-- Half-prism does not encode a full prism (a constructor in particular) as it does not allow to set variant b of t.

type HalfPrism s t a = forall p. ChoiceLike p => Optic p s t a Void

-- ChoiceLike is enough to encode a half-prism as there is an isomorphism between:
-- `s -> Either a t` and `forall ChoiceLike p. p a Void -> p s t`.

halfprism :: forall s t a. (s -> Either a t) -> HalfPrism s t a
halfprism eliminate = leftlike >>> lcmap eliminate

-- Does it have any sense/application?
halfprism' :: forall t a. (t -> Either a t) -> HalfPrism t t a
halfprism' = halfprism

newVariant :: forall @l s t a. IsSymbol l => Cons l a t s => HalfPrism (Variant s) (Variant t) a
newVariant = halfprism (on (Proxy @l) Left Right)

-- There is no: `instance StrongLike (->)` as it would require (a -> Void)

-- Useful instance for decoding half-prisms
instance ChoiceLike (Cont r) where
  leftlike r = wrap $ either (unwrap r absurd)
  rightlike r = wrap $ flip either (unwrap r absurd)

halfprismDecode :: forall s t a. HalfPrism s t a -> s -> Either a t
halfprismDecode f = unwrap (f (Cont (const Left))) Right
