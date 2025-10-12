module Data.Profunctor.ChoiceLike where

import Prelude

import Data.Either (Either(..), either)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap)

-- ChoiceLike

class Profunctor p <= ChoiceLike p where
  leftlike :: forall t a. p a Void -> p (Either a t) t -- a eliminated, t preserved
  rightlike :: forall t a. p a Void -> p (Either t a) t -- a eliminated, t preserved

-- Is ChoiceLike a generalization of Choice?

-- Half-prism (a.k.a. eliminator) is similar to a prism but it only eliminates a variant, so it's only one function: `s -> Either a t`

-- ChoiceLike is enough to encode a half-prism as there is an isomorphism between:
-- `s -> Either a t` and `forall ChoiceLike p. p a Void -> p s t`.

halfprism :: forall s t a. (s -> Either a t) -> (forall p. ChoiceLike p => p a Void -> p s t)
halfprism eliminate = leftlike >>> lcmap eliminate

-- Does it have any sense/application?
halfprism' :: forall t a. (t -> Either a t) ->(forall p. ChoiceLike p => p a Void -> p t t)
halfprism' = halfprism

-- Half-prism does not encode a full prism (a constructor in particular) as it does not allow to set variant b of t.

-- Useful ChoiceLike instance for decoding half-prisms
-- Add to profunctors package? 
newtype Callback r a b = Callback ((b -> r) -> (a -> r))

derive instance Newtype (Callback r a b) _

instance Profunctor (Callback r) where
  dimap f g r = wrap \br -> (unwrap r) (br <<< g) <<< f

instance ChoiceLike (Callback r) where
  leftlike r = wrap $ either (unwrap r absurd)
  rightlike r = wrap $ flip either (unwrap r absurd)

halfprismDecode :: forall s t a. (forall p. ChoiceLike p => p a Void -> p s t) -> s -> Either a t
halfprismDecode f = unwrap (f (Callback (const Left))) Right
