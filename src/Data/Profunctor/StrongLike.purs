module Data.Profunctor.StrongLike where

import Prelude

import Data.Either (Either(..), either)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Tuple (Tuple(..))

-- StrongLike

class Profunctor p <= StrongLike p where
  firstlike :: forall s b . p Unit b -> p s (Tuple b s) -- b introduced, s preserved
  secondlike :: forall s b . p Unit b -> p s (Tuple s b) -- b introduced, s preserved

-- Is StrongLike a generalization of Strong?
-- can we turn:
-- `forall s b . p Unit b -> p s (Tuple b s)`
-- into:
-- forall s b . p a b -> p (Tuple a s) (Tuple b s)
-- ?

-- StrongLike is enough to encode a half-lens (a.k.a. introductor) as there is an isomorphism between
-- `Tuple b s -> t` and `forall StrongLike p. p Unit b -> p s t`.

halflens :: forall s t b. (Tuple b s -> t) -> (forall p. StrongLike p => p Unit b -> p s t)
halflens introduce = firstlike >>> rmap introduce

-- Does it have any sense/application?
halflens' :: forall s b. (Tuple b s -> s) -> (forall p. StrongLike p => p Unit b -> p s s)
halflens' = halflens

-- Useful StringLike instance for decoding half-lenses
-- Add to profunctors package? 
instance StrongLike (->) where
  firstlike f s = Tuple (f unit) s
  secondlike f s = Tuple s (f unit)

halflensDecode :: forall s t b. (forall p. StrongLike p => p Unit b -> p s t) -> Tuple b s -> t
halflensDecode f (Tuple b s) = f (const b) s

-- ChoiceLike

class Profunctor p <= ChoiceLike p where
  leftlike :: forall t a. p a Void -> p (Either a t) t -- a eliminated, t preserved
  rightlike :: forall t a. p a Void -> p (Either t a) t -- a eliminated, t preserved

-- Is ChoiceLike a generalization of Choice?

-- ChoiceLike is enough to encode a half-prism (a.k.a. eliminator) as there is an isomorphism between:
-- `s -> Either a t` and `forall ChoiceLike p. p a Void -> p s t`.

halfprism :: forall s t a. (s -> Either a t) -> (forall p. ChoiceLike p => p a Void -> p s t)
halfprism eliminate = leftlike >>> lcmap eliminate

-- Does it have any sense/application?
halfprism' :: forall t a. (t -> Either a t) ->(forall p. ChoiceLike p => p a Void -> p t t)
halfprism' = halfprism 

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
