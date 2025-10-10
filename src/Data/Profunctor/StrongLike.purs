module Data.Profunctor.StrongLike where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Profunctor.Choice (class Choice)
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe (unsafeThrow)

-- StrongLike

class Profunctor p <= StrongLike p where
  firstlike :: forall s b . p Unit b -> p s (Tuple b s) -- b introduced, s preserved

instance StrongLike (->) where
  firstlike f s = Tuple (f unit) s

-- what about dual `forall a b . p (Tuple a b) b -> p a b`? It's not trivial.

-- StrongLike is a generalization of Strong

-- instance Strong p => StrongLike p where
--   firstlike = lcmap (\a -> Tuple a a) <<< first
--   secondlike = lcmap (\a -> Tuple a a) <<< second

-- StrongLike is not isomorphic to Strong, because:

-- firstlikeToFirst :: forall p a b c. StrongLike p => p a b -> p (Tuple a c) (Tuple b c)
-- firstlikeToFirst pab = (firstlike pab :: p a (Tuple b a)) -- impossible to do: p a (Tuple b a) -> p (Tuple a c) (Tuple b c)

-- StrongLike is enough to encode lenses even though it's more generic and weaker than Strong.
-- Compare to `lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b` from `profunctor-lenses` library.

-- setter? introducer?
halflens :: forall s t b. (Tuple b s -> t) -> (forall p. StrongLike p => p Unit b -> p s t)
halflens set = firstlike >>> rmap set

halflensInv :: forall s t b. (forall p. StrongLike p => p Unit b -> p s t) -> Tuple b s -> t
halflensInv f (Tuple b s) = f (const b) s

-- fieldSetter?
halffield :: forall s b. (Tuple b s -> s) -> (forall p. StrongLike p => p Unit b -> p s s)
halffield = halflens

-- with halflens/halffield, `p s b` gets more context (the whole `s`, not only part `b`) which might be useful e.g. for validation new b candidates against the whole s 
-- and not letting invalid candidate `b` go out of the lens

-- lens :: forall p s t a b. StrongLike p => (s -> a) -> (Tuple b s -> t) -> p a b -> p s t -- weaker constraint than Strong, "Strong" is stronger than necessary 
-- lens get set = lcmap get >>> halflens set

-- field :: forall p s a. StrongLike p => (s -> a) -> (Tuple a s -> s) -> p a a -> p s s
-- field = lens


-- ChoiceLike

class Profunctor p <= ChoiceLike p where
  -- leftlike :: forall t a. p Unit Unit -> p (Either Unit t) (Either Unit t) -- a eliminated, t preserved
  leftlike :: forall t a. p a Void -> p (Either a t) t -- a eliminated, t preserved

-- instance ChoiceLike (->) where
--   leftlike a2u aort = case aort of
--     Left a -> Left a
--     Right t -> Right t

newtype R :: forall k. Type -> Type -> k -> Type
newtype R r a b = R (a -> r)

derive instance Newtype (R r a b) _

instance Profunctor (R r) where
  dimap f _ r = wrap (unwrap r <<< f)

instance ChoiceLike (R r) where
  leftlike (R a2r) = R (case _ of
    Left a -> a2r a
    Right t -> unsafeThrow "TODO"
  )

-- (a -> r) -> (Either a t -> r) and let r = Either a t

-- ChoiceLike is a generalization of Choice

-- instance Choice p => ChoiceLike p where
--   leftlike = rmap (either identity identity) <<< left
--   rightlike = rmap (either identity identity) <<< right

-- ChoiceLike is not isomorphic to Choice, because:

-- TODO

-- ChoiceLike is enough to encode prisms even though it's more generic and weaker than Choice.
-- Compare to `prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b` from `profunctor-lenses` library.

-- matcher? eliminator?
halfprism :: forall s t a. (s -> Either a t) -> (forall p. ChoiceLike p => p a Void -> p s t)
halfprism match = leftlike >>> lcmap match

halfprismInv :: forall s t a. (forall p. ChoiceLike p => p a Void -> p s t) -> s -> Either a t -- (a -> Either a t) -> (s -> Either a t)
halfprismInv f s = unwrap (f (R Left)) s

-- ctorMatcher? 
-- halfctor :: forall p s t a. ChoiceLike p => (s -> Either a t) -> p a Void -> p s t
-- halfctor = halfprism 

-- with halfprism/halfctor, `p a t` can produce more variants (the sum `t`, not only variant `a`) which might be useful

-- prism :: forall p s t a b. ChoiceLike p => (b -> t) -> (s -> Either a t) -> p a b -> p s t
-- prism build match = rmap build >>> halfprism match

-- ctor :: forall p t a. ChoiceLike p => (a -> t) -> (t -> Either a t) -> p a a -> p t t
-- ctor = prism



-- what about dual `forall a b . p (Either a b) b -> p a b`? It's trivial: it's `lcmap Right`