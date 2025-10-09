module Data.Profunctor.StrongLike where

import Prelude

import Data.Either (Either)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Tuple (Tuple)

-- StrongLike

class Profunctor p <= StrongLike p where
  firstlike :: forall s b . p s b -> p s (Tuple b s) -- b introduced, s preserved
  secondlike :: forall s b . p s b -> p s (Tuple s b)  -- b introduced, s preserved

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
halflens :: forall p s t b. StrongLike p => (Tuple b s -> t) -> p s b -> p s t
halflens set = firstlike >>> rmap set

-- fieldSetter?
halffield :: forall p s b. StrongLike p => (Tuple b s -> s) -> p s b -> p s s
halffield = halflens

-- with halflens/halffield, `p s b` gets more context (the whole `s`, not only part `b`) which might be useful e.g. for validation new b candidates against the whole s 
-- and not letting invalid candidate `b` go out of the lens

lens :: forall p s t a b. StrongLike p => (s -> a) -> (Tuple b s -> t) -> p a b -> p s t -- weaker constraint than Strong, "Strong" is stronger than necessary 
lens get set = lcmap get >>> halflens set

field :: forall p s a. StrongLike p => (s -> a) -> (Tuple a s -> s) -> p a a -> p s s
field = lens


-- ChoiceLike

class Profunctor p <= ChoiceLike p where
  leftlike :: forall t a. p a t -> p (Either a t) t -- a eliminated, t preserved
  rightlike :: forall t a. p a t -> p (Either t a) t -- a eliminated, t preserved

-- ChoiceLike is a generalization of Choice

-- instance Choice p => ChoiceLike p where
--   leftlike = rmap (either identity identity) <<< left
--   rightlike = rmap (either identity identity) <<< right

-- ChoiceLike is not isomorphic to Choice, because:

-- TODO

-- ChoiceLike is enough to encode prisms even though it's more generic and weaker than Choice.
-- Compare to `prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b` from `profunctor-lenses` library.

-- matcher? eliminator?
halfprism :: forall p s t a. ChoiceLike p => (s -> Either a t) -> p a t -> p s t
halfprism match = leftlike >>> lcmap match

-- ctorMatcher? 
halfctor :: forall p t a. ChoiceLike p => (t -> Either a t) -> p a t -> p t t
halfctor = halfprism 

-- with halfprism/halfctor, `p a t` can produce more variants (the sum `t`, not only variant `a`) which might be useful

prism :: forall p s t a b. ChoiceLike p => (b -> t) -> (s -> Either a t) -> p a b -> p s t
prism build match = rmap build >>> halfprism match

ctor :: forall p t a. ChoiceLike p => (a -> t) -> (t -> Either a t) -> p a a -> p t t
ctor = prism



-- what about dual `forall a b . p (Either a b) b -> p a b`? It's trivial: it's `lcmap Right`