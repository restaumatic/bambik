module Data.Profunctor.StrongLike where

import Prelude

import Data.Profunctor (class Profunctor, rmap)
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

-- Half-lens (a.k.a. introductor) is similar to a lens but it only introduces a part so it's only one function: `Tuple s b -> t`

-- StrongLike is enough to encode a half-lens as there is an isomorphism between
-- `Tuple b s -> t` and `forall StrongLike p. p Unit b -> p s t`.

halflens :: forall s t b. (Tuple b s -> t) -> (forall p. StrongLike p => p Unit b -> p s t)
halflens introduce = firstlike >>> rmap introduce

-- Does it have any sense/application?
halflens' :: forall s b. (Tuple b s -> s) -> (forall p. StrongLike p => p Unit b -> p s s)
halflens' = halflens

-- Useful StrongLike instance for decoding half-lenses
-- Add to profunctors package? 
instance StrongLike (->) where
  firstlike f s = Tuple (f unit) s
  secondlike f s = Tuple s (f unit)

halflensDecode :: forall s t b. (forall p. StrongLike p => p Unit b -> p s t) -> Tuple b s -> t
halflensDecode f (Tuple b s) = f (const b) s
