module Data.Profunctor.StrongLike where

import Prelude

import Data.Lens (Optic)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (class Profunctor, rmap)
import Data.Profunctor.Cont (Cont(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons, class Lacks)
import Record (insert)
import Type.Proxy (Proxy(..))

-- StrongLike

class Profunctor p <= StrongLike p where
  firstlike :: forall s b . p Unit b -> p s (Tuple b s) -- b introduced, s preserved
  secondlike :: forall s b . p Unit b -> p s (Tuple s b) -- b introduced, s preserved

-- TODO: Is StrongLike a generalization of Strong or vice-versa?

-- Half-lens (a.k.a. introductor) is similar to a lens but it only introduces a part so it's only one function: `Tuple s b -> t`
-- Half-lens does not encode a full lens (a field in particular) as it does not allow to extract the b part from s.

type HalfLens s t b = forall p. StrongLike p => Optic p s t Unit b

-- StrongLike is enough to encode a half-lens as there is an isomorphism between
-- `Tuple b s -> t` and `forall StrongLike p. p Unit b -> p s t`.

halflens :: forall s t b. (Tuple b s -> t) -> HalfLens s t b
halflens introduce = firstlike >>> rmap introduce

-- Does it have any sense/application?
halflens' :: forall s b. (Tuple b s -> s) -> HalfLens s s b
halflens' = halflens

-- TODO: rename to `newField`?
field :: forall @l s r a. IsSymbol l => Cons l a r s => Lacks l r => HalfLens (Record r) (Record s) a
field = halflens (\(Tuple b s) -> insert (Proxy @l) b s)

instance StrongLike (->) where
  firstlike f s = Tuple (f unit) s
  secondlike f s = Tuple s (f unit)

-- Useful instance for decoding half-lenses
instance StrongLike (Cont r) where
  firstlike callbackunitb = wrap $ \bs2r s -> unwrap callbackunitb (\b -> bs2r (Tuple b s)) unit
  secondlike callbackunitb = wrap $ \sb2r s -> unwrap callbackunitb (\b -> sb2r (Tuple s b)) unit

halflensDecode :: forall s t b. HalfLens s t b -> Tuple b s -> t
halflensDecode f (Tuple b s) = unwrap (f (Cont (\g _ -> g b))) identity s

