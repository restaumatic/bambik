module Data.Profunctor.ElimVarP where

import Prelude

import Data.Either (Either(..), either)
import Data.Lens (Optic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, case_, on)
import Prim.Row (class Cons, class Lacks)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= ElimVarP p where
  liftElimVar :: forall s w. p w Void -> p (Either w s) s -- w written, s preserved

-- TODO: check ElimVarP relation to Choice

-- useful ElimVarP instance
newtype ElimVar r a b = ElimVar (a -> Either r b)

derive instance Newtype (ElimVar r a b) _

instance Profunctor (ElimVar r) where
  dimap f g h = wrap (\a -> g <$> unwrap h (f a))

instance ElimVarP (ElimVar r) where
  liftElimVar h = wrap $ either (Left <<< either identity absurd <<< unwrap h) Right

-- `forall p. ElimVarP p => Optic p s t w Void` encodes `s -> Either w t` using `instance ElimVarP (ElimVar r)`:
elimVar :: forall s t w. (s -> Either w t) -> (forall p. ElimVarP p => Optic p s t w Void)
elimVar f = liftElimVar >>> lcmap f

elimVarInv :: forall s t w. (forall p. ElimVarP p => Optic p s t w Void) -> s -> Either w t
elimVarInv optic = unwrap (optic (ElimVar Left))

handle :: forall @l p s t e. ElimVarP p => IsSymbol l => Cons l e t s => Lacks l t => Optic p (Variant s) (Variant t) e (Variant ())
handle = rmap case_ >>> elimVar (on (Proxy @l) Left Right)

-- otherwise :: forall @l p s t e. ElimVarP p => IsSymbol l => Cons l e t s => Lacks l t => Optic p (Variant s) (Variant t) (Variant s) (Variant t)
-- otherwise = rmap case_ >>> elimVar (on (Proxy @l) Left Right)


-- TODO: we need kind of `p (Variant s) (Variant ())` so we need it. Or do we? Without that we enforce exhaustive pattern match which is maybe good.
otherwise :: forall p a. p a Void
otherwise = unsafeCoerce unit
