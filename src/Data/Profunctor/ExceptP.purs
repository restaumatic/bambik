module Data.Profunctor.ExceptP where

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

class Profunctor p <= ExceptP p where
  liftExcept :: forall s w. p w Void -> p (Either w s) s -- w written, s preserved

-- TODO: check ExceptP relation to Choice

-- useful ExceptP instance
-- `Except r` is a Kleisli arrow for `Except r`
newtype Except r a b = Except (a -> Either r b)

derive instance Newtype (Except r a b) _

instance Profunctor (Except r) where
  dimap f g h = wrap (\a -> g <$> unwrap h (f a))

instance ExceptP (Except r) where
  liftExcept h = wrap $ either (Left <<< either identity absurd <<< unwrap h) Right

-- `forall p. ExceptP p => Optic p s t w Void` encodes `s -> Either w t` using `instance ExceptP (Except r)`:
elimVar :: forall s t w. (s -> Either w t) -> (forall p. ExceptP p => Optic p s t w Void)
elimVar f = liftExcept >>> lcmap f

elimVarInv :: forall s t w. (forall p. ExceptP p => Optic p s t w Void) -> s -> Either w t
elimVarInv optic = unwrap (optic (Except Left))

handle :: forall @l p s t e. ExceptP p => IsSymbol l => Cons l e t s => Lacks l t => Optic p (Variant s) (Variant t) e (Variant ())
handle = rmap case_ >>> elimVar (on (Proxy @l) Left Right)

-- otherwise :: forall @l p s t e. ExceptP p => IsSymbol l => Cons l e t s => Lacks l t => Optic p (Variant s) (Variant t) (Variant s) (Variant t)
-- otherwise = rmap case_ >>> elimVar (on (Proxy @l) Left Right)


-- TODO: we need kind of `p (Variant s) (Variant ())` so we need it. Or do we? Without that we enforce exhaustive pattern match which is maybe good.
otherwise :: forall p a. p a Void
otherwise = unsafeCoerce unit
