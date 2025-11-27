module Data.Profunctor.ExceptP where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..), either)
import Data.Int (base36)
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

-- ExceptP is related to a Kleisli arrow for except monad called `Except r`
newtype Except w a b = Except (a -> Either w b)

derive instance Newtype (Except w a b) _

instance Profunctor (Except w) where
  dimap f g h = wrap (\a -> g <$> unwrap h (f a))

instance ExceptP (Except w) where
  liftExcept :: forall s x. Except w x Void -> Except w (Either x s) s -- it's like using Except to make an optic on Except?!
  liftExcept h = wrap $ either (Left <<< either identity absurd <<< unwrap h) Right

-- additionally
instance Semigroupoid (Except w) where
  compose g f = wrap \a ->
    case unwrap f a of
      Left w -> Left w
      Right b -> unwrap g b

instance Category (Except w) where
  identity = wrap Right

-- `forall p. ExceptP p => Optic p a b w Void` is isomorphic to `Except w a b`
elimVar :: forall p w a b. ExceptP p => Except w a b -> Optic p a b w Void
elimVar f = liftExcept >>> lcmap (unwrap f)

elimVarInv :: forall w a b. (forall p. ExceptP p => Optic p a b w Void) -> Except w a b
elimVarInv optic = optic (Except Left)

handle :: forall @l p s t e. ExceptP p => IsSymbol l => Cons l e t s => Lacks l t => Optic p (Variant s) (Variant t) e (Variant ())
handle = rmap case_ >>> elimVar (wrap (on (Proxy @l) Left Right))

-- TODO: we need kind of `p (Variant s) (Variant ())` so we need it. Or do we? Without that we enforce exhaustive pattern match which is maybe good.
otherwise :: forall p a. p a Void
otherwise = unsafeCoerce unit
