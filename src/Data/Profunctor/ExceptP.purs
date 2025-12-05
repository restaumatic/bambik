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

-- case, scenario
-- accepts only `p w Void` that does not perform any observable effect
-- effectless? final?
-- data-dependent?
-- Decomposing property variant to cases
class Profunctor p <= ExceptP p where
  liftExcept :: forall s w. p w Void -> p (Either w s) s -- w written, s preserved
  -- endPropertyVariantCases :: p Void Unit -- last line in output

-- liftExcept (view :: UI Web View Void) :: UI Web (Either View s) s
--   removes case

-- if you want to project anything you have to be in variant case, cases are records and have fields

-- p Void Void -> p s s -- static is no-op write

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

-- `ExceptP p => Optic p a b w Void` is isomorphic to `Except w a b`
except :: forall p w a b. ExceptP p => Except w a b -> Optic p a b w Void
except f = liftExcept >>> lcmap (unwrap f)

exceptInv :: forall w a b. (forall p. ExceptP p => Optic p a b w Void) -> Except w a b
exceptInv optic = optic (Except Left)

-- a -> Either w a
if' :: forall p w a. ExceptP p => Except w a a -> Optic p a a w Void
if' f = except f

never :: forall p a. ExceptP p => Optic p a a Void Void
never = except (Except \a -> Right a)

case' :: forall @l p s t e. ExceptP p => IsSymbol l => Cons l e t s => Lacks l t => Optic p (Variant s) (Variant t) e Void
case' = except (wrap (on (Proxy @l) Left Right))



-- TODO: we need kind of `p (Variant s) (Variant ())` so we need it. Or do we? Without that we enforce exhaustive pattern match which is maybe good.
otherwise :: forall p a. p a Void
otherwise = unsafeCoerce unit
