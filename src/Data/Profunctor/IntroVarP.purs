module Data.Profunctor.IntroVarO where

import Prelude

import Data.Either (Either(..), either)
import Data.Lens (Optic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, rmap)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, expand, inj)
import Prim.Row (class Cons, class Union)
import Type.Proxy (Proxy(..))

-- choice, option, aside, alternative, parallel, one aside another, one of, static
-- accepts only `p Void r` that is always viable/visible and the same - static?
-- it may change on its own (like animation) but it's not controllable from the outside
-- contextless? context-independent?
-- data-independent?
-- continuous?
-- state?
-- Composing property variant from cases
class Profunctor p <= IntroVarP p where
  liftIntroVar :: forall s r. p Void r -> p s (Either s r) -- r read, s preserved

-- liftInfoVar (plainText "Hello!" :: UI Web Void Void) :: UI Web s s
--   variant/record unchanged
-- liftInfoVar (image "Home" :: UI Web Void Void) :: UI Web s s
--   variant/record unchanged
-- liftInfoVar (radioButton "Visa" :: UI Web Void Unit) :: UI Web s (Either s Unit)
--   expands variant




-- liftInfoVar (link "http://www.google.com" :: UI Web Void URL) :: UI Web s (Either s URL)
--   expands variant
-- liftInfoVar (button "OK" :: UI Web Void Unit) :: UI Web s (Either s Unit)
--   expands variant

-- p Void Void -> p s s -- static is no-op read

-- TODO: check IntroVarP relation to Choice

-- IntroVarP is related to a Kleisli arrow of ? monad / co-Kleisli arrow of ? comonad called `IntroVar r`
newtype IntroVar r a b = IntroVar (Either a r -> b)

derive instance Newtype (IntroVar r a b) _

instance Profunctor (IntroVar r) where
  dimap f g h = wrap \ear -> g (unwrap h (either (Left <<< f) Right ear))

instance IntroVarP (IntroVar r) where
  liftIntroVar :: forall s y. IntroVar r Void y -> IntroVar r s (Either s y) -- it's like using IntroVar to make an optic on IntroVar?!
  liftIntroVar h = wrap $ either Left (Right <<< unwrap h <<< Right)

-- it's not an instance of Semigroupoid, thus neither an instance of Category

-- `IntroVarP p => Optic p a b Void r` is isomorphic to `IntroVar r a b`
introVar :: forall r p a b. IntroVarP p => IntroVar r a b -> Optic p a b Void r
introVar f = liftIntroVar >>> rmap (unwrap f)

introVarInv :: forall r a b. (forall p. IntroVarP p => Optic p a b Void r) -> (Either a r -> b)
introVarInv optic = unwrap (optic (IntroVar (either absurd identity)))

always :: forall p a. IntroVarP p => Optic p a a Void Void
always = introVar (IntroVar (either identity absurd))

pick :: forall p @l t s r rest. IsSymbol l => Cons l r s t => Union s rest t => IntroVarP p => Optic p (Variant s) (Variant t) Void r
pick = introVar (wrap case _ of
  Left vars -> expand vars
  Right i -> inj (Proxy @l) i)
