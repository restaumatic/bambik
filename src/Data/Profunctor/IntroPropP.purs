module Data.Profunctor.IntroPropP where

import Prelude

import Data.Lens (Optic)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Profunctor.Cont (Cont, introduce', run)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons, class Lacks)
import Record (insert)
import Type.Proxy (Proxy(..))

class Profunctor p <= IntroPropP p where
  liftIntroProp :: forall s i. p Unit i -> p s (Tuple s i) -- i input, s preserved

-- `forall p. IntroPropP p => Optic p s t Unit i` encodes `s -> i -> t`

introProp :: forall s t i. (Tuple s i -> t) -> (forall p. IntroPropP p => Optic p s t Unit i)
introProp introduce = liftIntroProp >>> rmap introduce

-- uses `instance IntroPropP (Cont r)`, `instance IntroPropP (->)` is useless here
introPropInv :: forall s t i. (forall p. IntroPropP p => Optic p s t s i) -> s -> i -> t
introPropInv f s i = run (f (introduce' i)) s

input :: forall p @l i s t. IsSymbol l => Cons l i s t => Lacks l s => IntroPropP p => Optic p (Record s) (Record t) (Record ()) i
input = lcmap (const {}) >>> introProp \(Tuple s i) -> insert (Proxy @l) i s

instance IntroPropP (->) where
  liftIntroProp f s = Tuple s (f unit)

instance IntroPropP (Cont r) where
  liftIntroProp cont = wrap $ \si2r s -> unwrap cont (\i -> si2r (Tuple s i)) unit

-- IntroPropP is not a subclass of Strong
-- IntroPropP is a superclass of Strong:
strongToIntroPropP :: forall p. Profunctor p => (forall a b c. p a b -> p (Tuple c a) (Tuple c b)) -> (forall s i. p Unit i -> p s (Tuple s i))
strongToIntroPropP second = second >>> lcmap (\s -> Tuple s unit)

-- TODO: implement defaults as combination of IntroProp and ElimProp

-- utils

-- input not from the user
-- fakeIntro :: forall s. (forall p. IntroPropP p => Optic p s s Unit s)
-- fakeIntro = introProp \(Tuple olds news) -> news

-- doesn't introduce but replace - overwrite
replace :: forall s t p. IntroPropP p => Optic p s t Unit t
replace = introProp \(Tuple _ t) -> t

-- doesn't introduce but does nothing - no-op write
ignore :: forall t i p. IntroPropP p => Optic p t t Unit i
ignore = introProp \(Tuple s _) -> s

-- replaces with nothing - write/delete
foo :: forall s i p . IntroPropP p => Optic p s Unit Unit i
foo = introProp \_ -> unit

-- so it's "user write" with `p Unit i`
