module Data.Profunctor.IntroPropP where

import Prelude

import Data.Lens (Optic)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Profunctor.Cont (Cont, introduce', run)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), uncurry)
import Prim.Row (class Cons, class Lacks)
import Record (insert)
import Type.Proxy (Proxy(..))

class Profunctor p <= IntroPropP p where
  liftIntroProp :: forall s i. p Unit i -> p s (Tuple s i) -- i input, s context

-- `forall p. IntroPropP p => Optic p s t Unit i` encodes `s -> i -> t`

introProp :: forall s t i. (s -> i -> t) -> (forall p. IntroPropP p => Optic p s t Unit i)
introProp introduce = liftIntroProp >>> rmap (uncurry introduce)

-- uses `instance IntroPropP (Cont r)`, `instance IntroPropP (->)` is useless here
introPropInv :: forall s t i. (forall p. IntroPropP p => Optic p s t s i) -> s -> i -> t
introPropInv f s i = run (f (introduce' i)) s

input :: forall p @l t s i. IsSymbol l => Cons l i s t => Lacks l s => IntroPropP p => Optic p (Record s) (Record t) (Record ()) i
input = (introProp \s i -> insert (Proxy @l) i s) <<< lcmap (\_ -> {})

instance IntroPropP (->) where
  liftIntroProp f s = Tuple s (f unit)

instance IntroPropP (Cont r) where
  liftIntroProp cont = wrap $ \si2r s -> unwrap cont (\i -> si2r (Tuple s i)) unit

-- IntroPropP is not a subclass of Strong
-- IntroPropP is a superclass of Strong:
strongToIntroPropP :: forall p. Profunctor p => (forall a b c. p a b -> p (Tuple c a) (Tuple c b)) -> (forall s i. p Unit i -> p s (Tuple s i))
strongToIntroPropP second = second >>> lcmap (\s -> Tuple s unit)

-- TODO: implement defaults as combination of IntroProp and ElimProp
