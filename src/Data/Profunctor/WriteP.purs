module Data.Profunctor.WriteP where

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

class Profunctor p <= WriteP p where
  liftWrite :: forall s w. p Unit w -> p s (Tuple s w) -- w written, s preserved

-- `forall p. WriteP p => Optic p s t Unit w` encodes `Tuple s w -> t`

write :: forall p s t w. WriteP p => (Tuple s w -> t) -> Optic p s t Unit w
write w = liftWrite >>> rmap w

-- uses `instance WriteP (Cont r)`, `instance WriteP (->)` is useless here
writeInv :: forall s t w. (forall p. WriteP p => Optic p s t Unit w) -> Tuple s w -> t
writeInv f (Tuple s w) = run (f (introduce' w)) s

-- write and insert
input :: forall p @l i s t. IsSymbol l => Cons l i s t => Lacks l s => WriteP p => Optic p (Record s) (Record t) (Record ()) i
input = lcmap (const {}) >>> write \(Tuple s i) -> insert (Proxy @l) i s

overwrite :: forall s t p. WriteP p => Optic p s t Unit t
overwrite = write \(Tuple _ t) -> t

-- write but don't insert
ignore :: forall t w p. WriteP p => Optic p t t Unit w
ignore = write \(Tuple s _) -> s

instance WriteP (->) where
  liftWrite f s = Tuple s (f unit)

instance WriteP (Cont r) where
  liftWrite cont = wrap $ \si2r s -> unwrap cont (\i -> si2r (Tuple s i)) unit

-- WriteP is not a subclass of Strong
-- WriteP is a superclass of Strong:
strongToIntroPropP :: forall p. Profunctor p => (forall a b c. p a b -> p (Tuple c a) (Tuple c b)) -> (forall s i. p Unit i -> p s (Tuple s i))
strongToIntroPropP second = second >>> lcmap (\s -> Tuple s unit)
