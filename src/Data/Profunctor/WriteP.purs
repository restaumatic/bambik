module Data.Profunctor.WriteP where

import Prelude

import Data.Lens (Optic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Prim.Row (class Cons, class Lacks)
import Record (insert)
import Type.Proxy (Proxy(..))

class Profunctor p <= WriteP p where
  liftWrite :: forall s w. p Unit w -> p s (Tuple s w) -- w written, s preserved

-- WriteP is a superclass of Strong but not vice versa:
strongToIntroPropP :: forall p. Profunctor p => (forall a b c. p a b -> p (Tuple c a) (Tuple c b)) -> (forall s w. p Unit w -> p s (Tuple s w))
strongToIntroPropP second = second >>> lcmap (\s -> Tuple s unit)

-- useful WriteP instance
newtype Reader r a b = Reader (Tuple a r -> b)

derive instance Newtype (Reader r a b) _

instance Profunctor (Reader r) where
  dimap f g w = wrap \a'r -> g (unwrap w (Tuple (f (fst a'r)) (snd a'r)))

instance WriteP (Reader r) where
  liftWrite f = wrap \(Tuple s r) -> Tuple s (unwrap f (Tuple unit r))

-- `forall p. WriteP p => Optic p s t Unit w` encodes `Tuple s w -> t`

write :: forall p s t w. WriteP p => (Tuple s w -> t) -> Optic p s t Unit w
write w = liftWrite >>> rmap w

-- uses `instance WriteP (Reader r)`
writeInv :: forall s t w. (forall p. WriteP p => Optic p s t Unit w) -> Tuple s w -> t
writeInv o = unwrap (o (Reader snd))

-- write and insert
input :: forall p @l i s t. IsSymbol l => Cons l i s t => Lacks l s => WriteP p => Optic p (Record s) (Record t) (Record ()) i
input = lcmap (const {}) >>> write \(Tuple s i) -> insert (Proxy @l) i s

overwrite :: forall s t p. WriteP p => Optic p s t Unit t
overwrite = write \(Tuple _ t) -> t

-- write but don't insert
ignore :: forall t w p. WriteP p => Optic p t t Unit w
ignore = write \(Tuple s _) -> s

