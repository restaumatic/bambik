module Data.Profunctor.ReadP where

import Prelude

import Data.Lens (Optic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Prim.Row (class Cons, class Lacks)
import Record (insert)
import Type.Proxy (Proxy(..))

class Profunctor p <= ReadP p where
  liftRead :: forall s r. p Unit r -> p s (Tuple s r) -- r read, s preserved

-- ReadP is a superclass of Strong but not vice versa:
strongToReadP :: forall p. Profunctor p => (forall a b c. p a b -> p (Tuple c a) (Tuple c b)) -> (forall s r. p Unit r -> p s (Tuple s r))
strongToReadP second = second >>> lcmap (\s -> Tuple s unit)

-- useful ReadP instance
-- Reader r is a Kliesli arrow for the Reader r monad
-- Reader r is a co-Kliesli arrow for the Product comonad (TODO: check)
newtype Reader r a b = Reader (Tuple a r -> b)

derive instance Newtype (Reader r a b) _

instance Profunctor (Reader r) where
  dimap f g w = wrap \a'r -> g (unwrap w (Tuple (f (fst a'r)) (snd a'r)))

instance ReadP (Reader r) where
  liftRead f = wrap \(Tuple s r) -> Tuple s (unwrap f (Tuple unit r))

instance Semigroupoid (Reader r) where
  compose g f = wrap \ar -> unwrap g (Tuple (unwrap f ar) (snd ar))

instance Category (Reader r) where
  identity = wrap fst

-- `forall p. ReadP p => Optic p s t Unit r` encodes `Tuple s r -> t` using `instance ReadP (Reader r)`:
read :: forall p s t r. ReadP p => (Tuple s r -> t) -> Optic p s t Unit r
read f = liftRead >>> rmap f

readInv :: forall s t r. (forall p. ReadP p => Optic p s t Unit r) -> Tuple s r -> t
readInv o = unwrap (o (Reader snd))

-- read and insert it as a field into a record
input :: forall p @l r s t. IsSymbol l => Cons l r s t => Lacks l s => ReadP p => Optic p (Record s) (Record t) (Record ()) r
input = lcmap (const {}) >>> read \(Tuple s i) -> insert (Proxy @l) i s

load :: forall s r p. ReadP p => Optic p s r Unit r
load = read \(Tuple _ t) -> t

-- read but don't insert
ignore :: forall t r p. ReadP p => Optic p t t Unit r
ignore = read \(Tuple s _) -> s

