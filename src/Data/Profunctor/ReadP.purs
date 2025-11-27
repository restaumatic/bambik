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

-- ReadP is different then a Strong profunctor
-- ReadP is a superclass of Strong but not vice versa
strongToReadP :: forall p. Profunctor p => (forall a b c. p a b -> p (Tuple c a) (Tuple c b)) -> (forall s r. p Unit r -> p s (Tuple s r))
strongToReadP second = second >>> lcmap (\s -> Tuple s unit)

-- ReadP is related to a Kleisli arrow for the reader monad (and/or co-Kliesli arrow for the product comonad - TODO check) called `Reader r`
newtype Reader r a b = Reader (Tuple a r -> b)

derive instance Newtype (Reader r a b) _

instance Profunctor (Reader r) where
  dimap f g w = wrap \a'r -> g (unwrap w (Tuple (f (fst a'r)) (snd a'r)))

instance ReadP (Reader r) where
  liftRead :: forall s y. Reader r Unit y -> Reader r s (Tuple s y) -- it's like using Reader to make an optic on Reader?!
  liftRead f = wrap \(Tuple s r) -> Tuple s (unwrap f (Tuple unit r))

-- additionally
instance Semigroupoid (Reader r) where
  compose g f = wrap \ar -> unwrap g (Tuple (unwrap f ar) (snd ar))

instance Category (Reader r) where
  identity = wrap fst

-- `forall p. ReadP p => Optic p a b Unit r` is isomorphic to `Reader r a b`
read :: forall p r a b. ReadP p => Reader r a b -> Optic p a b Unit r
read f = liftRead >>> rmap (unwrap f)

readInv :: forall r a b. (forall p. ReadP p => Optic p a b Unit r) -> Reader r a b
readInv optic = optic (Reader snd)

-- read and insert it as a field into a record
input :: forall p @l r s t. IsSymbol l => Cons l r s t => Lacks l s => ReadP p => Optic p (Record s) (Record t) (Record ()) r
input = lcmap (const {}) >>> read (wrap \(Tuple s i) -> insert (Proxy @l) i s)

load :: forall s r p. ReadP p => Optic p s r Unit r
load = read $ wrap \(Tuple _ t) -> t

-- read but don't insert
ignore :: forall t r p. ReadP p => Optic p t t Unit r
ignore = read (wrap \(Tuple s _) -> s)

