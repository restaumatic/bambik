module Data.Profunctor.ReadP where

import Prelude

import Data.Lens (Optic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Prim.Row (class Cons, class Lacks)
import Record (delete, get)
import Type.Prelude (Proxy(..))

class Profunctor p <= ReadP p where
  liftRead :: forall s r. p r Unit -> p (Tuple s r) s -- r read, s preseved

-- `forall p. ReadP p => Optic p s t r Unit` encodes `s -> Tuple t r`?

read :: forall s t r. (s -> Tuple t r) -> (forall p. ReadP p => Optic p s t r Unit)
read r = liftRead >>> lcmap r

readInv :: forall s t r. (forall p. ReadP p => Optic p s t r Unit) -> s -> Tuple t r
readInv o = unwrap (o (Writer (Tuple unit)))

-- read/get and eliminate/delete
output :: forall @l r s t p. IsSymbol l => ReadP p => Cons l r t s => Lacks l t => Optic p (Record s) (Record t) r (Record ())
output = rmap (const unit) >>> read \s -> Tuple (delete (Proxy @l) s) (get (Proxy @l) s)

-- read/get
function :: forall p s a . ReadP p => (s -> a) -> Optic p s s a (Record ())
function f = rmap (const unit) >>> read \s -> Tuple s (f s)

-- fake read
constant :: forall p s a. ReadP p => a -> Optic p s s a (Record ())
constant a = function (const a)

-- useful ReadP instance
newtype Writer w a b = Writer (a -> Tuple b w)

derive instance Newtype (Writer w a b) _

instance Profunctor (Writer r) where
  dimap f g w = wrap \a' -> let (Tuple b r) = unwrap w (f a') in Tuple (g b) r

instance ReadP (Writer r) where
  liftRead (Writer f) = Writer \(Tuple s o) -> Tuple s (snd (f o))

-- ReadP is not a subclass of Strong
-- ReadP is a superclass of Strong:
strongToReadP :: forall p. Profunctor p => (forall a b c. p a b -> p (Tuple c a) (Tuple c b)) -> (forall s r. p r Unit -> p (Tuple s r) s)
strongToReadP second = second >>> rmap fst
