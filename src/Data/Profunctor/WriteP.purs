module Data.Profunctor.WriteP where

import Prelude

import Data.Lens (Optic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst)
import Prim.Row (class Cons, class Lacks)
import Record (delete, get)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= WriteP p where
  liftWrite :: forall s w. p w Unit -> p (Tuple w s) s -- w written, s preseved

-- WriteP is a superclass of Strong but not vice versa:
strongToWriteP :: forall p. Profunctor p => (forall a b c. p a b -> p (Tuple c a) (Tuple c b)) -> (forall s r. p r Unit -> p (Tuple s r) s)
strongToWriteP second = second >>> rmap fst

-- useful WriteP instance
newtype Writer w a b = Writer (a -> Tuple w b)

derive instance Newtype (Writer w a b) _

instance Profunctor (Writer w) where
  dimap f g w = wrap \a' -> g <$> unwrap w (f a')

instance WriteP (Writer w) where
  liftWrite f = wrap \(Tuple r s) -> Tuple (fst (unwrap f r)) s

instance Semigroup w => Semigroupoid (Writer w) where
  compose g f = wrap \a ->
    let Tuple w1 b = unwrap f a
        Tuple w2 c = unwrap g b
    in Tuple (w1 <> w2) c

instance Monoid w => Category (Writer w) where
  identity = wrap \x -> Tuple mempty x

-- `forall p. WriteP p => Optic p s t w Unit` encodes `s -> Tuple w t` using `instance WriteP (Writer w)`:
write :: forall p s t w. WriteP p => (s -> Tuple w t) -> Optic p s t w Unit
write f = liftWrite >>> lcmap f

writeInv :: forall s t w. (forall p. WriteP p => Optic p s t w Unit) -> s -> Tuple w t
writeInv o = unwrap (o (Writer (\x -> Tuple x unit)))

-- write a field and delete it from a record
output :: forall @l w s t p. IsSymbol l => WriteP p => Cons l w t s => Lacks l t => Optic p (Record s) (Record t) w (Record ())
output = rmap (const unit) >>> write \s -> Tuple (get (Proxy @l) s) (delete (Proxy @l) s)

writeProjection :: forall p s a . WriteP p => (s -> a) -> Optic p s s a (Record ())
writeProjection f = rmap (const unit) >>> write \s -> Tuple (f s) s

writeAll :: forall p s . WriteP p => Optic p s s s (Record ())
writeAll = writeProjection identity

writeConstant :: forall p s a. WriteP p => a -> Optic p s s a (Record ())
writeConstant a = writeProjection (const a)

-- TODO: we need kind of `p (Record s) (Record ())` so we need it. Or do we? Without that we enforce exhaustive pattern match which is maybe good.
otherwise :: forall p a. p a Unit
otherwise = unsafeCoerce unit
