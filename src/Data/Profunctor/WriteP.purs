module Data.Profunctor.WriteP where

import Prelude

import Data.Lens (Optic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst)
import Data.Variant (Variant)
import Prim.Row (class Cons, class Lacks)
import Record (delete, get)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- write to user, output interaction, output, closeable
-- event-based?
-- controls data flow?
-- Decomposing case record to properties
class Profunctor p <= WriteP p where
  liftWrite :: forall s w. p w Unit -> p (Tuple w s) s -- w written, s preserved, Unit for passing control flow

-- liftWrite (button "OK" :: UI Web Unit -{activates on new data}- Unit ) :: UI Web s s
-- liftWrite (infoDialog "Close" :: UI Web Unit Unit) :: UI Web s s
-- liftWrite (reservationDialog :: UI Web Reservation Unit) :: UI Web (Tuple Reservation s) s
--   shrinks record

-- WriteP is different than String profunctor
-- WriteP is a superclass of Strong but not vice versa:
strongToWriteP :: forall p. Profunctor p => (forall a b c. p a b -> p (Tuple c a) (Tuple c b)) -> (forall s r. p r Unit -> p (Tuple s r) s)
strongToWriteP second = second >>> rmap fst

-- WriteP is related to a Kleisli arrow for the writer monad called `Writer w`
-- data-dependent?
newtype Writer w a b = Writer (a -> Tuple w b)

derive instance Newtype (Writer w a b) _

instance Profunctor (Writer w) where
  dimap f g w = wrap \a' -> g <$> unwrap w (f a')

instance WriteP (Writer w) where
  liftWrite :: forall s x. Writer w x Unit -> Writer w (Tuple x s) s -- it's like using Writer to make an optic on Writer?!
  liftWrite f = wrap \(Tuple r s) -> Tuple (fst (unwrap f r)) s

-- additionally
instance Semigroup w => Semigroupoid (Writer w) where
  compose g f = wrap \a ->
    let Tuple w1 b = unwrap f a
        Tuple w2 c = unwrap g b
    in Tuple (w1 <> w2) c

instance Monoid w => Category (Writer w) where
  identity = wrap \x -> Tuple mempty x

-- `forall p. WriteP p => Optic p a b w Unit` is isomorphic to `Writer w a b`
write :: forall w p a b. WriteP p => Writer w a b -> Optic p a b w Unit
write f = liftWrite >>> lcmap (unwrap f)

writeInv :: forall w a b. (forall p. WriteP p => Optic p a b w Unit) -> Writer w a b
writeInv o = o (Writer (\x -> Tuple x unit))

-- write a field and delete it from a record
output :: forall @l w s t p. IsSymbol l => WriteP p => Cons l w t s => Lacks l t => Optic p (Record s) (Record t) w Unit
output = rmap (const unit) >>> write (wrap \s -> Tuple (get (Proxy @l) s) (delete (Proxy @l) s))

outputCases :: forall @l cases s t p. IsSymbol l => WriteP p => Cons l (Variant cases) t s => Lacks l t => Optic p (Record s) (Record t) (Variant cases) Unit
outputCases = write (wrap \s -> Tuple (get (Proxy @l) s) (delete (Proxy @l) s))

outputCase' :: forall @l l' w' cases s t p. IsSymbol l => WriteP p => Cons l (Variant cases) t s => Cons l' w' () cases => Lacks l t => Optic p (Record s) (Record t) (Variant cases) Unit
outputCase' = write (wrap \s -> Tuple (get (Proxy @l) s) (delete (Proxy @l) s))


-- writeProjection :: forall p s a . WriteP p => (s -> a) -> Optic p s s a (Record ())
-- writeProjection f = rmap (const unit) >>> write (wrap \s -> Tuple (f s) s)

-- writeAll :: forall p s . WriteP p => Optic p s s s (Record ())
-- writeAll = writeProjection identity

-- writeConstant :: forall p s a. WriteP p => a -> Optic p s s a (Record ())
-- writeConstant a = writeProjection (const a)

-- TODO: we need kind of `p (Record s) (Record ())` so we need it. Or do we? Without that we enforce exhaustive pattern match which is maybe good.
otherwise :: forall p a. p a Unit
otherwise = unsafeCoerce unit
