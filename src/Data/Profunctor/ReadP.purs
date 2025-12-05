module Data.Profunctor.ReadP where

import Prelude

import Data.Either (Either(..))
import Data.Lens (Optic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Profunctor.ExceptP (class ExceptP, liftExcept)
import Data.Profunctor.IntroVarO (class IntroVarP, liftIntroVar)
import Data.Profunctor.WriteP (class WriteP, liftWrite)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (Variant, case_, expand, inj, on)
import Effect.Exception.Unsafe (unsafeThrow)
import Prim.Row (class Cons, class Lacks, class Union)
import QualifiedDo.Semigroupoid as S
import Record (delete, get, insert)
import Type.Proxy (Proxy(..))

-- read from user, input interaction, input, focusable, openable, linear, step by step, one after another, all of, dynamic
-- Composing case record from properties
class Profunctor p <= ReadP p where
  liftRead :: forall s r. p Unit r -> p s (Tuple s r) -- r read, s preserved, Unit for passing control flow

class Profunctor p <= FormP p where
  liftForm :: forall r. p Void r -> p Unit r

class Profunctor p <= XP p where
  liftX :: forall a. p Unit a -> p Void a

class Profunctor p <= YP p where
  liftY :: forall a. p a Void -> p a Unit

class Profunctor p <= ZP p where
  liftZ :: forall a. p a Unit -> p a Void

-- for button I want: p Void Unit -> p a a or p Unit Unit -> p a a if we want to activate button?

-- introduce property to product (tuple, Record)
-- p Void a -> p Unit a -- introduce property cases (in particular single case: `Variant a`) -> introduce property
-- p Void (Variant cases) -> p Unit (Variant cases) -- introduce property cases (in particular single case: `Variant a`) -> introduce property
-- requires liftRead
introduceProperty :: forall p @l prop s t. IsSymbol l => Cons l prop s t => Lacks l s => ReadP p => FormP p => Optic p (Record s) (Record t) (Variant ()) prop
introduceProperty = lcmap absurd >>> introduceToProduct >>> rmap (\(Tuple s i) -> insert (Proxy @l) i s)
  where
    introduceToProduct :: forall a b. p Void b -> p a (Tuple a b)
    introduceToProduct = liftForm >>> liftRead

-- introduce case to sum (either, Variant)
-- p Unit a -> p Void a -- introduce case properties (in particular single property: `Record a`) -> introduce case
-- p Unit (Record props) -> p Void (Record props) -- introduce case properties (in particular single property: `Record a`) -> introduce case
introduceCase :: forall p @l case_ s t r. IsSymbol l => Cons l case_ s t => Union s r t => IntroVarP p => XP p => Optic p (Variant s) (Variant t) (Record ()) case_
introduceCase = lcmap (const {}) >>> introduceToSum >>> rmap case _ of
  Left vars -> expand vars
  Right i -> inj (Proxy @l) i
  where
    introduceToSum :: forall a b. p Unit b -> p a (Either a b)
    introduceToSum = liftX >>> liftIntroVar



-- eliminate property from product (tuple, Record)
-- p a Void -> p a Unit -- eliminate property cases -> eliminate property
eliminateProperty :: forall p @l prop s t. IsSymbol l => Cons l prop t s => Lacks l t => WriteP p => YP p => Optic p (Record s) (Record t) prop (Variant ())
eliminateProperty = rmap (case_) >>> eliminateFromProduct >>> lcmap \prop -> Tuple (get (Proxy @l) prop) (delete (Proxy @l) prop)
  where
    eliminateFromProduct :: forall a b. p b Void -> p (Tuple b a) a
    eliminateFromProduct = liftY >>> liftWrite

-- eliminate case from sum (either, Variant)
-- p a Unit -> p a Void -- eliminate case properties -> eliminate case
eliminateCase :: forall p @l case_ s t r. IsSymbol l => Cons l case_ t s => Union t r s => ExceptP p => ZP p => Optic p (Variant s) (Variant t) case_ (Record ())
eliminateCase = rmap (const unit) >>> liftZ >>> liftExcept >>> lcmap (on (Proxy @l) Left Right)

-- example

textInput :: forall p @l s t. IsSymbol l => Cons l String s t => Lacks l s => ReadP p => FormP p => p (Record s) (Record t)
textInput = introduceProperty @l (unsafeThrow "actual text input" :: p (Variant ()) String)

textOutput :: forall p @l s t. IsSymbol l => Cons l String t s => Lacks l t => WriteP p => YP p => p (Record s) (Record t)
textOutput = eliminateProperty @l (unsafeThrow "actual text output" :: p String (Variant ()))

inputForm :: forall p @l s t prop. IsSymbol l => Cons l prop s t => Lacks l s => ReadP p => FormP p => Profunctor p => Optic p (Record s) (Record t) (Variant ()) prop
inputForm = introduceProperty @l >>> ((unsafeThrow "actual input form") :: forall a b. Optic p a b a b)

tab :: forall p @l case_ s t r. IsSymbol l => Cons l case_ s t => Union s r t => IntroVarP p => XP p => Optic p (Variant s) (Variant t) (Record ()) case_
tab = introduceCase @l >>> ((unsafeThrow "actual tab") :: forall a b. Optic p a b a b)

snackbar :: forall p @l case_ s t r. IsSymbol l => Cons l case_ t s => Union t r s => ExceptP p => ZP p => Optic p (Variant s) (Variant t) case_ (Record ())
snackbar = eliminateCase @l >>> (unsafeThrow "actual popup output" :: forall a b. Optic p a b a b)

saveOrder :: forall p. p (Record (fulfillment :: Variant (dinein :: Record (time :: String, table :: String), takeaway :: Record (time :: String, phone :: String), delivery :: Record (time :: String, address :: String)), payment :: Variant (cash :: Record (amount :: String), tab :: Record (amount :: String, cardNumber :: String)))) (Variant (networkError :: Record (message :: String), invalidOrder :: Record (message :: String), orderSaved :: Record (id :: String)))
saveOrder = unsafeThrow "actual save order"

orderInput :: forall p. Semigroupoid p => ReadP p => FormP p => IntroVarP p => XP p => ExceptP p => ZP p => WriteP p => YP p => p (Record ()) (Variant ())
orderInput = S.do
  inputForm @"fulfillment" S.do
    tab @"dinein" S.do
      textInput @"time"
      textInput @"table"
    tab @"takeaway" S.do
      textInput @"time"
      textInput @"phone"
    tab @"delivery" S.do
      textInput @"time"
      textInput @"address"
  inputForm @"payment" S.do
    tab @"cash" S.do
      textInput @"amount"
    tab @"tab" S.do
      textInput @"amount"
      textInput @"cardNumber"
  saveOrder
  -- button @"saveOrder" $ saveOrder
  -- button @"saveAndPrintOrder" $ saveOrder 
  snackbar @"networkError" S.do
    textOutput @"message"
  snackbar @"invalidOrder" S.do
    textOutput @"message"
  snackbar @"orderSaved" S.do
    textOutput @"id"


-- ReadP is different then a Strong profunctor
-- ReadP is a superclass of Strong but not vice versa
strongToReadP :: forall p. Profunctor p => (forall a b c. p a b -> p (Tuple c a) (Tuple c b)) -> (forall s r. p Unit r -> p s (Tuple s r))
strongToReadP second = second >>> lcmap (\s -> Tuple s unit)

-- ReadP is related to `Reader r` which is related to
-- a) a co-Kleisli arrow for the product comonad called `Reader r`
-- b) a Kleisli arrow for the reader monad
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

-- ReadP p => Optic p a b Unit r` is isomorphic to `Reader r a b`
read :: forall p r a b. ReadP p => Reader r a b -> Optic p a b Unit r
read f = liftRead >>> rmap (unwrap f)

readInv :: forall r a b. (forall p. ReadP p => Optic p a b Unit r) -> Reader r a b
readInv optic = optic (Reader snd)

foo :: forall p a. ReadP p => Optic p a a Unit Void
foo = read (Reader \(Tuple a _) -> a)

-- read and insert it as a field into a record
input :: forall p @l r s t. IsSymbol l => Cons l r s t => Lacks l s => ReadP p => Optic p (Record s) (Record t) Unit r
input = read (wrap \(Tuple s i) -> insert (Proxy @l) i s)

load :: forall s r p. ReadP p => Optic p s r Unit r
load = read $ wrap \(Tuple _ t) -> t

-- read but don't insert
ignore :: forall t r p. ReadP p => Optic p t t Unit r
ignore = read (wrap \(Tuple s _) -> s)

