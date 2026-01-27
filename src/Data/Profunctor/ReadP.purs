module Data.Profunctor.ReadP where

import Prelude

import Data.Either (Either(..))
import Data.Lens (Optic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Profunctor.ExceptP (class ExceptP, liftExcept)
import Data.Profunctor.IntroVarO (class IntroVarP, liftIntroVar)
import Data.Profunctor.Sum (class Sum)
import Data.Profunctor.Sum as Sum
import Data.Profunctor.WriteP (class WriteP, liftWrite)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (Variant, case_, expand, inj, on)
import Effect.Exception.Unsafe (unsafeThrow)
import Prim.Row (class Cons, class Lacks, class Union)
import QualifiedDo.Semigroupoid as S
import Record (delete, get, insert)
import Record.Unsafe (unsafeSet)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Profunctor.RecordsToVariants as RecordsToVariants

-- read from user, input interaction, input, focusable, openable, linear, step by step, one after another, all of, dynamic
-- Composing case record from properties
class Profunctor p <= ReadP p where
  liftRead :: forall s r. p Unit r -> p s (Tuple s r) -- r read, s preserved, Unit for passing control flow

  -- we need p s (Tuple Unit r)
  -- we need p s r

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
introduceProperty :: forall p @l prop s t. IsSymbol l => Cons l prop s t => ReadP p => FormP p => Optic p (Record s) (Record t) (Variant ()) prop
introduceProperty = lcmap absurd >>> introduceToProduct >>> rmap (\(Tuple s i) -> insertUnsafe (Proxy @l) i s)
  where
    introduceToProduct :: forall a b. p Void b -> p a (Tuple a b)
    introduceToProduct = liftForm >>> liftRead
    -- p (Record s) a -> p a
    -- def :: (Record s) -> Variant (a)
    -- lcamp def
    -- input :: p Void prop
    -- liftRead :: forall s r. p Unit r -> p s (Tuple s r) -- r read, s preserved, Unit for passing control flow
    -- liftForm :: forall r. p Void r -> p Unit r
    insertUnsafe -- unsafe as we don't require `Lacks l r1`
      :: forall r1 r2 l a
      . IsSymbol l
      -- => Lacks l r1
      => Cons l a r1 r2
      => Proxy l
      -> a
      -> Record r1
      -> Record r2
    insertUnsafe l a r = unsafeSet (reflectSymbol l) a r

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
eliminateCase :: forall p @l case_ s t. IsSymbol l => Cons l case_ t s => ExceptP p => ZP p => Optic p (Variant s) (Variant t) case_ (Record ())
eliminateCase = rmap (const unit) >>> liftZ >>> liftExcept >>> lcmap (on (Proxy @l) Left Right)

-- example

textInput :: forall p @l s t. IsSymbol l => Cons l String s t => Lacks l s => ReadP p => FormP p => p (Record s) (Record t)
textInput = introduceProperty @l (unsafeThrow "actual text input" :: p (Variant ()) String)

textOutput :: forall p @l s t. IsSymbol l => Cons l String t s => Lacks l t => WriteP p => YP p => p (Record s) (Record t)
textOutput = eliminateProperty @l (unsafeThrow "actual text output" :: p String (Variant ()))

-- button :: forall p @l s t. IsSymbol l => Cons l String t s => Lacks l t => WriteP p => YP p => p (Record s) (Record t)
-- button = eliminateProperty @l (unsafeThrow "actual text output" :: p String (Variant ()))

inputForm :: forall p @l s t prop. IsSymbol l => Cons l prop s t => ReadP p => FormP p => Profunctor p => Optic p (Record s) (Record t) (Variant ()) prop
inputForm = introduceProperty @l >>> ((unsafeThrow "actual input form") :: forall a b. Optic p a b a b)

tab :: forall p @l case_ s t r. IsSymbol l => Cons l case_ s t => Union s r t => IntroVarP p => XP p => Optic p (Variant s) (Variant t) (Record ()) case_
tab = introduceCase @l >>> ((unsafeThrow "actual tab") :: forall a b. Optic p a b a b)

-- button :: forall p @l case_ rest variantwithcase. Profunctor p => IsSymbol l => Cons l case_ rest variantwithcase => p case_ (Variant variantwithcase)
-- button = unsafeCoerce unit
button :: forall p a. Profunctor p => p a a
button = unsafeCoerce unit

-- twoButtons :: forall p i t. Sum p => p i (Variant ( apply ∷ i , save ∷ i | t ) )
-- twoButtons = Sum.do
--   button @"save"
--   button @"apply"

-- button :: forall p @l case_ s t r. IsSymbol l => Cons l case_ s t => Union s r t => IntroVarP p => XP p => p (Variant s) (Variant t) (Record ()) case_
-- button = introduceCase @l >>> ((unsafeThrow "actual tab") :: forall a b. Optic p a b a b)

consume :: forall p @l @a s t. IsSymbol l => Cons l a t s => Lacks l t => WriteP p => YP p => p (Record s) (Record t)
consume = eliminateProperty @l (unsafeThrow "consume" :: p a (Variant ()))


snackbar :: forall p @l case_ s t. IsSymbol l => Cons l case_ t s => ExceptP p => ZP p => Optic p (Variant s) (Variant t) case_ (Record ())
snackbar = eliminateCase @l >>> (unsafeThrow "actual popup output" :: forall a b. Optic p a b a b)

saveOrder :: forall p. p (Record (fulfillment :: Variant (dinein :: Record (time :: String, table :: String), takeaway :: Record (time :: String, phone :: String), delivery :: Record (time :: String, address :: String)), payment :: Variant (cash :: Record (amount :: String), tab :: Record (amount :: String, cardNumber :: String)))) (Variant (networkError :: Record (message :: String), invalidOrder :: Record (message :: String), orderSaved :: Record (id :: String)))
saveOrder = unsafeThrow "actual save order"

process :: forall p r v. p (Variant
                        ( save :: Record (userToken :: String, fulfillment :: Variant
                                                    ( delivery :: { address :: String
                                                                  , time :: String
                                                                  }
                                                    , dinein :: { table :: String
                                                                , time :: String
                                                                }
                                                    , takeaway :: { phone :: String
                                                                  , time :: String
                                                                  }
                                                    )
                                  , payment :: Variant
                                                ( cash :: { amount :: String
                                                          }
                                                , tab :: { amount :: String
                                                          , cardNumber :: String
                                                          }
                                                )
                        | r)
                        , saveAndPrintReceipt :: Record (userToken :: String, fulfillment :: Variant
                                                                    ( delivery :: { address :: String
                                                                                  , time :: String
                                                                                  }
                                                                    , dinein :: { table :: String
                                                                                , time :: String
                                                                                }
                                                                    , takeaway :: { phone :: String
                                                                                  , time :: String
                                                                                  }
                                                                    )
                                                , payment :: Variant
                                                                ( cash :: { amount :: String
                                                                          }
                                                                , tab :: { amount :: String
                                                                        , cardNumber :: String
                                                                        }
                                                                )
                        | r)
                    ))
                    (Variant ( networkError :: { message :: String
                                    }
                              , invalidOrder :: { message :: String
                                                }
                              , printerError :: { message :: String
                                              }
                              , orderSaved :: { id :: String
                                              }
                              | v)
                    )
process = unsafeThrow "actual process order"


----

exampleForm :: forall p s.
  Semigroupoid p => ReadP p => FormP p => IntroVarP p => XP p => p (Record s)
                                                                                  { fulfillment :: Variant
                                                                                                     ( delivery :: { address :: String
                                                                                                                   , time :: String
                                                                                                                   }
                                                                                                     , dinein :: { table :: String
                                                                                                                 , time :: String
                                                                                                                 }
                                                                                                     , takeaway :: { phone :: String
                                                                                                                   , time :: String
                                                                                                                   }
                                                                                                     )
                                                                                  , payment :: Variant
                                                                                                 ( cash :: { amount :: String
                                                                                                           }
                                                                                                 , tab :: { amount :: String
                                                                                                          , cardNumber :: String
                                                                                                          }
                                                                                                 )
                                                                                  | s
                                                                                  }
exampleForm = S.do
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

exampleHandling :: forall p t.
  Semigroupoid p => ExceptP p => ZP p => WriteP p => YP p => p
                                                                              (Variant
                                                                                 ( invalidOrder :: { message :: String
                                                                                                   }
                                                                                 , orderSaved :: { id :: String
                                                                                                 }
                                                                                 , printerError :: { message :: String
                                                                                                   }
                                                                                 | t
                                                                                 )
                                                                              )
                                                                              (Variant t)
exampleHandling = S.do
  snackbar @"invalidOrder" S.do
    textOutput @"message"
  snackbar @"printerError" S.do
    textOutput @"message"
  snackbar @"orderSaved" S.do
    textOutput @"id"


--   -------- presentation --------------- ------------ business --------------
-- p (Record context) (Variant exceptions) (Variant request) (Variant response)

-- business :: p (Record requestParameters) (Variant responses)

-- orderInput :: forall k1002 s1007 t1139. Semigroupoid k1002 => ReadP k1002 => FormP k1002 => IntroVarP k1002 => XP k1002 => Sum k1002 => ExceptP k1002 => ZP k1002 => WriteP k1002 => YP k1002 => k1002 (Record ("property1" :: String | s1007)) (Variant ("case1" :: String | t1139))
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
  Sum.do
    S.do
      button \{fulfillment, payment, authToken} -> mock @(Variant invalidOrder, orderSaved, networkError)
      -- button
      -- consume @"authToken" @String
      -- consume @"fulfillment"
      -- someAction
      snackbar @"invalidOrder" S.do
        textOutput @"message"
      snackbar @"orderSaved" S.do
        textOutput @"id"
    S.do
      button
      consume @"printerConnectionPool"
      someAction
      snackbar @"invalidOrder" S.do
        textOutput @"message"
      snackbar @"printerError" S.do
        textOutput @"message"
      snackbar @"orderSaved" S.do
        textOutput @"id"

someAction :: forall p r v. p (Record r) (Variant v)
someAction = unsafeThrow "some action"

save :: forall p. p (Record ( "property1" :: String) ) (Variant ("case1" :: String) )
save = unsafeThrow "actual someAction"

saveAndPrintReceipt :: forall p. p (Record ( "property1" :: String) ) (Variant ("case1" :: String) )
saveAndPrintReceipt = unsafeThrow "actual save and print receipt"

-- main :: ∀ p. Semigroupoid p ⇒ ReadP p ⇒ WriteP p ⇒ FormP p ⇒ IntroVarP p ⇒ YP p ⇒ XP p ⇒ ZP p ⇒ ExceptP p ⇒ Sum p ⇒ p (Record (userToken :: String, userId :: String)) (Variant (networkError :: Record (message :: String), internalError :: Record (message :: String)))
-- main = orderInput


-- ReadP is different then a Strong profunctor
-- ReadP is a superclass of Strong but not vice versa
strongToReadP :: forall p. Profunctor p => (forall a b c. p a b -> p (Tuple c a) (Tuple c b)) -> (forall s r. p Unit r -> p s (Tuple s r))
strongToReadP second = second >>> lcmap (\s -> Tuple s unit)

-- ReadP is related to `Reader r` which is related to
-- a) a co-Kleisli arrow for the product comonad called `Reader r`
-- b) a Kleisli arrow for the reader monad
newtype Reader r a b = Reader (Tuple a r -> b)

newtype Asker r a b = ReaderWithDefault
  { reader :: Tuple a r -> b
  , default :: a -> r
  }

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

