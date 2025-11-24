module Data.Profunctor.OutputP where

import Prelude

import Data.Lens (Optic)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Profunctor.Cont (Cont(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst)
import Prim.Row (class Cons)
import Record (get)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= OutputP p where
  liftOutputP :: forall s o. p o Void -> p (Tuple s o) s -- o output, s context

-- `forall p. OutputP p => Optic p s s o Void` encodes `s -> o`

output :: forall s o. (s -> o) -> (forall p. OutputP p => Optic p s s o Void)
output project = liftOutputP >>> lcmap (\s -> Tuple s (project s))

function = output

-- uses `instance OutputP (Cont r)`, instance OutputP (->)` is useless here
outputInv :: forall s o. (forall p. OutputP p => Optic p s s o Void) -> s -> o
outputInv f s = unwrap (f (Cont \_ -> identity)) (unsafeCoerce unit) s -- TODO `unsafeCoerce unit`` is a smell

output' :: forall @l o t s. IsSymbol l => Cons l o t s => (forall p. OutputP p => Optic p (Record s) (Record s) o Void)
output' = output $ get (Proxy @l)

instance OutputP (->) where
  liftOutputP _ (Tuple s _) = s

instance OutputP (Cont r) where
  liftOutputP cont =  wrap \_ (Tuple _ e) -> unwrap cont absurd e

-- OutputP is not a subclass of Strong
-- OutputP is a superclass of Strong:
strongToOutputP :: forall p. Profunctor p => (forall a b c. p a b -> p (Tuple c a) (Tuple c b)) -> (forall s o. p o Void -> p (Tuple s o) s)
strongToOutputP second = second >>> rmap fst

-- utils

static :: forall s a. a -> (forall p. OutputP p => Optic p s s a Void)
static a = output (const a)

constant = static
