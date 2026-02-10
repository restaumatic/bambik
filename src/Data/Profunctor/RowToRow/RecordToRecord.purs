module Data.Profunctor.RowToRow.RecordToRecord
  ( bind
  , recordToRecord
  , class RecordToRecord
  , discard
  , withDefault
  )
  where

import Data.Function (const)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Symbol (class IsSymbol)
import Data.Unit (Unit, unit)
import Prim.Row (class Cons, class Nub, class Union)
import Prim.RowList as RL
import Record (insert)
import Type.Proxy (Proxy(..))

class Profunctor p <= RecordToRecord p where
  recordToRecord :: forall a b c d ac i o. Union a c ac => Nub ac i => Union b d o => p (Record a) (Record b) -> p (Record c) (Record d) -> p (Record i) (Record o)

bind ∷ ∀ f a b c d ac i o. RecordToRecord f ⇒ Union a c ac ⇒ Nub ac i ⇒ Union b d o ⇒ f (Record a) (Record b) → (f (Record a) (Record b) → f (Record c) (Record d)) → f (Record i) (Record o)
bind first cont = recordToRecord first (cont first)

discard ∷ ∀ f a b c d ac i o. RecordToRecord f ⇒ Union a c ac ⇒ Nub ac i ⇒ Union b d o ⇒ f (Record a) (Record b) → (Unit → f (Record c) (Record d)) → f (Record i) (Record o)
discard first cont = bind first (\_ -> cont unit)

withDefault :: forall l p a r o. RL.RowToList r (RL.Cons l a RL.Nil) => IsSymbol l => Cons l a () r => Profunctor p => p (Record r) o -> a -> p (Record ()) o
withDefault p default = lcmap (const (insert (Proxy :: Proxy l) default {})) p
