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
  recordToRecord :: forall i1 o1 i2 o2 i12 i o i1x i2x.
    Union i1 i2 i12 => Nub i12 i =>     -- i is deduped union of i1 and i2 (inclusive inputs)
    Union i1 i1x i => Union i2 i2x i => -- projection evidence: i1 ⊆ i, i2 ⊆ i
    Union o1 o2 o => Union o2 o1 o =>   -- o1 and o2 partition o (exclusive outputs)
    p (Record i1) (Record o1) -> p (Record i2) (Record o2) -> p (Record i) (Record o)

bind ∷ ∀ f i1 o1 i2 o2 i12 i o i1x i2x. RecordToRecord f ⇒ Union i1 i2 i12 ⇒ Nub i12 i ⇒ Union i1 i1x i ⇒ Union i2 i2x i ⇒ Union o1 o2 o ⇒ Union o2 o1 o ⇒ f (Record i1) (Record o1) → (f (Record i1) (Record o1) → f (Record i2) (Record o2)) → f (Record i) (Record o)
bind first cont = recordToRecord first (cont first)

discard ∷ ∀ f i1 o1 i2 o2 i12 i o i1x i2x. RecordToRecord f ⇒ Union i1 i2 i12 ⇒ Nub i12 i ⇒ Union i1 i1x i ⇒ Union i2 i2x i ⇒ Union o1 o2 o ⇒ Union o2 o1 o ⇒ f (Record i1) (Record o1) → (Unit → f (Record i2) (Record o2)) → f (Record i) (Record o)
discard first cont = bind first (\_ -> cont unit)

withDefault :: forall l p a r o. RL.RowToList r (RL.Cons l a RL.Nil) => IsSymbol l => Cons l a () r => Profunctor p => p (Record r) o -> a -> p (Record ()) o
withDefault p default = lcmap (const (insert (Proxy :: Proxy l) default {})) p
