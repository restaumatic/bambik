module Data.Profunctor.RowToRow.RecordToVariant
  ( bind
  , class RecordToVariant
  , discard
  , recordToVariant
  )
  where

import Data.Profunctor (class Profunctor)
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Prim.Row (class Nub, class Union)

class Profunctor p <= RecordToVariant p where
  recordToVariant :: forall i1 o1 o1x i2 o2 o2x i12 o12 i o i1x i2x.
    Union i1 i2 i12 => Nub i12 i =>     -- i is deduped union of i1 and i2 (inclusive inputs)
    Union i1 i1x i => Union i2 i2x i => -- projection evidence: i1 ⊆ i, i2 ⊆ i
    Union o1 o2 o12 => Nub o12 o =>     -- o is deduped union of o1 and o2 (inclusive outputs)
    Union o1 o1x o => Union o2 o2x o => -- expansion evidence: o1 ⊆ o, o2 ⊆ o
    p (Record i1) (Variant o1) -> p (Record i2) (Variant o2) -> p (Record i) (Variant o)

bind ∷ ∀ f i1 o1 o1x i2 o2 o2x i12 o12 i o i1x i2x. RecordToVariant f ⇒ Union i1 i2 i12 ⇒ Nub i12 i ⇒ Union i1 i1x i ⇒ Union i2 i2x i ⇒ Union o1 o2 o12 ⇒ Nub o12 o ⇒ Union o1 o1x o ⇒ Union o2 o2x o ⇒ f (Record i1) (Variant o1) → (f (Record i1) (Variant o1) → f (Record i2) (Variant o2)) → f (Record i) (Variant o)
bind first cont = recordToVariant first (cont first)

discard ∷ ∀ f i1 o1 o1x i2 o2 o2x i12 o12 i o i1x i2x. RecordToVariant f ⇒ Union i1 i2 i12 ⇒ Nub i12 i ⇒ Union i1 i1x i ⇒ Union i2 i2x i ⇒ Union o1 o2 o12 ⇒ Nub o12 o ⇒ Union o1 o1x o ⇒ Union o2 o2x o ⇒ f (Record i1) (Variant o1) → (Unit → f (Record i2) (Variant o2)) → f (Record i) (Variant o)
discard first cont = bind first (\_ -> cont unit)
