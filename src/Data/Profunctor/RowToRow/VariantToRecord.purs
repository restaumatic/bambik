module Data.Profunctor.RowToRow.VariantToRecord
  ( bind
  , variantToRecord
  , class VariantToRecord
  , discard
  )
  where

import Data.Profunctor (class Profunctor)
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Data.Variant.Internal (class VariantTags)
import Prim.Row (class Union)
import Prim.RowList (class RowToList)

class Profunctor p <= VariantToRecord p where
  variantToRecord :: forall i1 i1l o1 i2 i2l o2 i o.
    Union i1 i2 i => Union i2 i1 i => -- i1 and i2 partition i (disjoint inputs)
    Union o1 o2 o => Union o2 o1 o => -- o1 and o2 partition o (disjoint outputs)
    RowToList i1 i1l => VariantTags i1l => RowToList i2 i2l => VariantTags i2l => -- runtime tag dispatch
    p (Variant i1) (Record o1) -> p (Variant i2) (Record o2) -> p (Variant i) (Record o)

bind ∷ ∀ f i1 i1l o1 i2 i2l o2 i o. VariantToRecord f ⇒ Union i1 i2 i ⇒ Union i2 i1 i ⇒ RowToList i1 i1l ⇒ VariantTags i1l ⇒ RowToList i2 i2l ⇒ VariantTags i2l ⇒ Union o1 o2 o ⇒ Union o2 o1 o ⇒ f (Variant i1) (Record o1) → (f (Variant i1) (Record o1) → f (Variant i2) (Record o2)) → f (Variant i) (Record o)
bind first cont = variantToRecord first (cont first)

discard ∷ ∀ f i1 i1l o1 i2 i2l o2 i o. VariantToRecord f ⇒ Union i1 i2 i ⇒ Union i2 i1 i ⇒ RowToList i1 i1l ⇒ VariantTags i1l ⇒ RowToList i2 i2l ⇒ VariantTags i2l ⇒ Union o1 o2 o ⇒ Union o2 o1 o ⇒ f (Variant i1) (Record o1) → (Unit → f (Variant i2) (Record o2)) → f (Variant i) (Record o)
discard first cont = bind first (\_ -> cont unit)
