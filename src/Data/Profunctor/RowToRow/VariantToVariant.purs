module Data.Profunctor.RowToRow.VariantToVariant
  ( bind
  , variantToVariant
  , class VariantToVariant
  , discard
  )
  where

import Data.Profunctor (class Profunctor)
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Data.Variant.Internal (class VariantTags)
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)

class Profunctor p <= VariantToVariant p where
  variantToVariant :: forall i1 i1l o1 bx i2 i2l o2 dx o12 i o. 
    Union i1 i2 i => Union i2 i1 i => -- i1 and i2 partition i (disjoint inputs)
    Union o1 o2 o12 => Nub o12 o =>   -- o is deduped union of o1 and o2 (overlapping outputs)
    Union o1 bx o => Union o2 dx o => -- expansion evidence: o1 ⊆ o, o2 ⊆ o
    RowToList i1 i1l => VariantTags i1l => RowToList i2 i2l => VariantTags i2l => -- runtime tag dispatch
    p (Variant i1) (Variant o1) -> p (Variant i2) (Variant o2) -> p (Variant i) (Variant o)
                                                             
bind ∷ ∀ f i1 i1l o1 o1x i2 i2l o2 o2x o12 i o. VariantToVariant f ⇒ Union i1 i2 i ⇒ Union i2 i1 i ⇒ RowToList i1 i1l ⇒ VariantTags i1l ⇒ RowToList i2 i2l ⇒ VariantTags i2l ⇒ Union o1 o2 o12 ⇒ Nub o12 o ⇒ Union o1 o1x o ⇒ Union o2 o2x o ⇒ f (Variant i1) (Variant o1) → (f (Variant i1) (Variant o1) → f (Variant i2) (Variant o2)) → f (Variant i) (Variant o)
bind first cont = variantToVariant first (cont first)

discard ∷ ∀ f i1 i1l o1 o1x i2 i2l o2 o2x o12 i o. VariantToVariant f ⇒ Union i1 i2 i ⇒ Union i2 i1 i ⇒ RowToList i1 i1l ⇒ VariantTags i1l ⇒ RowToList i2 i2l ⇒ VariantTags i2l ⇒ Union o1 o2 o12 ⇒ Nub o12 o ⇒ Union o1 o1x o ⇒ Union o2 o2x o ⇒ f (Variant i1) (Variant o1) → (Unit → f (Variant i2) (Variant o2)) → f (Variant i) (Variant o)
discard first cont = bind first (\_ -> cont unit)
