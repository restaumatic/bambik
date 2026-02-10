module Data.Profunctor.RowToRow.VariantToRecord
  ( bind
  , variantToRecord
  , class VariantToRecord
  , discard
  , VariantToRecordPrim
  )
  where

import Data.Profunctor (class Profunctor)
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Prim.Row (class Cons, class Nub, class Union)

class Profunctor p <= VariantToRecord p where
  variantToRecord :: forall a b c d ac i o. Union a c ac => Nub ac i => Union b d o => p (Variant a) (Record b) -> p (Variant c) (Record d) -> p (Variant i) (Record o)

type VariantToRecordPrim :: (Type -> Type -> Type) -> Symbol -> Type -> Row Type -> Type
type VariantToRecordPrim p propname proptype r = forall v. Cons propname proptype () v => p (Variant r) (Record v)

bind ∷ ∀ f a b c d ac i o. VariantToRecord f ⇒ Union a c ac ⇒ Nub ac i ⇒ Union b d o ⇒ f (Variant a) (Record b) → (f (Variant a) (Record b) → f (Variant c) (Record d)) → f (Variant i) (Record o)
bind first cont = variantToRecord first (cont first)

discard ∷ ∀ f a b c d ac i o. VariantToRecord f ⇒ Union a c ac ⇒ Nub ac i ⇒ Union b d o ⇒ f (Variant a) (Record b) → (Unit → f (Variant c) (Record d)) → f (Variant i) (Record o)
discard first cont = bind first (\_ -> cont unit)
