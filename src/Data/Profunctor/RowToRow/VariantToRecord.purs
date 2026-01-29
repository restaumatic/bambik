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
import Prim.Row (class Union)

class Profunctor p <= VariantToRecord p where
  variantToRecord :: forall a b c d i o. Union a c i => Union b d o => p (Variant a) (Record b) -> p (Variant c) (Record d) -> p (Variant i) (Record o)

bind ∷ ∀ f a b c d i o. VariantToRecord f ⇒ Union a c i ⇒ Union b d o ⇒ f (Variant a) (Record b) → (f (Variant a) (Record b) → f (Variant c) (Record d)) → f (Variant i) (Record o)
bind first cont = variantToRecord first (cont first)

discard ∷ ∀ f a b c d i o. VariantToRecord f ⇒ Union a c i ⇒ Union b d o ⇒ f (Variant a) (Record b) → (Unit → f (Variant c) (Record d)) → f (Variant i) (Record o)
discard first cont = bind first (\_ -> cont unit)
