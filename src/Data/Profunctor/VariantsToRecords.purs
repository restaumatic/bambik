module Data.Profunctor.VariantsToRecords
  ( bind
  , variantsToRecords
  , class VariantsToRecords
  , discard
  )
  where

import Data.Profunctor (class Profunctor)
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Prim.Row (class Union)

class Profunctor p <= VariantsToRecords p where
  variantsToRecords :: forall a b c d i o. Union a c i => Union b d o => p (Variant a) (Record b) -> p (Variant c) (Record d) -> p (Variant i) (Record o)

bind ∷ ∀ f a b c d i o. VariantsToRecords f ⇒ Union a c i => Union b d o => f (Variant a) (Record b) → (f (Variant a) (Record b) → f (Variant c) (Record d)) → f (Variant i) (Record o)
bind a b = a `variantsToRecords` b a

discard ∷ ∀ f a b c d i o. VariantsToRecords f ⇒ Union a c i => Union b d o => f (Variant a) (Record b) → (Unit → f (Variant c) (Record d)) → f (Variant i) (Record o)
discard a b = a `variantsToRecords` b unit
