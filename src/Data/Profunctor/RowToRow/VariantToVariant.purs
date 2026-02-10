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
import Prim.Row (class Nub, class Union)

class Profunctor p <= VariantToVariant p where
  variantToVariant :: forall a b c d ac i o. Union a c ac => Nub ac i => Union b d o => p (Variant a) (Variant b) -> p (Variant c) (Variant d) -> p (Variant i) (Variant o)

bind ∷ ∀ f a b c d ac i o. VariantToVariant f ⇒ Union a c ac ⇒ Nub ac i ⇒ Union b d o ⇒ f (Variant a) (Variant b) → (f (Variant a) (Variant b) → f (Variant c) (Variant d)) → f (Variant i) (Variant o)
bind first cont = variantToVariant first (cont first)

discard ∷ ∀ f a b c d ac i o. VariantToVariant f ⇒ Union a c ac ⇒ Nub ac i ⇒ Union b d o ⇒ f (Variant a) (Variant b) → (Unit → f (Variant c) (Variant d)) → f (Variant i) (Variant o)
discard first cont = bind first (\_ -> cont unit)
