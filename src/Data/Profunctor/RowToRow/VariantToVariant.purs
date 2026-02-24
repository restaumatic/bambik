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
  variantToVariant :: forall a al b bx c cl d dx bd i o. Union a c i => Union c a i => RowToList a al => VariantTags al => RowToList c cl => VariantTags cl => Union b d bd => Nub bd o => Union b bx o => Union d dx o => p (Variant a) (Variant b) -> p (Variant c) (Variant d) -> p (Variant i) (Variant o)

bind ∷ ∀ f a al b bx c cl d dx bd i o. VariantToVariant f ⇒ Union a c i ⇒ Union c a i ⇒ RowToList a al ⇒ VariantTags al ⇒ RowToList c cl ⇒ VariantTags cl ⇒ Union b d bd ⇒ Nub bd o ⇒ Union b bx o ⇒ Union d dx o ⇒ f (Variant a) (Variant b) → (f (Variant a) (Variant b) → f (Variant c) (Variant d)) → f (Variant i) (Variant o)
bind first cont = variantToVariant first (cont first)

discard ∷ ∀ f a al b bx c cl d dx bd i o. VariantToVariant f ⇒ Union a c i ⇒ Union c a i ⇒ RowToList a al ⇒ VariantTags al ⇒ RowToList c cl ⇒ VariantTags cl ⇒ Union b d bd ⇒ Nub bd o ⇒ Union b bx o ⇒ Union d dx o ⇒ f (Variant a) (Variant b) → (Unit → f (Variant c) (Variant d)) → f (Variant i) (Variant o)
discard first cont = bind first (\_ -> cont unit)
