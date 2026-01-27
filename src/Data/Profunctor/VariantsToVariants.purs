module Data.Profunctor.VariantsToVariants
  ( bind
  , variantsToVariants
  , class VariantsToVariants
  , discard
  )
  where

import Data.Profunctor (class Profunctor)
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Prim.Row (class Nub, class Union)

class Profunctor p <= VariantsToVariants p where
  variantsToVariants :: forall a b c d bd i o. Union a c i => Union b d bd => Nub bd o => p (Variant a) (Variant b) -> p (Variant c) (Variant d) -> p (Variant i) (Variant o)

bind ∷ ∀ f a b c d bd i o. VariantsToVariants f ⇒ Union a c i => Union b d bd => Nub bd o => f (Variant a) (Variant b) → (f (Variant a) (Variant b) → f (Variant c) (Variant d)) → f (Variant i) (Variant o)
bind a b = a `variantsToVariants` b a

discard ∷ ∀ f a b c d bd i o. VariantsToVariants f ⇒ Union a c i => Union b d bd => Nub bd o => f (Variant a) (Variant b) → (Unit → f (Variant c) (Variant d)) → f (Variant i) (Variant o)
discard a b = a `variantsToVariants` b unit
