module Data.Profunctor.RowToRow.VariantToVariant
  ( bind
  , variantToVariant
  , class VariantToVariant
  , discard
  , VariantToVariantPrim
  )
  where

import Data.Profunctor (class Profunctor)
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Prim.Row (class Cons, class Nub, class Union)

class Profunctor p <= VariantToVariant p where
  variantToVariant :: forall a b c d bd i o. Union a c i => Union b d bd => Nub bd o => p (Variant a) (Variant b) -> p (Variant c) (Variant d) -> p (Variant i) (Variant o)

type VariantToVariantPrim :: (Type -> Type -> Type) -> Symbol -> Type -> Row Type -> Type
type VariantToVariantPrim p casename casetype r = forall v. Cons casename casetype () v => p (Variant r) (Variant v)

bind ∷ ∀ f a b c d bd i o. VariantToVariant f ⇒ Union a c i ⇒ Union b d bd ⇒ Nub bd o ⇒ f (Variant a) (Variant b) → (f (Variant a) (Variant b) → f (Variant c) (Variant d)) → f (Variant i) (Variant o)
bind first cont = variantToVariant first (cont first)

discard ∷ ∀ f a b c d bd i o. VariantToVariant f ⇒ Union a c i ⇒ Union b d bd ⇒ Nub bd o ⇒ f (Variant a) (Variant b) → (Unit → f (Variant c) (Variant d)) → f (Variant i) (Variant o)
discard first cont = bind first (\_ -> cont unit)
