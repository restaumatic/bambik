module Data.Profunctor.VariantsToVariants
  ( bind
  , sumToSum
  , class SumToSum
  , discard
  )
  where

import Data.Either (Either(..))
import Data.Profunctor (class Profunctor, dimap)
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Prim.Row (class Nub, class Union)
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= SumToSum p where
  sumToSum :: forall a b c d. p a b -> p c d -> p (Either a c) (Either b d)

bind ∷ ∀ f a b c d bd i o. SumToSum f ⇒ Union a c i ⇒ Union b d bd ⇒ Nub bd o ⇒ f (Variant a) (Variant b) → (f (Variant a) (Variant b) → f (Variant c) (Variant d)) → f (Variant i) (Variant o)
bind first cont = dimap splitVariant mergeVariant (sumToSum first (cont first))
  where
    splitVariant :: Variant i -> Either (Variant a) (Variant c)
    splitVariant v = unsafeCoerce v

    mergeVariant :: Either (Variant b) (Variant d) -> Variant o
    mergeVariant (Left v) = unsafeCoerce v
    mergeVariant (Right v) = unsafeCoerce v

discard ∷ ∀ f a b c d bd i o. SumToSum f ⇒ Union a c i ⇒ Union b d bd ⇒ Nub bd o ⇒ f (Variant a) (Variant b) → (Unit → f (Variant c) (Variant d)) → f (Variant i) (Variant o)
discard first cont = bind first (\_ -> cont unit)
