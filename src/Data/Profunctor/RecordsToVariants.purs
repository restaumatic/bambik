module Data.Profunctor.RecordsToVariants
  ( bind
  , productToSum
  , class ProductToSum
  , discard
  )
  where

import Data.Either (Either(..))
import Data.Profunctor (class Profunctor, dimap)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Prim.Row (class Nub, class Union)
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= ProductToSum p where
  productToSum :: forall a b c d. p a b -> p c d -> p (Tuple a c) (Either b d)

bind ∷ ∀ f a b c d ac bd i o. ProductToSum f ⇒ Union a c ac ⇒ Nub ac i ⇒ Union b d bd ⇒ Nub bd o ⇒ f (Record a) (Variant b) → (f (Record a) (Variant b) → f (Record c) (Variant d)) → f (Record i) (Variant o)
bind first cont = dimap splitRecord mergeVariant (productToSum first (cont first))
  where
    splitRecord :: Record i -> Tuple (Record a) (Record c)
    splitRecord r = Tuple (unsafeCoerce r) (unsafeCoerce r)

    mergeVariant :: Either (Variant b) (Variant d) -> Variant o
    mergeVariant (Left v) = unsafeCoerce v
    mergeVariant (Right v) = unsafeCoerce v

discard ∷ ∀ f a b c d ac bd i o. ProductToSum f ⇒ Union a c ac ⇒ Nub ac i ⇒ Union b d bd ⇒ Nub bd o ⇒ f (Record a) (Variant b) → (Unit → f (Record c) (Variant d)) → f (Record i) (Variant o)
discard first cont = bind first (\_ -> cont unit)


