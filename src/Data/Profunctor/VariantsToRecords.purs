module Data.Profunctor.VariantsToRecords
  ( bind
  , sumToProduct
  , class SumToProduct
  , discard
  )
  where

import Data.Either (Either)
import Data.Profunctor (class Profunctor, dimap)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Prim.Row (class Union)
import Record.Unsafe.Union (unsafeUnion)
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= SumToProduct p where
  sumToProduct :: forall a b c d. p a b -> p c d -> p (Either a c) (Tuple b d)

bind ∷ ∀ f a b c d i o. SumToProduct f ⇒ Union a c i ⇒ Union b d o ⇒ f (Variant a) (Record b) → (f (Variant a) (Record b) → f (Variant c) (Record d)) → f (Variant i) (Record o)
bind first cont = dimap splitVariant mergeRecord (sumToProduct first (cont first))
  where
    splitVariant :: Variant i -> Either (Variant a) (Variant c)
    splitVariant v = unsafeCoerce v

    mergeRecord :: Tuple (Record b) (Record d) -> Record o
    mergeRecord (Tuple rb rd) = unsafeUnion rb rd

discard ∷ ∀ f a b c d i o. SumToProduct f ⇒ Union a c i ⇒ Union b d o ⇒ f (Variant a) (Record b) → (Unit → f (Variant c) (Record d)) → f (Variant i) (Record o)
discard first cont = bind first (\_ -> cont unit)
