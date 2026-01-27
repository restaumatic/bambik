module Data.Profunctor.RecordsToRecords
  ( bind
  , productToProduct
  , class ProductToProduct
  , discard
  )
  where

import Data.Profunctor (class Profunctor, dimap)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Prim.Row (class Nub, class Union)
import Record.Unsafe.Union (unsafeUnion)
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= ProductToProduct p where
  productToProduct :: forall a b c d. p a b -> p c d -> p (Tuple a c) (Tuple b d)

bind ∷ ∀ f a b c d ac i o. ProductToProduct f ⇒ Union a c ac ⇒ Nub ac i ⇒ Union b d o ⇒ f (Record a) (Record b) → (f (Record a) (Record b) → f (Record c) (Record d)) → f (Record i) (Record o)
bind first cont = dimap splitRecord mergeRecord (productToProduct first (cont first))
  where
    splitRecord :: Record i -> Tuple (Record a) (Record c)
    splitRecord r = Tuple (unsafeCoerce r) (unsafeCoerce r)

    mergeRecord :: Tuple (Record b) (Record d) -> Record o
    mergeRecord (Tuple rb rd) = unsafeUnion rb rd

discard ∷ ∀ f a b c d ac i o. ProductToProduct f ⇒ Union a c ac ⇒ Nub ac i ⇒ Union b d o ⇒ f (Record a) (Record b) → (Unit → f (Record c) (Record d)) → f (Record i) (Record o)
discard first cont = bind first (\_ -> cont unit)
