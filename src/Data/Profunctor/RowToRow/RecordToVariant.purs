module Data.Profunctor.RowToRow.RecordToVariant
  ( bind
  , class RecordToVariant
  , discard
  , recordToVariant
  )
  where

import Data.Profunctor (class Profunctor)
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Prim.Row (class Nub, class Union)

class Profunctor p <= RecordToVariant p where
  recordToVariant :: forall a b c d ac bd i o. Union a c ac => Nub ac i => Union b d bd => Nub bd o => p (Record a) (Variant b) -> p (Record c) (Variant d) -> p (Record i) (Variant o)

bind ∷ ∀ f a b c d ac bd i o. RecordToVariant f ⇒ Union a c ac ⇒ Nub ac i ⇒ Union b d bd ⇒ Nub bd o ⇒ f (Record a) (Variant b) → (f (Record a) (Variant b) → f (Record c) (Variant d)) → f (Record i) (Variant o)
bind first cont = recordToVariant first (cont first)

discard ∷ ∀ f a b c d ac bd i o. RecordToVariant f ⇒ Union a c ac ⇒ Nub ac i ⇒ Union b d bd ⇒ Nub bd o ⇒ f (Record a) (Variant b) → (Unit → f (Record c) (Variant d)) → f (Record i) (Variant o)
discard first cont = bind first (\_ -> cont unit)
