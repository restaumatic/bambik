module Data.Profunctor.RowToRow.RecordToRecord
  ( bind
  , recordToRecord
  , class RecordToRecord
  , discard
  , RR
  )
  where

import Data.Profunctor (class Profunctor)
import Data.Unit (Unit, unit)
import Prim.Row (class Nub, class Union)

type RR :: (Type -> Type -> Type) -> Symbol -> Symbol -> Type -> Type -> Type
type RR p la lb a b = p (Record (la :: a)) (Record (lb :: b))

class Profunctor p <= RecordToRecord p where
  recordToRecord :: forall a b c d ac i o. Union a c ac => Nub ac i => Union b d o => p (Record a) (Record b) -> p (Record c) (Record d) -> p (Record i) (Record o)

bind ∷ ∀ f a b c d ac i o. RecordToRecord f ⇒ Union a c ac ⇒ Nub ac i ⇒ Union b d o ⇒ f (Record a) (Record b) → (f (Record a) (Record b) → f (Record c) (Record d)) → f (Record i) (Record o)
bind first cont = recordToRecord first (cont first)

discard ∷ ∀ f a b c d ac i o. RecordToRecord f ⇒ Union a c ac ⇒ Nub ac i ⇒ Union b d o ⇒ f (Record a) (Record b) → (Unit → f (Record c) (Record d)) → f (Record i) (Record o)
discard first cont = bind first (\_ -> cont unit)
