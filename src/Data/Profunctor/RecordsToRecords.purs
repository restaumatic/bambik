module Data.Profunctor.RecordsToRecords
  ( bind
  , recordsToRecords
  , class RecordsToRecords
  , discard
  )
  where

import Data.Profunctor (class Profunctor)
import Data.Unit (Unit, unit)
import Prim.Row (class Nub, class Union)

class Profunctor p <= RecordsToRecords p where
  recordsToRecords :: forall a b c d ac i o. Union a c ac => Nub ac i => Union b d o => p (Record a) (Record b) -> p (Record c) (Record d) -> p (Record i) (Record o)

bind ∷ ∀ f a b c d ac i o. RecordsToRecords f ⇒ Union a c ac => Nub ac i => Union b d o => f (Record a) (Record b) → (f (Record a) (Record b) → f (Record c) (Record d)) → f (Record i) (Record o)
bind a b = a `recordsToRecords` b a

discard ∷ ∀ f a b c d ac i o. RecordsToRecords f ⇒ Union a c ac => Nub ac i => Union b d o => f (Record a) (Record b) → (Unit → f (Record c) (Record d)) → f (Record i) (Record o)
discard a b = a `recordsToRecords` b unit
