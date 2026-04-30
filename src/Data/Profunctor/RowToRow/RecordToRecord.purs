module Data.Profunctor.RowToRow.RecordToRecord
  ( bind
  , recordToRecord
  , class RecordToRecord
  , discard
  )
  where

import Data.Profunctor (class Profunctor)
import Data.Unit (Unit, unit)
import Type.Row.Constraints (class ExclusiveRows, class InclusiveRows)

class Profunctor p <= RecordToRecord p where
  recordToRecord :: forall i1 o1 i2 o2 i12 i1x i2x i o.
    InclusiveRows i1 i2 i i12 i1x i2x =>
    ExclusiveRows o1 o2 o =>
    p (Record i1) (Record o1) -> p (Record i2) (Record o2) -> p (Record i) (Record o)

bind :: forall f i1 o1 i2 o2 i12 i1x i2x i o.
  RecordToRecord f =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  ExclusiveRows o1 o2 o =>
  f (Record i1) (Record o1) -> (f (Record i1) (Record o1) -> f (Record i2) (Record o2)) -> f (Record i) (Record o)
bind first cont = recordToRecord first (cont first)

discard :: forall f i1 o1 i2 o2 i12 i1x i2x i o.
  RecordToRecord f =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  ExclusiveRows o1 o2 o =>
  f (Record i1) (Record o1) -> (Unit -> f (Record i2) (Record o2)) -> f (Record i) (Record o)
discard first cont = bind first (\_ -> cont unit)
