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
import Type.Row.Constraints (class InclusiveRows)

class Profunctor p <= RecordToVariant p where
  recordToVariant :: forall i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
    InclusiveRows i1 i2 i i12 i1x i2x =>
    InclusiveRows o1 o2 o o12 o1x o2x =>
    p (Record i1) (Variant o1) -> p (Record i2) (Variant o2) -> p (Record i) (Variant o)

bind :: forall f i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
  RecordToVariant f =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  InclusiveRows o1 o2 o o12 o1x o2x =>
  f (Record i1) (Variant o1) -> (f (Record i1) (Variant o1) -> f (Record i2) (Variant o2)) -> f (Record i) (Variant o)
bind first cont = recordToVariant first (cont first)

discard :: forall f i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
  RecordToVariant f =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  InclusiveRows o1 o2 o o12 o1x o2x =>
  f (Record i1) (Variant o1) -> (Unit -> f (Record i2) (Variant o2)) -> f (Record i) (Variant o)
discard first cont = bind first (\_ -> cont unit)
