module Data.Profunctor.Row.VariantToRecord
  ( bind
  , variantToRecord
  , class VariantToRecord
  , discard
  )
  where

import Data.Profunctor (class Profunctor)
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Type.Row.Constraints (class DispatchableVariants, class ExclusiveRows)

class Profunctor p <= VariantToRecord p where
  variantToRecord :: forall i1 i1l i2 i2l o1 o2 i o.
    ExclusiveRows i1 i2 i =>
    ExclusiveRows o1 o2 o =>
    DispatchableVariants i1 i2 i1l i2l =>
    p (Variant i1) (Record o1) -> p (Variant i2) (Record o2) -> p (Variant i) (Record o)

bind :: forall f i1 i1l i2 i2l o1 o2 i o.
  VariantToRecord f =>
  ExclusiveRows i1 i2 i =>
  ExclusiveRows o1 o2 o =>
  DispatchableVariants i1 i2 i1l i2l =>
  f (Variant i1) (Record o1) -> (f (Variant i1) (Record o1) -> f (Variant i2) (Record o2)) -> f (Variant i) (Record o)
bind first cont = variantToRecord first (cont first)

discard :: forall f i1 i1l i2 i2l o1 o2 i o.
  VariantToRecord f =>
  ExclusiveRows i1 i2 i =>
  ExclusiveRows o1 o2 o =>
  DispatchableVariants i1 i2 i1l i2l =>
  f (Variant i1) (Record o1) -> (Unit -> f (Variant i2) (Record o2)) -> f (Variant i) (Record o)
discard first cont = bind first (\_ -> cont unit)
