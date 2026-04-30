module Data.Profunctor.RowToRow.VariantToVariant
  ( bind
  , variantToVariant
  , class VariantToVariant
  , discard
  )
  where

import Data.Profunctor (class Profunctor)
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Type.Row.Constraints (class DispatchableVariants, class ExclusiveRows, class InclusiveRows)

class Profunctor p <= VariantToVariant p where
  variantToVariant :: forall i1 i1l i2 i2l o1 o2 o12 o1x o2x i o.
    ExclusiveRows i1 i2 i =>
    InclusiveRows o1 o2 o o12 o1x o2x =>
    DispatchableVariants i1 i2 i1l i2l =>
    p (Variant i1) (Variant o1) -> p (Variant i2) (Variant o2) -> p (Variant i) (Variant o)

bind :: forall f i1 i1l i2 i2l o1 o2 o12 o1x o2x i o.
  VariantToVariant f =>
  ExclusiveRows i1 i2 i =>
  InclusiveRows o1 o2 o o12 o1x o2x =>
  DispatchableVariants i1 i2 i1l i2l =>
  f (Variant i1) (Variant o1) -> (f (Variant i1) (Variant o1) -> f (Variant i2) (Variant o2)) -> f (Variant i) (Variant o)
bind first cont = variantToVariant first (cont first)

discard :: forall f i1 i1l i2 i2l o1 o2 o12 o1x o2x i o.
  VariantToVariant f =>
  ExclusiveRows i1 i2 i =>
  InclusiveRows o1 o2 o o12 o1x o2x =>
  DispatchableVariants i1 i2 i1l i2l =>
  f (Variant i1) (Variant o1) -> (Unit -> f (Variant i2) (Variant o2)) -> f (Variant i) (Variant o)
discard first cont = bind first (\_ -> cont unit)
