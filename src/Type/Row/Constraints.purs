module Type.Row.Constraints
  ( class InclusiveRows
  , class ExclusiveRows
  , class DispatchableVariants
  )
  where

import Data.Variant.Internal (class VariantTags)
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)

-- r1 and r2 may overlap; r is their deduped union; both r1 ⊆ r and r2 ⊆ r.
-- Witness rows: r12 = r1 ∪ r2 (pre-nub), r1x = r ∖ r1, r2x = r ∖ r2.
class
  ( Union r1 r2 r12
  , Nub r12 r
  , Union r1 r1x r
  , Union r2 r2x r
  ) <= InclusiveRows r1 r2 r r12 r1x r2x

instance
  ( Union r1 r2 r12
  , Nub r12 r
  , Union r1 r1x r
  , Union r2 r2x r
  ) => InclusiveRows r1 r2 r r12 r1x r2x

-- r1 and r2 are disjoint; their union is r.
class
  ( Union r1 r2 r
  , Union r2 r1 r
  ) <= ExclusiveRows r1 r2 r

instance
  ( Union r1 r2 r
  , Union r2 r1 r
  ) => ExclusiveRows r1 r2 r

-- Variants r1 and r2 carry runtime tag info for dispatch.
-- Witness lists: r1l = RowToList r1, r2l = RowToList r2.
class
  ( RowToList r1 r1l
  , VariantTags r1l
  , RowToList r2 r2l
  , VariantTags r2l
  ) <= DispatchableVariants r1 r2 r1l r2l

instance
  ( RowToList r1 r1l
  , VariantTags r1l
  , RowToList r2 r2l
  , VariantTags r2l
  ) => DispatchableVariants r1 r2 r1l r2l
