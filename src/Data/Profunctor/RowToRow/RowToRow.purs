module Data.Profunctor.RowToRow.RowToRow
  ( class RowToRow
  )
  where

import Data.Profunctor.RowToRow.RecordToRecord (class RecordToRecord)
import Data.Profunctor.RowToRow.RecordToVariant (class RecordToVariant)
import Data.Profunctor.RowToRow.VariantToRecord (class VariantToRecord)
import Data.Profunctor.RowToRow.VariantToVariant (class VariantToVariant)

class (RecordToRecord p, RecordToVariant p, VariantToRecord p, VariantToVariant p) <= RowToRow p
