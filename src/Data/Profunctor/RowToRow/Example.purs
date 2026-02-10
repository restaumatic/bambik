module Data.Profunctor.RowToRow.Example where

import Data.Profunctor (class Profunctor)
import Data.Profunctor.RowToRow.RecordToRecord as RecordToRecord
import Data.Profunctor.RowToRow.RecordToVariant as RecordToVariant
import Data.Profunctor.RowToRow.VariantToRecord as VariantToRecord
import Data.Profunctor.RowToRow.VariantToVariant as VariantToVariant
import Data.Variant (Variant)

data MyRowToRowProfunctor :: forall k1 k2. k1 -> k2 -> Type
data MyRowToRowProfunctor a b = MyRowToRowProfunctor

instance Profunctor MyRowToRowProfunctor where
  dimap _ _ MyRowToRowProfunctor = MyRowToRowProfunctor

instance RecordToRecord.RecordToRecord MyRowToRowProfunctor where
  recordToRecord MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor

instance RecordToVariant.RecordToVariant MyRowToRowProfunctor where
  recordToVariant MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor

instance VariantToRecord.VariantToRecord MyRowToRowProfunctor where
  variantToRecord MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor

instance VariantToVariant.VariantToVariant MyRowToRowProfunctor where
  variantToVariant MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor

data MyData = MyData

recordToRecordExample :: MyRowToRowProfunctor
  (Record ( in1 :: MyData , in2 :: MyData , in3 :: MyData ))
  (Record ( out1 :: MyData , out2 :: MyData , out3 :: MyData )) -- notice that this type signature can be inferred from the expression
recordToRecordExample = RecordToRecord.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in1" :: MyData)) (Record ( "out1" :: MyData ))) -- out depends on in
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in1" :: MyData, "in2" :: MyData )) (Record ( "out2" :: MyData ))) -- out can depend on multiple ins
  -- (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in1" :: MyData)) (Record ( "out1" :: MyData ))) -- outs cannot be duplicated
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in3" :: MyData)) (Record ( "out3" :: MyData ))) -- all ins and outs must be covered

recordToVariantExample :: MyRowToRowProfunctor
  (Record ( in1 :: MyData , in2 :: MyData , in3 :: MyData ))
  (Variant ( out1 :: MyData , out2 :: MyData , out3 :: MyData )) -- notice that this type signature can be inferred from the expression
recordToVariantExample = RecordToVariant.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in1" :: MyData )) (Variant ( "out1" :: MyData ))) -- out depends on in
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in1" :: MyData, "in2" :: MyData )) (Variant ( "out2" :: MyData ))) -- out can depend on multiple ins
  -- (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in1" :: MyData )) (Variant ( "out1" :: MyData ))) -- outs cannot be duplicated
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in3" :: MyData )) (Variant ( "out3" :: MyData ))) -- all ins and outs must be covered

variantToVariantExample :: MyRowToRowProfunctor
  (Variant ( in1 :: MyData , in2 :: MyData , in3 :: MyData ))
  (Variant ( out1 :: MyData , out2 :: MyData , out3 :: MyData )) -- notice that this type signature can be inferred from the expression
variantToVariantExample = VariantToVariant.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "in1" :: MyData )) (Variant ( "out1" :: MyData ))) -- out depends on in
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "in1" :: MyData, "in2" :: MyData )) (Variant ( "out2" :: MyData ))) -- out can depend on multiple ins
  -- (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "in1" :: MyData )) (Variant ( "out1" :: MyData ))) -- outs cannot be duplicated
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "in3" :: MyData )) (Variant ( "out3" :: MyData ))) -- all ins and outs must be covered

variantToRecordExample :: MyRowToRowProfunctor
  (Variant ( in1 :: MyData , in2 :: MyData , in3 :: MyData ))
  (Record ( out1 :: MyData , out2 :: MyData , out3 :: MyData )) -- notice that this type signature can be inferred from the expression
variantToRecordExample = VariantToRecord.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "in1" :: MyData )) (Record ( "out1" :: MyData ))) -- out depends on in
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "in1" :: MyData, "in2" :: MyData )) (Record ( "out2" :: MyData ))) -- out can depend on multiple ins
  -- (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "in1" :: MyData )) (Record ( "out1" :: MyData ))) -- outs cannot be duplicated
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "in3" :: MyData )) (Record ( "out3" :: MyData ))) -- all ins and outs must be covered

-- Single field/case examples

singleRecordToRecord :: MyRowToRowProfunctor (Record ( only :: String )) (Record ( result :: Int ))
singleRecordToRecord = RecordToRecord.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "only" :: String )) (Record ( "result" :: Int )))

singleVariantToVariant :: MyRowToRowProfunctor (Variant ( input :: String )) (Variant ( output :: Int ))
singleVariantToVariant = VariantToVariant.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "input" :: String )) (Variant ( "output" :: Int )))
