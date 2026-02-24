module Data.Profunctor.RowToRow.Example
  ( MyData(..)
  , MyRowToRowProfunctor
  , checkbox
  , recordToRecordExample
  , recordToVariantExample
  , textInput
  , textOutput
  , variantToRecordExample
  , variantToVariantExample
  )
  where

import Prelude

import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Profunctor.RowToRow.RecordToRecord (withDefault)
import Data.Profunctor.RowToRow.RecordToRecord as RecordToRecord
import Data.Profunctor.RowToRow.RecordToVariant as RecordToVariant
import Data.Profunctor.RowToRow.VariantToRecord as VariantToRecord
import Data.Profunctor.RowToRow.VariantToVariant as VariantToVariant
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, inj)
import Prim.Row (class Cons)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import QualifiedDo.Semigroupoid as Semigroupoid

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

instance Semigroupoid MyRowToRowProfunctor where
  compose MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor


data MyData = MyData

-- rule of thumb:
-- disjoint variants in inputs
-- disjoint records on outputs

recordToRecordExample :: MyRowToRowProfunctor
  (Record ( in1 :: MyData , in2 :: MyData , in3 :: MyData ))
  (Record ( out1 :: MyData , out2 :: MyData , out3 :: MyData )) -- notice that this type signature can be inferred from the expression
recordToRecordExample = RecordToRecord.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in1" :: MyData)) (Record ( "out1" :: MyData ))) -- out depends on in
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in1" :: MyData, "in2" :: MyData )) (Record ( "out2" :: MyData ))) -- out can depend on multiple ins
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in3" :: MyData)) (Record ( "out3" :: MyData ))) -- all ins and outs must be covered
  -- (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in1" :: MyData)) (Record ( "out1" :: MyData ))) -- out fields must be disjoint 

recordToVariantExample :: MyRowToRowProfunctor
  (Record ( in1 :: MyData , in2 :: MyData , in3 :: MyData ))
  (Variant ( out1 :: MyData , out2 :: MyData , out3 :: MyData )) -- notice that this type signature can be inferred from the expression
recordToVariantExample = RecordToVariant.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in1" :: MyData )) (Variant ( "out1" :: MyData ))) -- out depends on in
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in1" :: MyData, "in2" :: MyData )) (Variant ( "out2" :: MyData ))) -- out can depend on multiple ins
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in3" :: MyData )) (Variant ( "out3" :: MyData ))) -- all ins and outs must be covered
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in3" :: MyData )) (Variant ( "out1" :: MyData ))) -- out fields can be duplicated

variantToVariantExample :: MyRowToRowProfunctor
  (Variant ( in1 :: MyData , in2 :: MyData , in3 :: MyData ))
  (Variant ( out1 :: MyData , out2 :: MyData, out3 :: MyData )) -- notice that this type signature can be inferred from the expression
variantToVariantExample = VariantToVariant.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "in1" :: MyData )) (Variant ( "out1" :: MyData )))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "in2" :: MyData, "in3" :: MyData )) (Variant ( "out1" :: MyData, "out2" :: MyData, "out3" :: MyData )))

variantToRecordExample :: MyRowToRowProfunctor
  (Variant ( in1 :: MyData , in2 :: MyData , in3 :: MyData ))
  (Record ( out1 :: MyData , out2 :: MyData , out3 :: MyData )) -- notice that this type signature can be inferred from the expression
variantToRecordExample = VariantToRecord.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "in1" :: MyData )) (Record ( "out1" :: MyData )))
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Variant ( "in2" :: MyData, "in3" :: MyData )) (Record ( "out2" :: MyData, "out3" :: MyData )))

-- Single field/case examples

textInput :: forall @l r . IsSymbol l => Cons l String () r => MyRowToRowProfunctor (Record r) (Record r)
textInput = MyRowToRowProfunctor

checkbox :: forall @l r . IsSymbol l => Cons l Boolean () r => MyRowToRowProfunctor (Record r) (Record r)
checkbox = MyRowToRowProfunctor

textOutput :: forall @l r . IsSymbol l => Cons l String () r => MyRowToRowProfunctor (Record r) (Record r)
textOutput = MyRowToRowProfunctor

button :: forall @l r v. IsSymbol l => Cons l (Record r) () v => MyRowToRowProfunctor (Record r) (Variant v)
button = MyRowToRowProfunctor


ui = Semigroupoid.do
  RecordToRecord.do
    textOutput @"code"
    textInput @"name" `withDefault` ""
    textInput @"phonePrefix" `withDefault` "+48"
    textInput @"phoneSuffix" `withDefault` ""
    checkbox @"subscribe" `withDefault` false
  RecordToVariant.do
    button @"submitMonthly"
    button @"submitYearly"
  -- VariantToVariant.do



-- TODO what is that?
whatisthat :: forall l p a r o. RL.RowToList r (RL.Cons l a RL.Nil) => IsSymbol l => Cons l a () r => Profunctor p => p (Variant r) o -> a -> p (Variant ()) o
whatisthat p default = lcmap (const (inj (Proxy :: Proxy l) default)) p

whatisthat2 :: forall l p a r i. RL.RowToList r (RL.Cons l a RL.Nil) => IsSymbol l => Cons l a () r => Profunctor p => p i (Variant ()) -> a -> p i (Variant r)
whatisthat2 p default = rmap (const (inj (Proxy :: Proxy l) default)) p
