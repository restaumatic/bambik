module Data.Profunctor.Row.Example
  ( MyData(..)
  , MyRowToRowProfunctor
  , badge
  , button
  , checkbox
  , dropdown
  , eventLog
  , icon
  , image
  , link
  , menuItem
  , modal
  , notification
  , outlet
  , rating
  , recordToRecordExample
  , recordToVariantExample
  , searchBar
  , slider
  , statusBar
  , text
  , textInput
  , variantToRecordExample
  , variantToVariantExample
  , widenRecordInputExample
  , narrowVariantInputExample
  , narrowRecordOutputExample
  , widenVariantOutputExample
  , wizardStep
  )
  where

import Prelude

import Data.Profunctor (class Profunctor)
import Data.Profunctor.Row.Default (withRecordDefault, withRecordOutputDefault)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row (class Row, narrowRecordOutput, narrowVariantInput, widenRecordInput, widenVariantOutput)
import Data.Profunctor.Row.VariantToRecord (class VariantToRecord)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant (class VariantToVariant)
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (Variant)
import Prim.Row (class Cons)
import QualifiedDo.Semigroupoid as Semigroupoid

data MyRowToRowProfunctor :: forall k1 k2. k1 -> k2 -> Type
data MyRowToRowProfunctor a b = MyRowToRowProfunctor

-- Here's a profunctor
instance Profunctor MyRowToRowProfunctor where
  dimap _ _ MyRowToRowProfunctor = MyRowToRowProfunctor

-- It's also a semigroupoid
instance Semigroupoid MyRowToRowProfunctor where
  compose MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor

-- And here's the thing, it's a row-to-row profunctor
instance RecordToRecord MyRowToRowProfunctor where
  recordToRecord MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor

instance RecordToVariant MyRowToRowProfunctor where
  recordToVariant MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor

instance VariantToRecord MyRowToRowProfunctor where
  variantToRecord MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor

instance VariantToVariant MyRowToRowProfunctor where
  variantToVariant MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor

instance Row MyRowToRowProfunctor

-- here's some data type, let's take the minimal and most trivial data type with no values possible - it doesn't matter.  
data MyData

-- rule of thumb:
-- exclusive variants in inputs
-- exclusive records on outputs


-- "Exclusive variants in inputs" (VariantToRecord, VariantToVariant):
--   Union i1 i2 i => Union i2 i1 i
--   No Nub — i1 and i2 must partition i exclusively. Each variant case goes to exactly one handler.
--   A variant holds one value at a time, so routing must be unambiguous.
-- "Exclusive records on outputs" (RecordToRecord, VariantToRecord):
--   Union o1 o2 o => Union o2 o1 o
--   No Nub — o1 and o2 must be exclusive. Each profunctor contributes distinct fields.
--   Every field in a record must be produced exactly once.
-- "Inclusive records on inputs" (RecordToRecord, RecordToVariant):
--   Union i1 i2 i12 => Nub i12 i => Union i1 i1x i => Union i2 i2x i
--   Nub permits inclusion. Multiple profunctors can read the same record field.
--   Fine, because all fields are always present.
-- "Inclusive variants on outputs" (RecordToVariant, VariantToVariant):
--   Union o1 o2 o12 => Nub o12 o => Union o1 o1x o => Union o2 o2x o
--   Nub permits inclusion. Multiple profunctors can produce the same variant case.
--   Fine, because a variant is "one of" — multiple sources can offer the same case.

recordToRecordExample :: MyRowToRowProfunctor
  (Record ( in1 :: MyData , in2 :: MyData , in3 :: MyData ))
  (Record ( out1 :: MyData , out2 :: MyData , out3 :: MyData )) -- notice that this type signature can be inferred from the expression
recordToRecordExample = RecordToRecord.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in1" :: MyData)) (Record ( "out1" :: MyData ))) -- out depends on in
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in1" :: MyData, "in2" :: MyData )) (Record ( "out2" :: MyData ))) -- out can depend on multiple ins
  (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in3" :: MyData)) (Record ( "out3" :: MyData ))) -- all ins and outs must be covered
  -- (MyRowToRowProfunctor :: MyRowToRowProfunctor (Record ( "in1" :: MyData)) (Record ( "out1" :: MyData ))) -- out fields must be exclusive

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

-- Record-to-record (model in, optionally captures field)

text :: forall @l r. Cons l String () r => MyRowToRowProfunctor (Record r) (Record ())
text = MyRowToRowProfunctor

textInput :: forall @l r. Cons l String () r => MyRowToRowProfunctor (Record r) (Record r)
textInput = MyRowToRowProfunctor

checkbox :: forall @l r. Cons l Boolean () r => MyRowToRowProfunctor (Record r) (Record r)
checkbox = MyRowToRowProfunctor

slider :: forall @l r. Cons l Number () r => MyRowToRowProfunctor (Record r) (Record r)
slider = MyRowToRowProfunctor

dropdown :: forall @l a r. Cons l a () r => MyRowToRowProfunctor (Record r) (Record r)
dropdown = MyRowToRowProfunctor

image :: forall @l r. Cons l String () r => MyRowToRowProfunctor (Record r) (Record ())
image = MyRowToRowProfunctor

badge :: forall @l r. Cons l Int () r => MyRowToRowProfunctor (Record r) (Record ())
badge = MyRowToRowProfunctor

-- Record-to-variant (model in, fires event case)

button :: forall @l r v. Cons l (Record r) () v => MyRowToRowProfunctor (Record r) (Variant v)
button = MyRowToRowProfunctor

icon :: forall @l r v. Cons l (Record r) () v => MyRowToRowProfunctor (Record r) (Variant v)
icon = MyRowToRowProfunctor

link :: forall @l r v. Cons l String () v => MyRowToRowProfunctor (Record r) (Variant v)
link = MyRowToRowProfunctor

menuItem :: forall @l r v. Cons l (Record r) () v => MyRowToRowProfunctor (Record r) (Variant v)
menuItem = MyRowToRowProfunctor

-- Variant-to-record (sum-shaped model in, optionally captures field)

statusBar :: forall @l a r. Cons l a () r => MyRowToRowProfunctor (Variant r) (Record r)
statusBar = MyRowToRowProfunctor

eventLog :: forall @l a r. Cons l a () r => MyRowToRowProfunctor (Variant r) (Record r)
eventLog = MyRowToRowProfunctor

outlet :: forall v. MyRowToRowProfunctor (Variant v) (Record ())
outlet = MyRowToRowProfunctor

searchBar :: forall @l v r. Cons l String () r => MyRowToRowProfunctor (Variant v) (Record r)
searchBar = MyRowToRowProfunctor

rating :: forall @l v r. Cons l Int () r => MyRowToRowProfunctor (Variant v) (Record r)
rating = MyRowToRowProfunctor

-- Variant-to-variant (sum-shaped model in, fires event case)

notification :: forall @l a r. Cons l a () r => MyRowToRowProfunctor (Variant r) (Variant r)
notification = MyRowToRowProfunctor

modal :: forall @l a r. Cons l a () r => MyRowToRowProfunctor (Variant r) (Variant r)
modal = MyRowToRowProfunctor

wizardStep :: forall @l v w. Cons l (Record ()) () w => MyRowToRowProfunctor (Variant v) (Variant w)
wizardStep = MyRowToRowProfunctor


-- Unary row-to-row combinator examples.
--
-- Each example pins the inferred type to confirm the row reshaping
-- works in both Record-to-* and *-to-Record/Variant directions.

widenRecordInputExample :: MyRowToRowProfunctor
  (Record ( in1 :: MyData , in2 :: MyData , in3 :: MyData , extra :: MyData ))
  (Record ( out1 :: MyData , out2 :: MyData , out3 :: MyData ))
widenRecordInputExample = widenRecordInput recordToRecordExample

narrowVariantInputExample :: MyRowToRowProfunctor
  (Variant ( in1 :: MyData , in2 :: MyData ))
  (Record ( out1 :: MyData , out2 :: MyData , out3 :: MyData ))
narrowVariantInputExample = narrowVariantInput variantToRecordExample

narrowRecordOutputExample :: MyRowToRowProfunctor
  (Record ( in1 :: MyData , in2 :: MyData , in3 :: MyData ))
  (Record ( out1 :: MyData , out2 :: MyData ))
narrowRecordOutputExample = narrowRecordOutput recordToRecordExample

widenVariantOutputExample :: MyRowToRowProfunctor
  (Record ( in1 :: MyData , in2 :: MyData , in3 :: MyData ))
  (Variant ( out1 :: MyData , out2 :: MyData , out3 :: MyData , extra :: MyData ))
widenVariantOutputExample = widenVariantOutput recordToVariantExample

ui = Semigroupoid.do
  RecordToRecord.do -- inputs inclusive, outputs exclusive
    text @"message" `withRecordOutputDefault` "foo!"
    text @"code"
    textInput @"name" `withRecordDefault` ""
    textInput @"phonePrefix" `withRecordDefault` "+48"
    textInput @"phoneSuffix" `withRecordDefault` ""
    checkbox @"subscribe" `withRecordDefault` false
  RecordToVariant.do -- inputs inclusive, outputs inclusive 
    button @"submitMonthly"
    button @"submitYearly"
    button @"submit"
    icon @"submit"
  -- VariantToVariant.do -- inputs exclusive, outputs inclusive
  --   MyRowToRowProfunctor
  --   MyRowToRowProfunctor
  -- VariantToRecord.do -- inputs exclusive, outputs exclusive
  --   MyRowToRowProfunctor
  --   MyRowToRowProfunctor
