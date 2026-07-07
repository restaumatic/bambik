module Data.Profunctor.Row.Example
  ( MyData(..)
  , MyRowToRowProfunctor
  , actionButton
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
  , request
  , searchBar
  , slider
  , statusBar
  , submit
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

import Data.Either (Either(..))
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord, withRecordDefault, withRecordOutputDefault)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant, class Resolving, shutter)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row (class Row, narrowRecordOutput, narrowVariantInput, widenRecordInput, widenVariantOutput)
import Data.Profunctor.Row.VariantToRecord (class VariantToRecord, class Retaining, reel)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant (class VariantToVariant)
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Symbol (class IsSymbol)
import Data.Variant (inj)
import Prim.Row (class Cons)
import QualifiedDo.Semigroupoid as Semigroupoid
import Type.Proxy (Proxy(..))

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

-- It also carries the two mixed-direction strengths: Shutter (× → +) and Reel (+ → ×)
instance Resolving MyRowToRowProfunctor where
  resolve MyRowToRowProfunctor = MyRowToRowProfunctor

instance Retaining MyRowToRowProfunctor where
  retain MyRowToRowProfunctor = MyRowToRowProfunctor

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
  { in1 :: MyData, in2 :: MyData, in3 :: MyData }
  { out1 :: MyData, out2 :: MyData, out3 :: MyData } -- notice that this type signature can be inferred from the expression
recordToRecordExample = RecordToRecord.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor { "in1" :: MyData } { "out1" :: MyData }) -- out depends on in
  (MyRowToRowProfunctor :: MyRowToRowProfunctor { "in1" :: MyData, "in2" :: MyData } { "out2" :: MyData }) -- out can depend on multiple ins
  (MyRowToRowProfunctor :: MyRowToRowProfunctor { "in3" :: MyData } { "out3" :: MyData }) -- all ins and outs must be covered
  -- (MyRowToRowProfunctor :: MyRowToRowProfunctor { "in1" :: MyData } { "out1" :: MyData }) -- out fields must be exclusive

recordToVariantExample :: MyRowToRowProfunctor
  { in1 :: MyData, in2 :: MyData, in3 :: MyData }
  [ out1 :: MyData, out2 :: MyData, out3 :: MyData ] -- notice that this type signature can be inferred from the expression
recordToVariantExample = RecordToVariant.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor { "in1" :: MyData } [ "out1" :: MyData ]) -- out depends on in
  (MyRowToRowProfunctor :: MyRowToRowProfunctor { "in1" :: MyData, "in2" :: MyData } [ "out2" :: MyData ]) -- out can depend on multiple ins
  (MyRowToRowProfunctor :: MyRowToRowProfunctor { "in3" :: MyData } [ "out3" :: MyData ]) -- all ins and outs must be covered
  (MyRowToRowProfunctor :: MyRowToRowProfunctor { "in3" :: MyData } [ "out1" :: MyData ]) -- out fields can be duplicated

variantToVariantExample :: MyRowToRowProfunctor
  [ in1 :: MyData, in2 :: MyData, in3 :: MyData ]
  [ out1 :: MyData, out2 :: MyData, out3 :: MyData ] -- notice that this type signature can be inferred from the expression
variantToVariantExample = VariantToVariant.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor [ "in1" :: MyData ] [ "out1" :: MyData ])
  (MyRowToRowProfunctor :: MyRowToRowProfunctor [ "in2" :: MyData, "in3" :: MyData ] [ "out1" :: MyData, "out2" :: MyData, "out3" :: MyData ])

variantToRecordExample :: MyRowToRowProfunctor
  [ in1 :: MyData, in2 :: MyData, in3 :: MyData ]
  { out1 :: MyData, out2 :: MyData, out3 :: MyData } -- notice that this type signature can be inferred from the expression
variantToRecordExample = VariantToRecord.do
  (MyRowToRowProfunctor :: MyRowToRowProfunctor [ "in1" :: MyData ] { "out1" :: MyData })
  (MyRowToRowProfunctor :: MyRowToRowProfunctor [ "in2" :: MyData, "in3" :: MyData ] { "out2" :: MyData, "out3" :: MyData })

-- Record-to-record (model in, optionally captures field)

text :: forall @l r. Cons l String () r => MyRowToRowProfunctor { | r } {}
text = MyRowToRowProfunctor

textInput :: forall @l r. Cons l String () r => MyRowToRowProfunctor { | r } { | r }
textInput = MyRowToRowProfunctor

checkbox :: forall @l r. Cons l Boolean () r => MyRowToRowProfunctor { | r } { | r }
checkbox = MyRowToRowProfunctor

slider :: forall @l r. Cons l Number () r => MyRowToRowProfunctor { | r } { | r }
slider = MyRowToRowProfunctor

dropdown :: forall @l a r. Cons l a () r => MyRowToRowProfunctor { | r } { | r }
dropdown = MyRowToRowProfunctor

image :: forall @l r. Cons l String () r => MyRowToRowProfunctor { | r } {}
image = MyRowToRowProfunctor

badge :: forall @l r. Cons l Int () r => MyRowToRowProfunctor { | r } {}
badge = MyRowToRowProfunctor

-- Record-to-variant (model in, fires event case)

button :: forall @l r v. Cons l { | r } () v => MyRowToRowProfunctor { | r } [ | v ]
button = MyRowToRowProfunctor

-- A no-data action button: reads nothing and fires case `l` with an empty payload
-- ({} ≅ Unit) — for actions like "cancel" that carry nothing.
actionButton :: forall @l v. Cons l {} () v => MyRowToRowProfunctor {} [ | v ]
actionButton = MyRowToRowProfunctor

-- A Shutter (× → +): the submit action as a loop step. Reads the whole form and
-- either fires the `done` case carrying it (Done → on to the backend) or snaps
-- back to the `loop` case with a prompt (Loop → return the form for correction).
-- Both output cases are caller-chosen labels. Built on `shutter` — no `(->)`
-- instance, because a pure function can't loop.
submit
  :: forall @done @loop r vl vd v
   . IsSymbol done
  => IsSymbol loop
  => Cons loop String () vl
  => Cons done { | r } vl v
  => Cons loop String vd v
  => MyRowToRowProfunctor { | r } [ | v ]
submit =
  shutter
    identity
    (inj (Proxy @done))
    (\_ -> inj (Proxy @loop) "review your details")
    MyRowToRowProfunctor

icon :: forall @l r v. Cons l { | r } () v => MyRowToRowProfunctor { | r } [ | v ]
icon = MyRowToRowProfunctor

link :: forall @l r v. Cons l String () v => MyRowToRowProfunctor { | r } [ | v ]
link = MyRowToRowProfunctor

menuItem :: forall @l r v. Cons l { | r } () v => MyRowToRowProfunctor { | r } [ | v ]
menuItem = MyRowToRowProfunctor

-- Variant-to-record (sum-shaped model in, optionally captures field)

-- A Reel (+ → ×): the page entity that *retains* its status across renders.
-- Built on `reel`; the carrier holds the state, so the dispatch only routes the
-- incoming case in (`Left`) — the retained channel is the do-nothing carrier's.
statusBar :: forall @l r. Cons l String () r => MyRowToRowProfunctor [ | r ] { | r }
statusBar = reel (\s -> Left s) (MyRowToRowProfunctor :: MyRowToRowProfunctor [ | r ] { | r })

-- A Reel (+ → ×): an event log that retains accumulated history — the same
-- stateful-entity shape as `statusBar`, also built on `reel`.
eventLog :: forall @l r. Cons l String () r => MyRowToRowProfunctor [ | r ] { | r }
eventLog = reel (\s -> Left s) (MyRowToRowProfunctor :: MyRowToRowProfunctor [ | r ] { | r })

outlet :: forall v. MyRowToRowProfunctor [ | v ] {}
outlet = MyRowToRowProfunctor

searchBar :: forall @l v r. Cons l String () r => MyRowToRowProfunctor [ | v ] { | r }
searchBar = MyRowToRowProfunctor

rating :: forall @l v r. Cons l Int () r => MyRowToRowProfunctor [ | v ] { | r }
rating = MyRowToRowProfunctor

-- Variant-to-variant (sum-shaped model in, fires event case)

-- Fully deferred: any input variant to any output variant, pinned at the use site.

notification :: forall v w. MyRowToRowProfunctor [ | v ] [ | w ]
notification = MyRowToRowProfunctor

modal :: forall v w. MyRowToRowProfunctor [ | v ] [ | w ]
modal = MyRowToRowProfunctor

-- A fake request/response dispatch to the backend: send whatever action variant `v`,
-- the response comes back on *some* set of cases `w` — both left deferred, since the
-- backend takes any action and one request may resolve to several outcomes (e.g.
-- `thankYou` or `failure`), inferred from downstream.
request :: forall v w. MyRowToRowProfunctor [ | v ] [ | w ]
request = MyRowToRowProfunctor

wizardStep :: forall @l v w. Cons l {} () w => MyRowToRowProfunctor [ | v ] [ | w ]
wizardStep = MyRowToRowProfunctor


-- Unary row-to-row combinator examples.
--
-- Each example pins the inferred type to confirm the row reshaping
-- works in both Record-to-* and *-to-Record/Variant directions.

widenRecordInputExample :: MyRowToRowProfunctor
  { in1 :: MyData, in2 :: MyData, in3 :: MyData, extra :: MyData }
  { out1 :: MyData, out2 :: MyData, out3 :: MyData }
widenRecordInputExample = widenRecordInput recordToRecordExample

narrowVariantInputExample :: MyRowToRowProfunctor
  [ in1 :: MyData, in2 :: MyData ]
  { out1 :: MyData, out2 :: MyData, out3 :: MyData }
narrowVariantInputExample = narrowVariantInput variantToRecordExample

narrowRecordOutputExample :: MyRowToRowProfunctor
  { in1 :: MyData, in2 :: MyData, in3 :: MyData }
  { out1 :: MyData, out2 :: MyData }
narrowRecordOutputExample = narrowRecordOutput recordToRecordExample

widenVariantOutputExample :: MyRowToRowProfunctor
  { in1 :: MyData, in2 :: MyData, in3 :: MyData }
  [ out1 :: MyData, out2 :: MyData, out3 :: MyData, extra :: MyData ]
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
