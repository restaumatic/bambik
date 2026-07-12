-- | A phantom carrier exercising the row-profunctor API shape — no runtime,
-- | just instances and label-indexed widget signatures. See showcase/App.purs
-- | for a four-direction pipeline written against it.
module Data.Profunctor.Row.Example
  ( MyRowToRowProfunctor
  , actionButton
  , checkbox
  , eventLog
  , modal
  , notification
  , request
  , statusBar
  , submit
  , textInput
  )
  where

import Prelude

import Data.Profunctor (class Profunctor)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord)
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant)
import Data.Profunctor.Row.VariantToRecord (class VariantToRecord)
import Data.Profunctor.Row.VariantToVariant (class VariantToVariant)
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons)

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
  pempty = MyRowToRowProfunctor

instance RecordToVariant MyRowToRowProfunctor where
  recordToVariant MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor
  pempty = MyRowToRowProfunctor

instance VariantToRecord MyRowToRowProfunctor where
  variantToRecord MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor
  pempty = MyRowToRowProfunctor

instance VariantToVariant MyRowToRowProfunctor where
  variantToVariant MyRowToRowProfunctor MyRowToRowProfunctor = MyRowToRowProfunctor
  pempty = MyRowToRowProfunctor

-- rule of thumb:
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

-- Record-to-record (model in, optionally captures field)

textInput :: forall @l r. Cons l String () r => MyRowToRowProfunctor { | r } { | r }
textInput = MyRowToRowProfunctor

checkbox :: forall @l r. Cons l Boolean () r => MyRowToRowProfunctor { | r } { | r }
checkbox = MyRowToRowProfunctor

-- Record-to-variant (model in, fires event case)

-- The submit action: reads the whole form and either fires the `done` case
-- carrying it, or the `loop` case with a prompt (return the form for
-- correction). Both output cases are caller-chosen labels.
submit
  :: forall @done @loop r vl vd v
   . IsSymbol done
  => IsSymbol loop
  => Cons loop String () vl
  => Cons done { | r } vl v
  => Cons loop String vd v
  => MyRowToRowProfunctor { | r } [ | v ]
submit = MyRowToRowProfunctor

-- A no-data action button: reads nothing and fires case `l` with an empty payload
-- ({} ≅ Unit) — for actions like "cancel" that carry nothing.
actionButton :: forall @l v. Cons l {} () v => MyRowToRowProfunctor {} [ | v ]
actionButton = MyRowToRowProfunctor

-- Variant-to-record (sum-shaped model in, optionally captures field)

statusBar :: forall @l r. Cons l String () r => MyRowToRowProfunctor [ | r ] { | r }
statusBar = MyRowToRowProfunctor

eventLog :: forall @l r. Cons l String () r => MyRowToRowProfunctor [ | r ] { | r }
eventLog = MyRowToRowProfunctor

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
