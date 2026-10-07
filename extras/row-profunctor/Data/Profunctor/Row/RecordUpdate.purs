-- | The **update merge** (`×→×`, experiment): operands fed one shared
-- | record, each writing the fields of its own output row into it, the
-- | block releasing the whole record.
-- |
-- | Where `RecordToRecord`'s operands own disjoint fields and the merge
-- | outputs exactly their union, an update block's operands may write any
-- | sub-row of the record — a display writes nothing (`{}`), an editor its
-- | field, a whole-row stage every field — and every field nobody writes
-- | passes through from the input. So a display needs no `# shown` to sit
-- | in a block, an editor and a display are siblings, and the block is
-- | again a whole-row stage:
-- |
-- | ```purescript
-- | RecordUpdate.do
-- |   headline4 (text countLine)
-- |   ( Semigroupoid.do
-- |     button @"Count" {}
-- |     snackbar @"Count" countedLine # fold increment )
-- | ```
-- |
-- | Overlapping writes resolve by time: the latest emission wins on the
-- | fields it declares, which is what lets a fold that resets an edited
-- | field share a block with that field's editor. On the timeless `(->)`
-- | the later operand wins.
-- |
-- | Every line of a block but its last may write any part of the record;
-- | the last is a whole-row stage (see `discard`).
-- |
-- | Laws, as far as the experiment states them: **answer** — a feed is
-- | released once, whole, after every operand was fed it; **write** — an
-- | emission outside a feed changes exactly the fields of its operand's
-- | declared output row and is released at once; **pass-through** — a
-- | field no operand declares is the fed one.
module Data.Profunctor.Row.RecordUpdate
  ( class RecordUpdate
  , recordUpdate
  , bind
  , discard
  ) where

import Data.Profunctor (class Profunctor)
import Data.Profunctor.Row (class FieldNames, exactRow)
import Data.Unit (Unit, unit)
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Record.Unsafe.Union (unsafeUnion)

class Profunctor p <= RecordUpdate p where
  recordUpdate
    :: forall r o1 o2 x1 x2 rl1 rl2
     . Union o1 x1 r
    => Union o2 x2 r
    => RowToList o1 rl1
    => FieldNames rl1 o1 o1
    => RowToList o2 rl2
    => FieldNames rl2 o2 o2
    => p { | r } { | o1 }
    -> p { | r } { | o2 }
    -> p { | r } { | r }

instance RecordUpdate (->) where
  recordUpdate f g r = unsafeUnion (exactRow (g r)) (unsafeUnion (exactRow (f r)) r)

bind
  :: forall p r o x rl rlr
   . RecordUpdate p
  => Union o x r
  => RowToList o rl
  => FieldNames rl o o
  => Union r () r
  => RowToList r rlr
  => FieldNames rlr r r
  => p { | r } { | o }
  -> (p { | r } { | o } -> p { | r } { | r })
  -> p { | r } { | r }
bind first cont = recordUpdate first (cont first)

-- | A block's lines but its last may write any part of the record; the last
-- | line is a whole-row stage — in the app's loop, the event chain whose
-- | folds release the next model — so the row every fold returns stays the
-- | model the knot declares (guardrails L18).
discard
  :: forall p r o x rl rlr
   . RecordUpdate p
  => Union o x r
  => RowToList o rl
  => FieldNames rl o o
  => Union r () r
  => RowToList r rlr
  => FieldNames rlr r r
  => p { | r } { | o }
  -> (Unit -> p { | r } { | r })
  -> p { | r } { | r }
discard first cont = recordUpdate first (cont unit)
