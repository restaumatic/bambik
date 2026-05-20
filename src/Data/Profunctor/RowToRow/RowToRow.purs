module Data.Profunctor.RowToRow.RowToRow
  ( class RowToRow
  , pickRecord
  , variantToMaybeRecord
  , class MaybeifyRow
  , class MaybeifyRowList
  , defaultMaybeRecord
  , widenRecordInput
  , narrowVariantInput
  , narrowRecordOutput
  , widenVariantOutput
  , variantInputAsMaybeRecord
  , variantOutputAsMaybeRecord
  )
  where

import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Profunctor.RowToRow.RecordToRecord (class RecordToRecord)
import Data.Profunctor.RowToRow.RecordToVariant (class RecordToVariant)
import Data.Profunctor.RowToRow.VariantToRecord (class VariantToRecord)
import Data.Profunctor.RowToRow.VariantToVariant (class VariantToVariant)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, expand)
import Data.Variant.Internal (VariantCase, VariantRep(..))
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Prim.RowList (class RowToList, RowList, Cons, Nil) as RL
import Record (insert) as Record
import Record.Unsafe (unsafeSet) as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class (RecordToRecord p, RecordToVariant p, VariantToRecord p, VariantToVariant p) <= RowToRow p

-- =====================================================================
-- Primitive value-level row reshapings.
--
-- Exported so the binary `recordToRecord` / `recordToVariant` /
-- `variantToRecord` / `variantToVariant` instances in `UI.purs` route
-- through the same helpers as the unary combinators below.
-- =====================================================================

-- Drop the fields named in `extra` from a `Record wider`, yielding the
-- `narrow` projection. Sound because PureScript records are JS objects
-- and `Union narrow extra wider` witnesses `narrow ⊆ wider`.
pickRecord :: forall narrow extra wider. Row.Union narrow extra wider => Record wider -> Record narrow
pickRecord = unsafeCoerce

-- Total Variant → Maybe-record translation: exactly one field is `Just`,
-- the rest are `Nothing`. Uses VariantRep's runtime `{type, value}` shape.
variantToMaybeRecord
  :: forall v vl mv
   . RL.RowToList v vl
  => MaybeifyRowList vl mv
  => Variant v
  -> Record mv
variantToMaybeRecord v =
  let VariantRep rep = (unsafeCoerce :: Variant v -> VariantRep VariantCase) v
  in Record.unsafeSet rep.type (Just rep.value) (defaultMaybeRecord (Proxy :: Proxy vl))

-- Type-level: maps a variant row `v` to a record row `mv` where every
-- field `name :: a` becomes `name :: Maybe a`.
class MaybeifyRow :: Row Type -> Row Type -> Constraint
class MaybeifyRow v mv | v -> mv

instance (RL.RowToList v vl, MaybeifyRowList vl mv) => MaybeifyRow v mv

class MaybeifyRowList :: RL.RowList Type -> Row Type -> Constraint
class MaybeifyRowList vl mv | vl -> mv where
  defaultMaybeRecord :: Proxy vl -> Record mv

instance MaybeifyRowList RL.Nil () where
  defaultMaybeRecord _ = {}

instance
  ( IsSymbol name
  , MaybeifyRowList tl tlMv
  , Row.Cons name (Maybe a) tlMv mv
  , Row.Lacks name tlMv
  ) => MaybeifyRowList (RL.Cons name a tl) mv where
  defaultMaybeRecord _ =
    Record.insert (Proxy :: Proxy name) (Nothing :: Maybe a) (defaultMaybeRecord (Proxy :: Proxy tl))

-- =====================================================================
-- Unary row-to-row transformations, derivable from `dimap` alone.
--
-- Naming: `<direction><RowKind><Side>`. `widen` enlarges a row,
-- `narrow` shrinks it. Each combinator is parametric in the side it
-- doesn't touch, so each covers two of the four R/V × R/V shapes.
--
-- The four transformations NOT provided here cannot be derived from
-- `dimap` alone:
--   * widening Record output (needs defaults for the extra fields)
--   * narrowing Variant output (needs a fallback for discarded cases)
--   * Record (Maybeify v) -> Variant v in either direction (partial)
-- =====================================================================

widenRecordInput :: forall p narrow extra wider o.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p (Record narrow) o -> p (Record wider) o
widenRecordInput = lcmap pickRecord

narrowVariantInput :: forall p narrow extra wider o.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p (Variant wider) o -> p (Variant narrow) o
narrowVariantInput = lcmap expand

narrowRecordOutput :: forall p i narrow extra wider.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p i (Record wider) -> p i (Record narrow)
narrowRecordOutput = rmap pickRecord

widenVariantOutput :: forall p i narrow extra wider.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p i (Variant narrow) -> p i (Variant wider)
widenVariantOutput = rmap expand

variantInputAsMaybeRecord :: forall p v vl mv o.
  Profunctor p =>
  RL.RowToList v vl =>
  MaybeifyRowList vl mv =>
  p (Record mv) o -> p (Variant v) o
variantInputAsMaybeRecord = lcmap variantToMaybeRecord

variantOutputAsMaybeRecord :: forall p i v vl mv.
  Profunctor p =>
  RL.RowToList v vl =>
  MaybeifyRowList vl mv =>
  p i (Variant v) -> p i (Record mv)
variantOutputAsMaybeRecord = rmap variantToMaybeRecord
