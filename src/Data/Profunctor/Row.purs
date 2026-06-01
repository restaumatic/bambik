module Data.Profunctor.Row
  ( class Row
  , pickRecord
  , variantToMaybeRecord
  , class MaybeifyRow
  , class MaybeifyRowList
  , defaultMaybeRecord
  , widenRecordInput
  , narrowVariantInput
  , narrowRecordOutput
  , widenVariantOutput
  , widenRecordToVariant
  , narrowVariantToRecord
  , variantInputAsMaybeRecord
  , variantOutputAsMaybeRecord
  )
  where

import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor, dimap, lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord)
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant)
import Data.Profunctor.Row.VariantToRecord (class VariantToRecord)
import Data.Profunctor.Row.VariantToVariant (class VariantToVariant)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, expand)
import Data.Variant.Internal (VariantCase, VariantRep(..))
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Prim.RowList (class RowToList, RowList, Cons, Nil) as RL
import Record (insert) as Record
import Record.Unsafe (unsafeSet) as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class (RecordToRecord p, RecordToVariant p, VariantToRecord p, VariantToVariant p) <= Row p

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

-- ---------------------------------------------------------------------
-- Both-sides reshapings for the two mixed shapes.
--
-- These touch *both* sides at once (a single `dimap`), and the free
-- direction is forced by variance: `Record → Variant` sits on the
-- widen/widen side, `Variant → Record` on the narrow/narrow side.
--
-- They are *reshapes*, not *focuses* — two orthogonal axes:
--   * direction  — widen (grow) vs narrow (shrink)
--   * complement — a reshape drops the complement (pure `dimap`,
--                  `Profunctor`-only); a focus (`focusRecord`/
--                  `focusVariant`) threads it across, needing strength.
-- `focusRecord` is itself a widen that *also* threads the complement, so
-- the contrast with `widenRecordToVariant` is the complement axis, not
-- direction. The through-threading focus does not exist for mixed kinds
-- (a product complement has no image in a sum one), so the mixed shapes
-- get only the complement-free reshape below. The opposite direction in
-- each (narrowing `Record → Variant`, widening `Variant → Record`) needs
-- defaults/fallbacks and is the irreducible binary merge instead.
-- See doc/row-profunctors.md, "Reshape vs focus: two axes, not a trio".
-- ---------------------------------------------------------------------

-- Widen both sides at once: `sub → s` on input, `subO → t` on output.
-- = `widenVariantOutput ∘ widenRecordInput`.
widenRecordToVariant :: forall p sub rest s subO restOut t.
  Profunctor p =>
  Row.Union sub rest s =>
  Row.Union subO restOut t =>
  p (Record sub) (Variant subO) -> p (Record s) (Variant t)
widenRecordToVariant = dimap pickRecord expand

-- Narrow both sides at once: `s → sub` on input, `t → subO` on output —
-- the categorical dual of `widenRecordToVariant` (arrows reversed swaps
-- the maps and flips the direction). = `narrowVariantInput ∘ narrowRecordOutput`.
narrowVariantToRecord :: forall p sub rest s subO restOut t.
  Profunctor p =>
  Row.Union sub rest s =>
  Row.Union subO restOut t =>
  p (Variant s) (Record t) -> p (Variant sub) (Record subO)
narrowVariantToRecord = dimap expand pickRecord

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
