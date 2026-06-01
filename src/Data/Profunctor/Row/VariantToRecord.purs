module Data.Profunctor.Row.VariantToRecord
  ( bind
  , variantToRecord
  , class VariantToRecord
  , discard
  , narrowVariantToRecord
  )
  where

import Data.Profunctor (class Profunctor, dimap)
import Data.Unit (Unit, unit)
import Data.Variant (Variant, expand)
import Prim.Row (class Union)
import Type.Row.Constraints (class DispatchableVariants, class ExclusiveRows)
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= VariantToRecord p where
  variantToRecord :: forall i1 i1l i2 i2l o1 o2 i o.
    ExclusiveRows i1 i2 i =>
    ExclusiveRows o1 o2 o =>
    DispatchableVariants i1 i2 i1l i2l =>
    p (Variant i1) (Record o1) -> p (Variant i2) (Record o2) -> p (Variant i) (Record o)

bind :: forall f i1 i1l i2 i2l o1 o2 i o.
  VariantToRecord f =>
  ExclusiveRows i1 i2 i =>
  ExclusiveRows o1 o2 o =>
  DispatchableVariants i1 i2 i1l i2l =>
  f (Variant i1) (Record o1) -> (f (Variant i1) (Record o1) -> f (Variant i2) (Record o2)) -> f (Variant i) (Record o)
bind first cont = variantToRecord first (cont first)

discard :: forall f i1 i1l i2 i2l o1 o2 i o.
  VariantToRecord f =>
  ExclusiveRows i1 i2 i =>
  ExclusiveRows o1 o2 o =>
  DispatchableVariants i1 i2 i1l i2l =>
  f (Variant i1) (Record o1) -> (Unit -> f (Variant i2) (Record o2)) -> f (Variant i) (Record o)
discard first cont = bind first (\_ -> cont unit)

-- | The free counterpart of `widenRecordToVariant` for the `Variant → Record` shape, and
-- | its exact categorical **dual**: reversing the arrows of `dimap pickRecord expand` swaps
-- | the two maps, which also reverses the direction — so this one *narrows* both sides
-- | instead of widening them:
-- |
-- | ```
-- | narrowVariantToRecord :: p (Variant s) (Record t) -> p (Variant sub) (Record subO)
-- |                        -- where sub ⊆ s,  subO ⊆ t   (Union)
-- | ```
-- |
-- | View a profunctor over the *whole* input variant and *whole* output record on a
-- | sub-variant / sub-record. `Profunctor`-only, both maps free: `expand` injects the `sub`
-- | input case up into `s` (the `rest` cases are never supplied — phantom-in), and
-- | `pickRecord` drops the `restOut` output fields (discard-out). Equal to
-- | `narrowVariantInput ∘ narrowRecordOutput` from `Data.Profunctor.Row`.
-- |
-- | The direction is **reversed** from `widenRecordToVariant`: that one widens (`sub →
-- | wider`) for free, this one narrows (`wider → sub`). The reason is variance — `Variant →
-- | Record` sits on the narrow/narrow free side of both row disciplines, the mirror of
-- | `Record → Variant`'s widen/widen side. The opposite (widening) direction here is **not**
-- | derivable: it would need a fallback for the extra input cases (widen-variant-in) *and*
-- | defaults for the extra output fields (widen-record-out) — the two irreducible
-- | data-fabricating operations. Supplying that complement handler *is* the binary
-- | `variantToRecord` merge, which is why this mixed kind admits no free unary widening.
narrowVariantToRecord
  :: forall p sub rest s subO restOut t
   . Profunctor p
  => Union sub rest s
  => Union subO restOut t
  => p (Variant s) (Record t)
  -> p (Variant sub) (Record subO)
narrowVariantToRecord = dimap expand pickRecord

-- Project a sub-record out of a wider record. Sound because PureScript records are JS
-- objects and `Union subO restOut t` witnesses `subO ⊆ t`. (Local copy to avoid the import
-- cycle through `Data.Profunctor.Row`, which depends on this module.)
pickRecord :: forall narrow extra wider. Union narrow extra wider => Record wider -> Record narrow
pickRecord = unsafeCoerce
