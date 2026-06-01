module Data.Profunctor.Row.RecordToVariant
  ( bind
  , class RecordToVariant
  , discard
  , recordToVariant
  , widenRecordToVariant
  )
  where

import Data.Profunctor (class Profunctor, dimap)
import Data.Unit (Unit, unit)
import Data.Variant (Variant, expand)
import Prim.Row (class Union)
import Type.Row.Constraints (class InclusiveRows)
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= RecordToVariant p where
  recordToVariant :: forall i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
    InclusiveRows i1 i2 i i12 i1x i2x =>
    InclusiveRows o1 o2 o o12 o1x o2x =>
    p (Record i1) (Variant o1) -> p (Record i2) (Variant o2) -> p (Record i) (Variant o)

bind :: forall f i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
  RecordToVariant f =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  InclusiveRows o1 o2 o o12 o1x o2x =>
  f (Record i1) (Variant o1) -> (f (Record i1) (Variant o1) -> f (Record i2) (Variant o2)) -> f (Record i) (Variant o)
bind first cont = recordToVariant first (cont first)

discard :: forall f i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
  RecordToVariant f =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  InclusiveRows o1 o2 o o12 o1x o2x =>
  f (Record i1) (Variant o1) -> (Unit -> f (Record i2) (Variant o2)) -> f (Record i) (Variant o)
discard first cont = bind first (\_ -> cont unit)

-- | Widen both sides of a `Record → Variant` profunctor at once: enlarge the input record
-- | from `sub` to `s` and the output variant from `subO` to `t`, where `sub ⊆ s` and
-- | `subO ⊆ t`:
-- |
-- | ```
-- | widenRecordToVariant :: p (Record sub) (Variant subO) -> p (Record s) (Variant t)
-- | ```
-- |
-- | This is a *reshape*, not a *focus* — two different axes (see the note below). It carries
-- | no complement across the input→output boundary, so it needs only `Profunctor` — not
-- | `Strong`/`Choice`. The two complements are inert: the extra input fields are **dropped**
-- | on the way in (`pickRecord`), and the extra output cases are **phantom** — never emitted,
-- | only widened into the result type (`expand`).
-- |
-- | Two axes, not a flat widen/narrow/focus trio:
-- |
-- |   * *direction* — widen (grow, `sub → wider`) vs narrow (shrink). `Variant → Record`'s
-- |     free reshape narrows; see `narrowVariantToRecord`.
-- |   * *complement* — a *reshape* (this) drops the complement (pure `dimap`); a *focus*
-- |     (`focusRecord`/`focusVariant`) threads it across, hence needs strength.
-- |
-- | `focusRecord` is *itself* a widen-direction operation — it just also threads `rest`. So
-- | the real contrast with it is the complement axis, not direction. The through-threading
-- | focus does not exist for mixed kinds: a product complement has no image in a sum one, so
-- | the mixed shapes get only the complement-free reshape.
-- |
-- | Equal to `dimap pickRecord expand`; i.e. `widenVariantOutput ∘ widenRecordInput` from
-- | `Data.Profunctor.Row`, packaged for the `Record → Variant` shape.
widenRecordToVariant
  :: forall p sub rest s subO restOut t
   . Profunctor p
  => Union sub rest s
  => Union subO restOut t
  => p (Record sub) (Variant subO)
  -> p (Record s) (Variant t)
widenRecordToVariant = dimap pickRecord expand

-- Project a sub-record out of a wider record. Sound because PureScript records are JS
-- objects and `Union sub rest s` witnesses `sub ⊆ s`. (Local copy to avoid the import
-- cycle through `Data.Profunctor.Row`, which depends on this module.)
pickRecord :: forall narrow extra wider. Union narrow extra wider => Record wider -> Record narrow
pickRecord = unsafeCoerce
