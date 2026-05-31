-- | `Record → Record` row profunctors, in three layers:
-- |
-- |   * `recordToRecord` — the n-ary **merge** class: combine two complete record-shaped
-- |     sub-profunctors (share inputs, disjoin outputs).
-- |   * `StrongRecordToRecord`/`focusRecord` — the row-typed **`Strong`**: focus a whole
-- |     sub-record, carrying the complement (`first`/`second`, relabeled to rows).
-- |   * `introduceProperty`/`eliminateProperty`/`editProperty` — the single-field
-- |     **combinators** built on `StrongRecordToRecord`.
module Data.Profunctor.Row.RecordToRecord
  ( bind
  , recordToRecord
  , class RecordToRecord
  , discard
  , class StrongRecordToRecord
  , focusRecord
  , introduceProperty
  , eliminateProperty
  , editProperty
  )
  where

import Data.Lens (Lens, Optic)
import Data.Lens.Extra.Commons (property) as Commons
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Strong (class Strong, first, second)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), snd)
import Data.Unit (Unit, unit)
import Prim.Row (class Cons, class Lacks, class Union)
import Record (delete, get, insert)
import Record (union) as Record
import Type.Proxy (Proxy(..))
import Type.Row.Constraints (class ExclusiveRows, class InclusiveRows)
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= RecordToRecord p where
  recordToRecord :: forall i1 o1 i2 o2 i12 i1x i2x i o.
    InclusiveRows i1 i2 i i12 i1x i2x =>
    ExclusiveRows o1 o2 o =>
    p (Record i1) (Record o1) -> p (Record i2) (Record o2) -> p (Record i) (Record o)

bind :: forall f i1 o1 i2 o2 i12 i1x i2x i o.
  RecordToRecord f =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  ExclusiveRows o1 o2 o =>
  f (Record i1) (Record o1) -> (f (Record i1) (Record o1) -> f (Record i2) (Record o2)) -> f (Record i) (Record o)
bind first cont = recordToRecord first (cont first)

discard :: forall f i1 o1 i2 o2 i12 i1x i2x i o.
  RecordToRecord f =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  ExclusiveRows o1 o2 o =>
  f (Record i1) (Record o1) -> (Unit -> f (Record i2) (Record o2)) -> f (Record i) (Record o)
discard first cont = bind first (\_ -> cont unit)

-- | Row-typed `Strong`: focus a **sub-record** `sub`, transforming it while carrying the
-- | complement `rest` of the row unchanged. Operates on rows on **both sides** — the
-- | argument is itself a `Record → Record` profunctor:
-- |
-- | ```
-- | focusRecord :: p (Record sub) (Record sub') -> p (Record s) (Record t)
-- |              -- where s = sub ∪ rest,  t = sub' ∪ rest   (ExclusiveRows)
-- | ```
-- |
-- | The labeled analogue of `Strong`'s `first`/`second`: instead of carrying a positional
-- | complement `c`, it carries the complement *row* `rest`, split off by `ExclusiveRows`.
-- | Equivalent to `Strong` (generic instance below): split `s` into `(sub, rest)`, run the
-- | argument on `sub` via `first`, and re-merge `sub'` with `rest`.
class Strong p <= StrongRecordToRecord p where
  focusRecord
    :: forall sub sub' rest s t
     . ExclusiveRows sub rest s
    => ExclusiveRows sub' rest t
    => p (Record sub) (Record sub')
    -> p (Record s) (Record t)

instance Strong p => StrongRecordToRecord p where
  focusRecord g =
    dimap (\s -> Tuple (pick s) (pick s))
          -- `Record.union` is left-biased and does not nub; safe here only because
          -- `ExclusiveRows sub' rest t` guarantees `sub'` and `rest` are disjoint.
          (\(Tuple sub' rest) -> Record.union sub' rest)
          (first g)

-- Project a sub-record out of a wider record. Sound because PureScript records are JS
-- objects and `Union narrow extra wider` witnesses `narrow ⊆ wider`.
pick :: forall narrow extra wider. Union narrow extra wider => Record wider -> Record narrow
pick = unsafeCoerce

-- | Introduce a new field `l :: prop`, computing its value from the whole record `s`
-- | (the `p s r` shape). `id &&& f` followed by `insert`.
introduceProperty
  :: forall p @l prop s t
   . IsSymbol l
  => Cons l prop s t
  => Lacks l s
  => StrongRecordToRecord p
  => Optic p (Record s) (Record t) (Record s) prop
introduceProperty f =
  dimap (\s -> Tuple s s) (\(Tuple s p) -> insert (Proxy @l) p s) (second f)

-- | Eliminate the field `l :: prop`, feeding its value to a sink and keeping the rest.
-- | The transpose of `introduceProperty`: `first` + `delete`.
eliminateProperty
  :: forall p @l prop s t x
   . IsSymbol l
  => Cons l prop t s
  => Lacks l t
  => StrongRecordToRecord p
  => Optic p (Record s) (Record t) prop x
eliminateProperty f =
  dimap (\s -> Tuple (get (Proxy @l) s) (delete (Proxy @l) s)) snd (first f)

-- | Edit an existing field in place — the standard `Strong` field lens.
editProperty
  :: forall @l s r a
   . IsSymbol l
  => Cons l a r s
  => Lens (Record s) (Record s) a a
editProperty = Commons.property @l
