-- | `Record → Record` row profunctors, in three layers:
-- |
-- |   * `recordToRecord` — the n-ary **merge** class: combine two complete record-shaped
-- |     sub-profunctors (share inputs, disjoin outputs).
-- |   * `StrongRecordToRecord`/`focusRecord` — the row-typed **`Strong`**: focus a whole
-- |     sub-record, carrying the complement (`first`/`second`, relabeled to rows).
-- |   * `introduceProperty`/`eliminateProperty`/`property` — the single-field
-- |     **combinators** built on `StrongRecordToRecord`.
module Data.Profunctor.Row.RecordToRecord
  ( bind
  , recordToRecord
  , class RecordToRecord
  , discard
  , class StrongRecordToRecord
  , focusRecord
  , lensE
  , lensProperty
  , introduceProperty
  , eliminateProperty
  , property
  , withRecordDefault
  , withRecordOutputDefault
  )
  where

import Data.Function (const)
import Data.Lens (Lens, Optic)
import Data.Lens.Record (prop)
import Data.Profunctor (class Profunctor, dimap, lcmap, rmap)
import Data.Profunctor.Strong (class Strong, first, second)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), snd)
import Data.Unit (Unit, unit)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList as RL
import Record (delete, get, insert)
import Record (union) as Record
import Type.Proxy (Proxy(..))
import Type.Row.Constraints (class ExclusiveRows, class InclusiveRows)
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= RecordToRecord p where
  recordToRecord :: forall i1 o1 i2 o2 i12 i1x i2x i o.
    InclusiveRows i1 i2 i i12 i1x i2x =>
    ExclusiveRows o1 o2 o =>
    p { | i1 } { | o1 } -> p { | i2 } { | o2 } -> p { | i } { | o }

bind :: forall f i1 o1 i2 o2 i12 i1x i2x i o.
  RecordToRecord f =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  ExclusiveRows o1 o2 o =>
  f { | i1 } { | o1 } -> (f { | i1 } { | o1 } -> f { | i2 } { | o2 }) -> f { | i } { | o }
bind first cont = recordToRecord first (cont first)

discard :: forall f i1 o1 i2 o2 i12 i1x i2x i o.
  RecordToRecord f =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  ExclusiveRows o1 o2 o =>
  f { | i1 } { | o1 } -> (Unit -> f { | i2 } { | o2 }) -> f { | i } { | o }
discard first cont = bind first (\_ -> cont unit)

-- | Row-typed `Strong`: focus a **sub-record** `sub`, transforming it while carrying the
-- | complement `rest` of the row unchanged. Operates on rows on **both sides** — the
-- | argument is itself a `Record → Record` profunctor:
-- |
-- | ```
-- | focusRecord :: p { | sub } { | sub' } -> p { | s } { | t }
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
    => p { | sub } { | sub' }
    -> p { | s } { | t }

instance Strong p => StrongRecordToRecord p where
  focusRecord g =
    dimap (\s -> Tuple (unsafeCoerce s) (unsafeCoerce s))
          -- `Record.union` is left-biased and does not nub; safe here only because
          -- `ExclusiveRows sub' rest t` guarantees `sub'` and `rest` are disjoint.
          (\(Tuple sub' rest) -> Record.union sub' rest)
          (first g)

-- | Construct a `Lens` straight from its **existential encoding**
-- | `∃c. (s → a × c) × (b × c → t)`: pick the residual `c`, then supply `decon`
-- | (split `s` into a focus `a` and the complement `c`) and `recon` (rebuild `t`
-- | from the new focus `b` and that same complement `c`). The quantified `c` is
-- | the eliminator of that existential; `first` (`Strong`) is the carrier that
-- | threads `c`. The standard `Data.Lens.lens` is this at the co-Yoneda witness
-- | `c := s`. Mirror of `shutterE` (resolve), `reelE` (retain), `prismE` (left).
lensE :: forall s t a b c. (s -> Tuple a c) -> (Tuple b c -> t) -> Lens s t a b
lensE decon recon g = dimap decon recon (first g)

-- | The single-field **row** existential lens for label `l`, type-changing: the
-- | focus is field `l` (`a → b`) and the residual `c` is the **rest of the
-- | record** — a sub-Record. Built via `lensE` at `c := { | rest }`. The row
-- | counterpart of the generic `lensE`; `property` is its monomorphic,
-- | `prop`-based cousin.
lensProperty
  :: forall @l s t a b rest
   . IsSymbol l
  => Cons l a rest s
  => Cons l b rest t
  => Lacks l rest
  => Lens { | s } { | t } a b
lensProperty =
  lensE
    (\r -> Tuple (get (Proxy @l) r) (delete (Proxy @l) r))
    (\(Tuple b rest) -> insert (Proxy @l) b rest)

-- | Introduce a new field `l :: prop`, computing its value from the whole record `s`
-- | (the `p s r` shape). `id &&& f` followed by `insert`.
introduceProperty
  :: forall p @l prop s t
   . IsSymbol l
  => Cons l prop s t
  => Lacks l s
  => StrongRecordToRecord p
  => Optic p { | s } { | t } { | s } prop
introduceProperty f =
  dimap (\s -> Tuple s s) (\(Tuple s p) -> insert (Proxy @l) p s) (second f)

-- | Eliminate the field `l :: prop`, feeding its value to a sink `p prop Unit` and keeping
-- | the rest. The sink's output is `Unit` — we discard it (via `snd`), and the monomorphic
-- | type makes that explicit. The transpose of `introduceProperty`: `first` + `delete`.
eliminateProperty
  :: forall p @l prop s t
   . IsSymbol l
  => Cons l prop t s
  => Lacks l t
  => StrongRecordToRecord p
  => Optic p { | s } { | t } prop Unit
eliminateProperty f =
  dimap (\s -> Tuple (get (Proxy @l) s) (delete (Proxy @l) s)) snd (first f)

-- | Edit an existing field in place — the standard `Strong` field lens.
property
  :: forall @l p s r a
   . IsSymbol l
  => Cons l a r s
  => Strong p
  => p a a -> p { | s } { | s }
property = prop (Proxy @l)

-- UI: seed a single-field input with an initial value. A widget that needs
-- a record field to display (e.g. `textInput @"name"`) becomes one needing
-- no input data — the default is shown initially and user edits flow back
-- via `o`. The default is consumed on every render.
-- Lifts `p { l :: a } o` into `p {} o`.
withRecordDefault :: forall l p a r o.
  RL.RowToList r (RL.Cons l a RL.Nil) =>
  IsSymbol l =>
  Cons l a () r =>
  Profunctor p =>
  p { | r } o -> a -> p {} o
withRecordDefault p default = lcmap (const (insert (Proxy :: Proxy l) default {})) p

-- UI: promote a read-only widget into a form contributor. A display-only
-- element like `textOutput` or `icon` that captures nothing gets lifted to
-- one that emits a fixed singleton record on every call — useful for static
-- fields like auto-IDs, hidden constants, or computed values the form layer
-- needs. The default is emitted on every render.
-- Lifts `p i {}` into `p i { l :: a }`.
withRecordOutputDefault :: forall l p a r i.
  RL.RowToList r (RL.Cons l a RL.Nil) =>
  IsSymbol l =>
  Cons l a () r =>
  Profunctor p =>
  p i {} -> a -> p i { | r }
withRecordOutputDefault p default = rmap (const (insert (Proxy :: Proxy l) default {})) p
