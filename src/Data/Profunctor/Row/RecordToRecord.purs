-- | `Record → Record` row profunctors, organized (uniformly across the four
-- | direction modules) as:
-- |
-- |   * **strength** — `Strong` (ecosystem class, imported): the unary power,
-- |     minimal and interop-friendly.
-- |   * **direction class** — `RecordToRecord`, the binary **merge**: the one
-- |     genuine per-carrier primitive.
-- |   * **free functions over the strength** — everything else: `focusRecord`
-- |     (sub-record focus), `property` (field lens), `recordToProperty`/
-- |     `eliminateProperty` (grow/drop one field), `lensE`, the defaults.
-- |
-- | Law connecting the two classes, for carriers with `identity :: p a a`:
-- | the unary introduce operator is the **identity-pinned merge**,
-- |
-- | ```
-- | recordToProperty @l g = recordToRecord identity (rmap (\f -> insert (Proxy @l) f {}) g)
-- | ```
-- |
-- | and conversely a merge is an iterated chain of single-field steps
-- | (see doc/row-profunctors.md, "The precise correspondence").
-- |
-- | Completing the arity ladder downward, the **nullary** operator is the
-- | class's own unit `pempty :: p {} {}` — the empty merge:
-- |
-- | ```
-- | recordToRecord pempty g = g = recordToRecord g pempty
-- | ```
-- |
-- | It is a class member (not a parametric silent element like `UI`'s
-- | `mempty`) because a lawful record-output unit must *announce* its
-- | contribution — the informationless `{}` — to the merge machinery, and
-- | anything typed `forall a b. p a b` is silent by parametricity (it can
-- | never fabricate a `b`). For `Category` carriers, `pempty = identity @{}`.
module Data.Profunctor.Row.RecordToRecord
  ( bind
  , recordToRecord
  , class RecordToRecord
  , discard
  , pempty
  , focusRecord
  , lensE
  , recordToProperty
  , eliminateProperty
  , property
  , withRecordDefault
  , withRecordOutputDefault
  )
  where

import Data.Function (const)
import Data.Lens (Lens)
import Data.Lens.Record (prop)
import Data.Profunctor (class Profunctor, dimap, lcmap, rmap)
import Data.Profunctor.Strong (class Strong, first, second)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..), snd)
import Data.Unit (Unit, unit)
import Prim.Row (class Cons)
import Prim.RowList as RL
import Record (get, insert)
import Record (union) as Record
import Record.Unsafe (unsafeDelete, unsafeSet)
import Type.Proxy (Proxy(..))
import Data.Profunctor.Row (class ExclusiveRows, class InclusiveRows)
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= RecordToRecord p where
  recordToRecord :: forall i1 o1 i2 o2 i12 i1x i2x i o.
    InclusiveRows i1 i2 i i12 i1x i2x =>
    ExclusiveRows o1 o2 o =>
    p { | i1 } { | o1 } -> p { | i2 } { | o2 } -> p { | i } { | o }
  -- | The **nullary** merge — the unit: reads nothing, contributes no fields.
  -- | Genuinely per-carrier: a parametric silent element cannot serve, because
  -- | a record-output unit must *announce* its informationless `{}` so the
  -- | merge machinery knows that side is complete. For `Category` carriers,
  -- | `pempty = identity @{}`.
  pempty :: p {} {}

bind :: forall p i1 o1 i2 o2 i12 i1x i2x i o.
  RecordToRecord p =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  ExclusiveRows o1 o2 o =>
  p { | i1 } { | o1 } -> (p { | i1 } { | o1 } -> p { | i2 } { | o2 }) -> p { | i } { | o }
bind first cont = recordToRecord first (cont first)

discard :: forall p i1 o1 i2 o2 i12 i1x i2x i o.
  RecordToRecord p =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  ExclusiveRows o1 o2 o =>
  p { | i1 } { | o1 } -> (Unit -> p { | i2 } { | o2 }) -> p { | i } { | o }
discard first cont = bind first (\_ -> cont unit)

-- | Row-typed `Strong`: focus a whole **sub-record** — the row-valued **focus**
-- | `f` — transforming it against the **background** `b`, which is carried
-- | unchanged. The **shot** `s` is refocused to `s'`. Operates on rows on
-- | **both sides** — the argument is itself a `Record → Record` profunctor:
-- |
-- | ```
-- | focusRecord :: p { | f } { | f' } -> p { | s } { | s' }
-- |              -- where s = f ∪ b,  s' = f' ∪ b   (ExclusiveRows)
-- | ```
-- |
-- | The labeled analogue of `Strong`'s `first`/`second`: instead of carrying a positional
-- | complement `c`, it carries the background *row* `b`, split off by `ExclusiveRows`.
-- | Plain `Strong` underneath: split `s` into `(f, b)`, run the argument on `f`
-- | via `first`, and re-merge `f'` with `b`.
focusRecord
  :: forall p f f' b s s'
   . Strong p
  => ExclusiveRows f b s
  => ExclusiveRows f' b s'
  => p { | f } { | f' }
  -> p { | s } { | s' }
focusRecord g =
  dimap (\s -> Tuple (unsafeCoerce s) (unsafeCoerce s))
        -- `Record.union` is left-biased and does not nub; safe here only because
        -- `ExclusiveRows f' b s'` guarantees `f'` and `b` are disjoint.
        (\(Tuple f' b) -> Record.union f' b)
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

-- | Introduce a new field `l :: f` (the **focus**), computed from the whole
-- | record (the **background** `b`) by the wrapped `p { | b } f`; the result is
-- | the **shot** `s`. `id &&& g` followed by writing field `l`. The exact
-- | dual of `caseToVariant` (`VariantToVariant`): the whole row sits at the
-- | wrapped profunctor's *input* end here, at its *output* end there — products
-- | grow the output row, sums grow the input row.
recordToProperty
  :: forall @l p b s f
   . IsSymbol l
  => Cons l f b s
  => Strong p
  => p { | b } f
  -> p { | b } { | s }
recordToProperty g =
  dimap (\b -> Tuple b b)
        -- `unsafeSet` (not `insert`, which would demand `Lacks l b`) realizes the
        -- layout `Cons l f b s` pins: s = (l :: f | b). Under a shadowed duplicate
        -- `l` the outer entry wins — the same first-label convention the Variant
        -- side's `inj`/`on` (hence `caseToVariant`) already follow without `Lacks`.
        (\(Tuple b f) -> unsafeSet (reflectSymbol (Proxy @l)) f b)
        (second g)

-- | Eliminate the field `l :: f` — cut the **focus** out of the **shot** `s`,
-- | keeping the **background** `b`. The focus value feeds a sink `p f Unit`
-- | whose output is discarded (via `snd`); the monomorphic `Unit` makes the
-- | discard explicit. The transpose of `recordToProperty`: `first` + `delete`.
eliminateProperty
  :: forall @l p f b s
   . IsSymbol l
  => Cons l f b s
  => Strong p
  => p f Unit
  -> p { | s } { | b }
eliminateProperty g =
  -- no `Lacks`: `unsafeDelete` realizes the layout `Cons l f b s` pins — see
  -- `recordToProperty`'s note.
  dimap (\s -> Tuple (get (Proxy @l) s) (unsafeDelete (reflectSymbol (Proxy @l)) s)) snd (first g)

-- | Edit an existing field in place — the standard `Strong` field lens, read
-- | photographically as **refocusing**: the **focus** `f → f'` changes, the
-- | **background** `b` stays, so the **shot** `s` becomes `s'` (the shared `b`
-- | witnesses "same rows except at `l`"). `f' := f` recovers the simple
-- | `p f f -> p { | s } { | s }` form. Contrast `resolveProperty`/`retainCase`,
-- | which hold the focus and transform the background. (The *diagonal*
-- | re-backgrounder — hold field `l`, transform everything else — needs no
-- | combinator of its own: it is `focusRecord` at the singleton complement
-- | `(l :: f)`.)
property
  :: forall @l p f f' b s s'
   . IsSymbol l
  => Cons l f b s
  => Cons l f' b s'
  => Strong p
  => p f f' -> p { | s } { | s' }
property = prop (Proxy @l)

-- UI: seed a single-field input with an initial value. A widget that needs
-- a record field to display (e.g. `textInput @"name"`) becomes one needing
-- no input data — the default is shown initially and user edits flow back
-- via `r`. The default is consumed on every render. In schema terms the
-- singleton row `s` is a shot with empty background.
-- Lifts `p { l :: f } r` into `p {} r`.
withRecordDefault :: forall l p f s r.
  RL.RowToList s (RL.Cons l f RL.Nil) =>
  IsSymbol l =>
  Cons l f () s =>
  Profunctor p =>
  p { | s } r -> f -> p {} r
withRecordDefault p default = lcmap (const (insert (Proxy :: Proxy l) default {})) p

-- UI: promote a read-only widget into a form contributor. A display-only
-- element like `textOutput` or `icon` that captures nothing gets lifted to
-- one that emits a fixed singleton record on every call — useful for static
-- fields like auto-IDs, hidden constants, or computed values the form layer
-- needs. The default is emitted on every render. In schema terms the
-- singleton row `s` is a shot with empty background.
-- Lifts `p r {}` into `p r { l :: f }`.
withRecordOutputDefault :: forall l p f s r.
  RL.RowToList s (RL.Cons l f RL.Nil) =>
  IsSymbol l =>
  Cons l f () s =>
  Profunctor p =>
  p r {} -> f -> p r { | s }
withRecordOutputDefault p default = rmap (const (insert (Proxy :: Proxy l) default {})) p
