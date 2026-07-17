-- | `Record → Record` row profunctors, organized as:
-- |
-- |   * **strength** — `Strong` (ecosystem class, imported): the unary power,
-- |     minimal and interop-friendly.
-- |   * **direction class** — `RecordToRecord`, the binary **merge**: the one
-- |     genuine per-carrier primitive.
-- |   * **free functions** — over the strength: `focusRecord` (sub-record
-- |     focus), `property` (the field lens), `tapped` (the display tap);
-- |     over bare `Profunctor`: `field` (`property`'s closed-singleton
-- |     form — the merge-operand shape, `dimap`-only and runtime-exact
-- |     by construction; the merges themselves enforce exactness via
-- |     `MergeableRecords`, so `property` operands are safe too);
-- |     over the co-strength `Costrong`: the `Colens` optic with
-- |     `colens`/`colensE` (the reversed lens) and its row form `feedback`
-- |     (the ×-trace at row granularity — a state sub-record loops from
-- |     output to input).
-- |
-- | The **nullary** operator is the class's own unit `pempty :: p {} {}` —
-- | the empty merge:
-- |
-- | ```
-- | recordToRecord pempty g = g = recordToRecord g pempty
-- | ```
-- |
-- | It is a class member (not a parametric silent element like `PUI`'s
-- | `silence`) because a lawful record-output unit must *announce* its
-- | contribution — the informationless `{}` — to the merge machinery, and
-- | anything typed `forall a b. p a b` is silent by parametricity (it can
-- | never fabricate a `b`). For `Category` carriers, `pempty = identity @{}`.
module Data.Profunctor.Row.RecordToRecord
  ( Colens
  , bind
  , colens
  , colensE
  , recordToRecord
  , class RecordToRecord
  , discard
  , feedback
  , asField
  , forField
  , forValue
  , projection
  , required
  , field
  , pempty
  , focusRecord
  , property
  , tapped
  , completed
  )
  where

import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap, lcmap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Costrong (class Costrong, unfirst)
import Data.Profunctor.Strong (class Strong, first, second)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (Unit, unit)
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Prim.RowList (class RowToList)
import Record (get, insert, union) as Record
import Type.Proxy (Proxy(..))
import Data.Profunctor.Row (class ExclusiveRows, class FieldNames, class OwnedRecordOutputs, class SharedRecordInputs, exactRow, widenRecordInput)
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= RecordToRecord p where
  -- | One constraint per side: `SharedRecordInputs` (rows may overlap,
  -- | label-blind broadcast) and `OwnedRecordOutputs` (disjoint rows —
  -- | one producer per field — plus `MergeableRecords`, the merge's
  -- | **runtime-exactness guarantee**: gated carriers trim each operand's
  -- | emission to its declared output row before the left-biased union, so
  -- | an operand whose runtime object carries stale copies of sibling
  -- | fields — an echo wire or lens rebuild over the widening-coerced
  -- | input — cannot shadow the siblings' genuine contributions).
  recordToRecord :: forall i1 o1 i2 o2 i12 i1x i2x i o o1l o2l.
    SharedRecordInputs i1 i2 i i12 i1x i2x =>
    OwnedRecordOutputs o1 o2 o o1l o2l =>
    p { | i1 } { | o1 } -> p { | i2 } { | o2 } -> p { | i } { | o }
  -- | The **nullary** merge — the unit: reads nothing, contributes no fields.
  -- | Genuinely per-carrier: a parametric silent element cannot serve, because
  -- | a record-output unit must *announce* its informationless `{}` so the
  -- | merge machinery knows that side is complete. For `Category` carriers,
  -- | `pempty = identity @{}`.
  pempty :: p {} {}

bind :: forall p i1 o1 i2 o2 i12 i1x i2x i o o1l o2l.
  RecordToRecord p =>
  SharedRecordInputs i1 i2 i i12 i1x i2x =>
  OwnedRecordOutputs o1 o2 o o1l o2l =>
  p { | i1 } { | o1 } -> (p { | i1 } { | o1 } -> p { | i2 } { | o2 }) -> p { | i } { | o }
bind first cont = recordToRecord first (cont first)

discard :: forall p i1 o1 i2 o2 i12 i1x i2x i o o1l o2l.
  RecordToRecord p =>
  SharedRecordInputs i1 i2 i i12 i1x i2x =>
  OwnedRecordOutputs o1 o2 o o1l o2l =>
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

-- | Edit an existing field in place — the standard `Strong` field lens, read
-- | photographically as **refocusing**: the **focus** `f → f'` changes, the
-- | **background** `b` stays, so the **shot** `s` becomes `s'` (the shared `b`
-- | witnesses "same rows except at `l`"). `f' := f` recovers the simple
-- | `p f f -> p { | s } { | s }` form. (The *diagonal* re-backgrounder — hold
-- | field `l`, transform everything else — needs no combinator of its own: it
-- | is `focusRecord` at the singleton complement `(l :: f)`.)
property
  :: forall @l p f f' b s s'
   . IsSymbol l
  => Cons l f b s
  => Cons l f' b s'
  => Strong p
  => p f f' -> p { | s } { | s' }
property = prop (Proxy @l)

-- | `property` at the **closed singleton row** — the merge-operand form:
-- | nests a widget (or a whole sub-composite) as exactly one field of the
-- | enclosing record, type-changing like `property` (`f' := f` recovers the
-- | simple `p v v -> p { | r } { | r }` form). The pinned empty background
-- | is what lets merge operands infer with no annotations — raw
-- | `property`'s open background is ambiguous under the merges' `Union`.
-- |
-- | With no background to carry it needs no strength — `dimap` suffices —
-- | and its emissions are **runtime-exact** by construction: exactly the one
-- | field, freshly built. (A lens emission (`property`) instead rebuilds the
-- | record from its retained input, which under the merges' widening
-- | coercions runtime-carries stale copies of *sibling* fields. The gated
-- | merges guard against this — their `MergeableRecords` evidence trims every
-- | operand emission to its declared output row before the left-biased
-- | `Record.union` — so this is no longer a correctness obligation on
-- | operands; `field` remains the preferred operand form for its
-- | annotation-free inference.)
-- | Adopt a canonically-labeled component for the **whole input**: what
-- | flows in becomes its `value` — `forField`'s zero-focus sibling
-- | (`forField @l` reads one field; `forValue` reads everything), for
-- | displays of a function of the whole model: `text # projection f # forValue`.
forValue :: forall p a b. Profunctor p => p { value :: a } b -> p a b
forValue = lcmap { value: _ }

-- | Map the **canonical value** on the input side: adapt a component
-- | expecting `{ value :: a }` to accept `{ value :: a' }` through a
-- | projection `a' -> a` — the explicit formatting stage for displays
-- | (`text # projection show # forField @l`), `lcmap`-only.
projection :: forall p a a' b. Profunctor p => (a' -> a) -> p { value :: a } b -> p { value :: a' } b
projection f = lcmap (\r -> { value: f r.value })

-- | Mark a type-changing selector (`{ value :: Maybe a } → { value :: a }`)
-- | as **always selected**: the `Maybe` input exists for the unselected
-- | display state, so when the model guarantees a selection it is vacuous —
-- | every model value shows as chosen. Dissolves the
-- | `dimap (\v -> { value: Just v }) _.value` bracket into a named stage:
-- | `select config options # required # asField @l`.
required :: forall p a b. Profunctor p => p { value :: Maybe a } b -> p { value :: a } b
required = lcmap (\r -> { value: Just r.value })

-- | Adopt a **canonically-labeled display** (`{ value :: a }` in) for field
-- | `l`: renames the incoming field, output untouched — `lcmap`-only, the
-- | input-side member of the adopter family (`asField` renames both sides
-- | of an editor, `forField` reads one field into a display). Closed
-- | singleton row: annotation-free as a merge operand, and a display owns
-- | no output fields.
forField :: forall @l p a o r. IsSymbol l => Profunctor p => Lacks l () => Cons l a () r => p { value :: a } o -> p { | r } o
forField = lcmap (\r -> { value: Record.get (Proxy @l) r })

-- | Adopt a **canonically-labeled** component (`{ value :: a }` in and out,
-- | the citizenship-carrying scalar interface) as business field `l`: a pure
-- | relabeling, `dimap`-only like `field` — merge-gate exactness untouched,
-- | annotation-free as a merge operand (closed singleton rows on both sides).
-- | `field` wraps its argument under `l`; `asField` renames `value` to `l`.
asField :: forall @l p a b s t. IsSymbol l => Profunctor p => Lacks l () => Cons l a () s => Cons l b () t => p { value :: a } { value :: b } -> p { | s } { | t }
asField = dimap (\r -> { value: Record.get (Proxy @l) r }) (\r -> Record.insert (Proxy @l) r.value {})

field :: forall @l p f f' si so. IsSymbol l => Profunctor p => Lacks l () => Cons l f () si => Cons l f' () so => p f f' -> p { | si } { | so }
field = dimap (Record.get (Proxy @l)) (\v -> Record.insert (Proxy @l) v {})

-- | A display **tap** on the `×`-diagonal: shows the value flowing through
-- | and passes it on — the pipeline-stage form of a live view. Pure `Strong`
-- | plus the leaf-echo protocol: `second` retains the value, and the
-- | display's echo triggers the forwarding. Honest only over *displays*
-- | (elements whose sole emission is the echo) — an editing widget inside
-- | would replay the retained upstream value on every edit.
tapped :: forall p s x. Strong p => p s x -> p s s
tapped display = dimap (\s -> Tuple s s) fst (second display)

-- | **Complete** a widget's output to its full input row: fields the
-- | widget doesn't produce are carried from the retained input, so a merge
-- | of editors covering only part of the model needs no `field @l identity`
-- | echo wires to close the loop. The emission is trimmed to its declared
-- | row first (the `FieldNames` evidence), so the left-biased union is
-- | runtime-exact — the same guarantee the merge gates give.
completed
  :: forall p n nx i o u ol
   . Strong p
  => Union n nx i
  => Union o i u
  => Nub u i
  => RowToList o ol
  => FieldNames ol o o
  => p { | n } { | o }
  -> p { | i } { | i }
completed w = dimap (\i -> Tuple i i) (\(Tuple o i) -> overlay (exactRow o) i) (first (widenRecordInput w))
  where
  -- runtime-exact: keys of o ⊆ keys of i, and `exactRow` trimmed o,
  -- so the union's runtime key set is exactly i's (justifying the Nub)
  overlay :: { | o } -> { | i } -> { | i }
  overlay o i = unsafeCoerce (Record.union o i)

-- | The `×`-diagonal **trace at row granularity**, over ecosystem
-- | `Costrong`: the **state** sub-record `fb` of the output loops back into
-- | the input, so the wrapped profunctor sees `i ∪ fb` and its `fb`
-- | contribution comes around again — state threading across a pipeline
-- | stage. Like `focusRecord`, the output is split by coercion, so the
-- | emitted `{ | o }` runtime-carries the looped fields — a `feedback`
-- | stage belongs in a pipeline, not as a record-merge operand.
-- |
-- | On a knowledge-gated carrier (`PUI`) the state channel must be
-- | **primed by the widget's first emission** — inputs are withheld until
-- | then, so the inner widget must be able to emit unfed (editors that emit
-- | on user input qualify; load-first ensembles use `looped`, the
-- | self-feeding special case, instead).
feedback
  :: forall p i o fb iw ow
   . Costrong p
  => ExclusiveRows i fb iw
  => ExclusiveRows o fb ow
  => p { | iw } { | ow }
  -> p { | i } { | o }
feedback g =
  unfirst
    (dimap
      (\(Tuple i fb) -> Record.union i fb)
      -- coerce-split, as in `focusRecord`: safe because `ExclusiveRows o fb ow`
      -- guarantees the two typed views are disjoint
      (\ow -> Tuple (unsafeCoerce ow) (unsafeCoerce ow))
      g)

-- | The optic `unfirst` induces: the **Colens** — the lens run backwards
-- | (`Colens s t a b ≅ Lens b a t s`). Eliminating the residual `c`
-- | (instantiated to `b`) by co-Yoneda collapses `∃c. (s × c → a) × (b → t × c)`
-- | to `(join : s → b → a) × (out : b → t)`: each input is read **against the
-- | widget's own last output** — the residual a lens would carry visibly in
-- | the type is hidden, threaded through state instead. The collapsed form
-- | shows why the `PUI` carrier gates it (there is no last output before the
-- | first emission). `feedback` is this optic at row granularity.
type Colens s t a b = forall p. Costrong p => p a b -> p s t

colens :: forall s t a b. (s -> b -> a) -> (b -> t) -> Colens s t a b
colens join out = colensE (\(Tuple s b) -> join s b) (\b -> Tuple (out b) b)

-- | Construct a `Colens` straight from its **existential encoding**
-- | `∃c. (s × c → a) × (b → t × c)`: pick the looped channel `c`, then supply
-- | `decon` (read the input joined with the channel) and `recon` (split each
-- | emission into the output and the channel's next value). `colens` is this
-- | at the co-Yoneda witness `c := b`.
colensE :: forall s t a b c. (Tuple s c -> a) -> (b -> Tuple t c) -> Colens s t a b
colensE decon recon g = unfirst (dimap decon recon g)
