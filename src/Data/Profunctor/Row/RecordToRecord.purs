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
  , atField
  , forField
  , forProperty
  , projected
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
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Costrong (class Costrong, unfirst)
import Data.Profunctor.Seeding (class Seeding, seeded)
import Data.Profunctor.Strong (class Strong, first, second)
import Control.Semigroupoid ((>>>))
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

-- | Feed a canonically-labeled component a **function of the whole input**:
-- | `projected f` turns a `{ value :: b }` component into one fed a bare `a`,
-- | with `f a` flowing in as its `value` — so `forValue` is exactly
-- | `projected identity`, and formatted displays read `text # projected
-- | readout` with no trailing `# forValue`. Composes straight into `forField`
-- | (which now reads a field into a *bare*-value display): `text # projected
-- | show # forField @l` formats field `l`. `lcmap`-only.
-- | The whole-value verbatim read is `projected identity`.
projected :: forall p a b o. Profunctor p => (a -> b) -> p { value :: b } o -> p a o
projected f = lcmap \a -> { value: f a }

-- | Mark a type-changing selector (`{ value :: Maybe a } → { value :: a }`)
-- | as **always selected**: the `Maybe` input exists for the unselected
-- | display state, so when the model guarantees a selection it is vacuous —
-- | every model value shows as chosen. Dissolves the
-- | `dimap (\v -> { value: Just v }) _.value` bracket into a named stage:
-- | `select config options # required # asField @l`.
-- | Its dual — a selector left possibly-unselected, the model keeping the
-- | `Maybe` — is `PUI.optional` (carrier-level: it must complete the leaf's
-- | `Just`-only echo, which no `dimap` can).
required :: forall p a b. Profunctor p => p { value :: Maybe a } b -> p { value :: a } b
required = lcmap (\r -> { value: Just r.value })

-- | Read field `l` into a **bare-value** display — the display expecting a
-- | plain `a`, as produced by `forValue`/`projected`: `text # projected show
-- | # forField @l` formats field `l`, and `text # forValue # forField @l`
-- | shows it verbatim. `lcmap`-only, the input-side member of the adopter
-- | family (`asField` renames both sides of an editor). Closed singleton row:
-- | annotation-free as a merge operand, and a display owns no output fields.
-- | Read field `l` (closed singleton row) into the canonical `{ value }`
-- | display, through the formatter — the display-side member of the
-- | mechanism-argument doctrine (L16): `text # forField @"points" show`,
-- | `identity` says verbatim (`text # forField @"prompt" identity`). The
-- | closed row is what makes a merge operand state its exact input;
-- | context-pinned wider rows use `forProperty @l f`. The whole-value
-- | reads are `projected f` (`projected identity` for verbatim).
-- | Feed a **structural** widget the bare field `l` (closed singleton
-- | row) — the non-display sibling of `forField` (which formats into the
-- | canonical `{ value }` display): a packaged collection reads its array
-- | (`… # atField @"value" # displayed`, the packaged-collection-display
-- | protocol), nested chrome reads its sub-rows
-- | (`… # foreach @"name" identity # atField @"dishes"`).
atField :: forall @l p a o r. IsSymbol l => Profunctor p => Lacks l () => Cons l a () r => p a o -> p { | r } o
atField = lcmap (Record.get (Proxy @l))

forField :: forall @l p a b o r. IsSymbol l => Profunctor p => Lacks l () => Cons l a () r => (a -> b) -> p { value :: b } o -> p { | r } o
forField f = lcmap (\r -> { value: f (Record.get (Proxy @l) r) })

-- | `forField`'s **open-row** sibling (the display-side `property`: the
-- | background is carried), for positions whose row the context already
-- | pins — collection items, pane payloads:
-- | `listOf {} entries (text # forProperty @"label" identity)`,
-- | `text # forProperty @"score" show`.
forProperty :: forall @l p a b t r o. IsSymbol l => Profunctor p => Cons l a t r => (a -> b) -> p { value :: b } o -> p { | r } o
forProperty f = lcmap (\r -> { value: f (Record.get (Proxy @l) r) })

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
-- |
-- | **Subsumption is built in**: the display may read a *narrower* row than
-- | the stage carries (`text # projected readout # tapped`, where `readout`
-- | declares only the fields it formats), so a closed-row read function needs
-- | no `widenRecordInput` at the tap.
tapped :: forall p narrow extra wider x. Strong p => Union narrow extra wider => p { | narrow } x -> p { | wider } { | wider }
tapped display = dimap (\s -> Tuple s s) fst (second (widenRecordInput display))

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
-- | The traced chain is an **entity** — it has state over time, so it has
-- | a known initial state — and `feedback` takes that t=0 value as its
-- | first argument: the whole inner input `{ | iw }` (the loop re-enters
-- | `×`-joined with the input, so the chain's starting point is the join).
-- | The seed is fed once at registration (a `seeded` wire composed into
-- | the chain), the chain renders and emits, and the state channel is
-- | primed before any input arrives — a `feedback` stage never starves.
-- | Emission-primed exotica remain expressible with raw `unfirst`/`colens`.
feedback
  :: forall p i o fb iw ow
   . Seeding p
  => Costrong p
  => ExclusiveRows i fb iw
  => ExclusiveRows o fb ow
  => { | iw }
  -> p { | iw } { | ow }
  -> p { | i } { | o }
feedback seed g =
  unfirst
    (dimap
      (\(Tuple i fb) -> Record.union i fb)
      -- coerce-split, as in `focusRecord`: safe because `ExclusiveRows o fb ow`
      -- guarantees the two typed views are disjoint
      (\ow -> Tuple (unsafeCoerce ow) (unsafeCoerce ow))
      (seeded seed >>> g))

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
