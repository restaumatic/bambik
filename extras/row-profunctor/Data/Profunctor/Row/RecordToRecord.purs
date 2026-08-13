-- | `Record → Record` row profunctors, organized as:
-- |
-- |   * **strength** — `Strong` (ecosystem class, imported): the unary power,
-- |     minimal and interop-friendly.
-- |
-- | The **canonical-row adopters** here (`forField`/`forProperty`/`projected`/`asField`/`required`) take the canonical label as their
-- | first type argument `@c` and carry no literal: which label a component
-- | speaks (`value`, `clicked`, `event`) is an L3 citizenship convention of
-- | the vocabulary, not a row-profunctor fact. The label is supplied at the
-- | **call site** — `# asCase @"clicked" @l` — so no layer hard-codes it and
-- | the convention is visible where it is used.
-- |   * **direction class** — `RecordToRecord`, the binary **merge**: the one
-- |     genuine per-carrier primitive.
-- |   * **free functions** — over the strength: `subStrong` (sub-record
-- |     focus), `focusProperty` (the field lens), `tapped` (the display tap);
-- |     over the **unit**: `announce` (its `rmap`-closure — the announcing
-- |     constant) and `with` (`announce a >>> w` over `Semigroupoid` —
-- |     discharge the initial-state obligation), plus the subsuming
-- |     `settled` (`rmap`-only normalization over a stated sub-row);
-- |     over bare `Profunctor`: the adopters `atField`/`atProperty` (read a
-- |     field, closed or open row), `forField`/`forProperty` (read one into
-- |     the canonical display row), `projected` (read the whole), `toField`
-- |     (build a field, the transpose of `toCase`) and the fused
-- |     `field`/`asField` (`field @l = atField @l <<< toField @l identity`;
-- |     `field` is `focusProperty`'s closed-singleton form — the
-- |     merge-operand shape, `dimap`-only and runtime-exact by construction;
-- |     the merges themselves enforce exactness via `MergeableRecords`, so
-- |     `focusProperty` operands are safe too);
-- |     over the co-strength `Costrong`: `feedback` (the ×-trace at row
-- |     granularity — a state sub-record loops from output to input, the
-- |     `Colens` optic's row form; the optic itself is in
-- |     `Data.Lens.Colens`).
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
  ( bind
  , recordToRecord
  , class RecordToRecord
  , discard
  , announce
  , blank
  , with
  , mvu
  , settled
  , informed
  , feedback
  , asField
  , atField
  , atProperty
  , forField
  , forProperty
  , projected
  , required
  , field
  , toField
  , pempty
  , subStrong
  , focusProperty
  , tapped
  , completed
  )
  where

import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor, dimap, lcmap, rmap)
import Data.Profunctor.Costrong (class Costrong, unfirst)
import Data.Profunctor.Looping (class Looping, looped)
import Data.Profunctor.Seeding (class Seeding, seeded)
import Data.Profunctor.Strong (class Strong, first, second)
import Control.Semigroupoid (class Semigroupoid, (>>>))
import Data.Function (const)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (Unit, unit)
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Prim.RowList (class RowToList)
import Record (get, insert, merge, union) as Record
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

-- | The **announcing constant**: silent except for one emission of `o` at
-- | registration — the `rmap`-closure of the unit,
-- |
-- | ```
-- | announce o = rmap (const o) pempty
-- | ```
-- |
-- | generalizing the unit's informationless `{}` announcement to a row of
-- | fields. As a merge operand it seeds fields; composed in front of a
-- | UI component it discharges the initial-state obligation (`with`'s
-- | implementation). Record-shaped like every `× → ×` built side (the
-- | `toField` convention) — a case is seeded by adopting an announcement
-- | (`announce { … } # toCase @l f`) or a `seeded` wire's `inj`, never by
-- | a variant-typed constant: the `× → +` unit is silent, so no
-- | case-announcer can close over it.
announce :: forall p r. RecordToRecord p => { | r } -> p {} { | r }
announce o = rmap (const o) pempty

-- | The **faceless announcing leaf**: reads nothing — stated as
-- | subsumption in its own signature, like `tapped`'s — and announces the
-- | informationless `{}` once at registration. The unit's `lcmap`-closure,
-- | `announce`'s exact twin on the other side:
-- |
-- | ```
-- | announce o = rmap (const o) pempty      -- build a constant output
-- | blank      = lcmap (const {}) pempty    -- accept any record input
-- | ```
-- |
-- | The leaf for elements whose whole face is decorators — a channel-fed
-- | SVG shape or styled `div` (`circle >>> attrWith "fill" f $ blank`):
-- | the decorators read the fed row, the leaf under them reads `()` of it,
-- | which is always exact. Positions whose mechanism already subsumes
-- | (merge operands, `clicked` content, `action`'s progress slot) need no
-- | `blank` — `pempty` fits them directly.
blank :: forall p i. RecordToRecord p => p { | i } {}
blank = lcmap (const {}) pempty

-- | **Discharge a UI component's initial-state obligation**: `with a w` supplies
-- | `w`'s input its t=0 value — the entity `w` edits exists from the very
-- | beginning, and `a` is its initial state — leaving nothing to feed
-- | (`with a w = announce a >>> w`, so `with a identity = announce a` on
-- | `Category` carriers). The residual input row of a pipeline is exactly
-- | what is *not yet known* at t=0; `with` (and `PUI.mvu`, its looping
-- | sibling) turns that obligation into `{}`, the one self-pointed record.
-- | For a pass-through seeding *stage* (feed once, then keep forwarding
-- | inputs), use the `seeded` wire directly: `seeded a >>> w`.
-- | Shaped `× → ×` like everything here: `with` closes a record pipeline.
-- | Seeding a `× → +` *emitter's* replay value is the announcement
-- | composed, spelled as such: `announce patch >>> button { … }`.
with :: forall p a o. RecordToRecord p => Semigroupoid p => { | a } -> p { | a } { | o } -> p {} { | o }
with a w = announce a >>> w

-- | The model–view–update shape, named: `mvu seed w = with seed (looped w)`.
-- | `w` is a same-type pipeline over the model — editors (`# completed`
-- | where they don't produce the whole model), displays, wires, and event
-- | stages folded in with Mealy folds. The model is an **entity**: it
-- | exists from the very beginning with a known initial state, and `seed`
-- | is that state — fed once at registration; from then on every emission
-- | of any stage re-enters at the top, re-entrancy-guarded (`Looping`).
-- | The result is **closed** (input `{}`): supplying the seed discharges
-- | the pipeline's initial-state obligation, which is what a mount entry
-- | demands. The standalone app reads `body $ ... $ mvu seed pipeline`.
mvu :: forall p model. Looping p => RecordToRecord p => Semigroupoid p => { | model } -> p { | model } { | model } -> p {} { | model }
mvu seed w = with seed (looped w)

-- | Row-typed `Strong`: focus a whole **sub-record** — the row-valued **focus**
-- | `f` — transforming it against the **background** `b`, which is carried
-- | unchanged. The **shot** `s` is refocused to `s'`. Operates on rows on
-- | **both sides** — the argument is itself a `Record → Record` profunctor:
-- |
-- | ```
-- | subStrong :: p { | f } { | f' } -> p { | s } { | s' }
-- |              -- where s = f ∪ b,  s' = f' ∪ b   (ExclusiveRows)
-- | ```
-- |
-- | The labeled analogue of `Strong`'s `first`/`second`: instead of carrying a positional
-- | complement `c`, it carries the background *row* `b`, split off by `ExclusiveRows`.
-- | Plain `Strong` underneath: split `s` into `(f, b)`, run the argument on `f`
-- | via `first`, and re-merge `f'` with `b`.
subStrong
  :: forall p f f' b s s'
   . Strong p
  => ExclusiveRows f b s
  => ExclusiveRows f' b s'
  => p { | f } { | f' }
  -> p { | s } { | s' }
subStrong g =
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
-- | is `subStrong` at the singleton complement `(l :: f)`.)
focusProperty
  :: forall @l p f f' b s s'
   . IsSymbol l
  => Cons l f b s
  => Cons l f' b s'
  => Strong p
  => p f f' -> p { | s } { | s' }
focusProperty = prop (Proxy @l)

-- | Feed a canonically-labeled component a **function of the whole input**:
-- | `projected f` turns a `{ value :: b }` component into one fed a bare `a`,
-- | with `f a` flowing in as its `value` — so formatted displays read
-- | `text # projected @"value" readout`, and the whole-value verbatim read is
-- | `projected @"value" identity`. `lcmap`-only.
projected :: forall @c p a b o cr. IsSymbol c => Lacks c () => Cons c b () cr => Profunctor p => (a -> b) -> p { | cr } o -> p a o
projected f = lcmap \a -> Record.insert (Proxy @c) (f a) {}

-- | Mark a type-changing selector (`{ value :: Maybe a } → { value :: a }`)
-- | as **always selected**: the `Maybe` input exists for the unselected
-- | display state, so when the model guarantees a selection it is vacuous —
-- | every model value shows as chosen. Dissolves the
-- | `dimap (\v -> { value: Just v }) _.value` bracket into a named stage:
-- | `select config options # required @"value" # asField @l`.
-- | Its dual — a selector left possibly-unselected, the model keeping the
-- | `Maybe` — is `PUI.optional` (carrier-level: it must complete the leaf's
-- | `Just`-only echo, which no `dimap` can).
required :: forall @c p a b si so. IsSymbol c => Lacks c () => Cons c (Maybe a) () si => Cons c a () so => Profunctor p => p { | si } b -> p { | so } b
required = lcmap (\r -> Record.insert (Proxy @c) (Just (Record.get (Proxy @c) r)) {})

-- | Feed a **structural** UI component the bare field `l` (closed singleton
-- | row) — the non-display sibling of `forField` (which formats into the
-- | canonical `{ value }` display): a packaged collection reads its array
-- | (`… # atField @"value" # displayed`, the packaged-collection-display
-- | protocol), nested chrome reads its sub-rows
-- | (`… # foreach @"name" identity # atField @"dishes"`).
atField :: forall @l p a o r. IsSymbol l => Profunctor p => Lacks l () => Cons l a () r => p a o -> p { | r } o
atField = lcmap (Record.get (Proxy @l))

-- | `atField`'s **open-row** sibling, exactly as `forProperty` is
-- | `forField`'s: feed a structural UI component the bare field `l` of a *wider*
-- | row, the background labels untouched. `lcmap`-only. The open row is
-- | legal here because a record input is **shared** — every operand may read
-- | every field — which is the same law that permits `forProperty` and
-- | forbids an open-row read at `[ | s ]`.
atProperty :: forall @l p a o t r. IsSymbol l => Profunctor p => Cons l a t r => p a o -> p { | r } o
atProperty = lcmap (Record.get (Proxy @l))

-- | Read field `l` (closed singleton row) into the canonical `{ value }`
-- | display, through the formatter — the display-side member of the
-- | mechanism-argument doctrine (L16): `text # forField @"value" @"points" show`,
-- | `identity` says verbatim (`text # forField @"value" @"prompt" identity`). The
-- | closed row is what makes a merge operand state its exact input;
-- | context-pinned wider rows use `forProperty @l f`. The whole-value
-- | reads are `projected f` (`projected identity` for verbatim).
-- |
-- | `lcmap`-only, the input-side member of the adopter family (`asField`
-- | renames both sides of an editor); a display owns no output fields.
forField :: forall @c @l p a b o r cr. IsSymbol c => IsSymbol l => Profunctor p => Lacks c () => Cons c b () cr => Lacks l () => Cons l a () r => (a -> b) -> p { | cr } o -> p { | r } o
forField f = lcmap (\r -> Record.insert (Proxy @c) (f (Record.get (Proxy @l) r)) {})

-- | `forField`'s **open-row** sibling (the display-side `focusProperty`: the
-- | background is carried), for positions whose row the context already
-- | pins — collection items, pane payloads:
-- | `… # forProperty @"value" @"label" identity` on a collection element,
-- | `… # forProperty @"value" @"score" show`.
forProperty :: forall @c @l p a b t r cr o. IsSymbol c => IsSymbol l => Profunctor p => Lacks c () => Cons c b () cr => Cons l a t r => (a -> b) -> p { | cr } o -> p { | r } o
forProperty f = lcmap (\r -> Record.insert (Proxy @c) (f (Record.get (Proxy @l) r)) {})

-- | Adopt a **canonically-labeled** component (`{ value :: a }` in and out,
-- | the citizenship-carrying scalar interface) as business field `l`: a pure
-- | relabeling, `dimap`-only like `field` — merge-gate exactness untouched,
-- | annotation-free as a merge operand (closed singleton rows on both sides).
-- | `field` wraps its argument under `l`; `asField` renames `value` to `l`.
asField :: forall @c @l p a b s t ci co. IsSymbol c => IsSymbol l => Profunctor p => Lacks c () => Cons c a () ci => Cons c b () co => Lacks l () => Cons l a () s => Cons l b () t => p { | ci } { | co } -> p { | s } { | t }
asField = dimap (\r -> Record.insert (Proxy @c) (Record.get (Proxy @l) r) {}) (\r -> Record.insert (Proxy @l) (Record.get (Proxy @c) r) {})

field :: forall @l p f f' si so. IsSymbol l => Profunctor p => Lacks l () => Cons l f () si => Cons l f' () so => p f f' -> p { | si } { | so }
field = dimap (Record.get (Proxy @l)) (\v -> Record.insert (Proxy @l) v {})

-- | Introduce a **bare** output as field `l` — `rmap`-only, the transpose of
-- | `RecordToVariant.toCase`, and the output half `field` fuses:
-- | `field @l = atField @l <<< toField @l identity`, mirroring how the
-- | deliberately-absent `+ → +` case wrap is `atCase @l # toCase @l' f`.
-- | The closed singleton row is what a **record output** admits: the side is
-- | *owned*, so a field may be built alone only when it is the whole row —
-- | an open-row build would have to produce the other fields from nothing,
-- | which is `completed`'s job over `Strong`.
-- | The canonical-row rename on this side needs no combinator of its own: it
-- | is `toField @l _.value`, exactly as `asCase` is `toCase` at the canonical
-- | eliminator.
toField :: forall @l p i a b s. IsSymbol l => Profunctor p => Lacks l () => Cons l b () s => (a -> b) -> p i a -> p i { | s }
toField f = rmap (\a -> Record.insert (Proxy @l) (f a) {})

-- | Settle a stage's emissions through a **total, type-preserving**
-- | normalization — the round-trip rule's mechanism made a word: a lossy
-- | adjustment belongs in the model, after `completed`, where the loop
-- | makes it a transaction — `formula # completed # settled commit`.
-- | Type-preservation is the contract: `settled` normalizes, it cannot
-- | re-shape. `rmap`-only.
-- |
-- | The normalizer **subsumes** (like `PUI.updated`'s handler): it may read
-- | and rebuild a sub-row of the emission, merged back over the full value,
-- | so a normalization states its exact footprint in its own signature.
-- | With `small ≡ big` this is the plain whole-row form.
settled
  :: forall p small u big i
   . Profunctor p
  => Union small big u
  => Nub u big
  => ({ | small } -> { | small })
  -> p i { | big }
  -> p i { | big }
settled f = rmap (\big -> Record.merge (f (unsafeCoerce big)) big)

-- | The **dispatch adapter**: make a single-record business function a
-- | Mealy handler (`PUI.updated`, or a `match` branch of one). The
-- | handler's two records — the event's payload and the retained model
-- | row — travel together into every fold, so `informed` merges them and
-- | the business function sees **one row of facts**, the payload's fields
-- | laid over the model's (fresh knowledge wins — the union is
-- | left-biased, like the merges), returning the match row:
-- |
-- | ```
-- | # updated (match { refunded: informed applyRefund })
-- | applyRefund :: { amount :: Number, balance :: Number } -> { balance :: Number }
-- | ```
-- |
-- | Reads are **per-branch exact**: `fed` is the function's own closed row,
-- | read from the merged facts by subsumption, so a branch states precisely
-- | which payload and model fields it consumes — unused payload fields cost
-- | nothing, and a payload label shadowing a model label reads as the
-- | payload's (first-label convention). What A12 once exempted as
-- | "mechanism-dictated currying" dissolves here; only scalar and `Array`
-- | payloads (a key, a fetched list) stay positional — they are not rows.
-- | Pure record algebra — no profunctor in sight; it lives here because
-- | its rows are this direction's.
informed
  :: forall pay small u fed extra
   . Union pay small u
  => Union fed extra u
  => ({ | fed } -> { | small })
  -> { | pay }
  -> { | small }
  -> { | small }
informed g pay small = g (unsafeCoerce (Record.union pay small))

-- | A display **tap** on the `×`-diagonal: shows the value flowing through
-- | and passes it on — the pipeline-stage form of a live view. Pure `Strong`
-- | plus the leaf-echo protocol: `second` retains the value, and the
-- | display's echo triggers the forwarding. Honest only over *displays*
-- | (elements whose sole emission is the echo) — an editing UI component inside
-- | would replay the retained upstream value on every edit.
-- |
-- | **Subsumption is built in**: the display may read a *narrower* row than
-- | the stage carries (`text # projected @"value" readout # tapped`, where `readout`
-- | declares only the fields it formats), so a closed-row read function needs
-- | no `widenRecordInput` at the tap.
tapped :: forall p narrow extra wider x. Strong p => Union narrow extra wider => p { | narrow } x -> p { | wider } { | wider }
tapped display = dimap (\s -> Tuple s s) fst (second (widenRecordInput display))

-- | **Complete** a UI component's output to its full input row: fields the
-- | UI component doesn't produce are carried from the retained input, so a merge
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
-- | stage. Like `subStrong`, the output is split by coercion, so the
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
      -- coerce-split, as in `subStrong`: safe because `ExclusiveRows o fb ow`
      -- guarantees the two typed views are disjoint
      (\ow -> Tuple (unsafeCoerce ow) (unsafeCoerce ow))
      (seeded seed >>> g))
