-- | `Record → Record` row profunctors, organized as:
-- |
-- |   * **strength** — `Strong` (ecosystem class, imported): the unary power,
-- |     minimal and interop-friendly.
-- |
-- | The adopters here (`projection`/`projected`/`forProperty`/`required`)
-- | carry **no canonical label at all**: the leaf states its business label
-- | once, as its own type argument, and each adopter reads it back out of
-- | the closed singleton row via `RowToList`'s fundep — so no layer
-- | hard-codes a label and none is ever repeated.
-- |   * **direction class** — `RecordToRecord`, the binary **merge**: the one
-- |     genuine per-carrier primitive.
-- |   * **free functions** — over the strength: `subStrong` (sub-record
-- |     focus), `field` (the field lens — the leaf lift, making every
-- |     label-indexed editor a whole-row citizen), `required` (a
-- |     type-changing selector adopted as an always-selected whole-row
-- |     citizen);
-- |     over the **unit**: `announce` (its `rmap`-closure — the announcing
-- |     constant) and `with` (`announce a >>> w` over `Semigroupoid` —
-- |     discharge the initial-state obligation), plus the subsuming
-- |     `settled` (`rmap`-only normalization over a stated sub-row);
-- |     over bare `Profunctor`: the adopters `atField`/`atProperty` (read a
-- |     field, closed or open row), `projection`/`forProperty` (retype/read one into
-- |     the canonical display row), `projected` (read the whole), `toField`
-- |     (build a field, the transpose of `toCase`) and `asField` (the
-- |     canonical-row rename for packaged controls);
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
  , projection
  , forProperty
  , projected
  , required
  , field
  , toField
  , muted
  , pempty
  , subStrong
  )
  where

import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor, dimap, lcmap, rmap)
import Data.Profunctor.Costrong (class Costrong, unfirst)
import Data.Profunctor.Looping (class Looping, looped)
import Data.Profunctor.Seeding (class Seeding, seeded)
import Data.Profunctor.Strong (class Strong, first)
import Control.Semigroupoid (class Semigroupoid, (>>>))
import Data.Function (const)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record (get, insert, merge, union) as Record
import Type.Proxy (Proxy(..))
import Data.Profunctor.Row (class ExclusiveRows, class OwnedRecordOutputs, class SharedRecordInputs)
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
-- | subsumption in its own signature, like the gated displays' — and announces the
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
-- | Only the *input* side is record-shaped (the announcement is a row of
-- | fields); the output rides through untouched, so `with` closes a
-- | record pipeline and seeds a `× → +` emitter's replay value alike —
-- | the leaf leads, the plumbing trails: `button { … } # with patch`.
with :: forall p a o. RecordToRecord p => Semigroupoid p => { | a } -> p { | a } o -> p {} o
with a w = announce a >>> w

-- | The model–view–update shape, named: `mvu seed w = with seed (looped w)`.
-- | `w` is a same-type pipeline over the model — editors (whole-row
-- | citizens, their unedited fields carried by `field @l`'s retained
-- | background), displays, wires, and event
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
-- |
-- | This is the **leaf lift**: every label-indexed editor is its scalar
-- | control under `field @l`, which is what makes the editor a **whole-row
-- | citizen** `p { l | rest } { l | rest }` — fed the wide row it edits
-- | field `l`, and each emission re-attaches the background retained by the
-- | `Strong` state channel, so the stage is runtime-complete by
-- | construction and no output completion is ever needed. The retained
-- | background is as fresh as the last feed: whole-row editors live inside
-- | a loop (`mvu`/`looped`/`bracketed`), whose re-broadcast keeps every
-- | sibling's background current within the turn. It also nests
-- | sub-composites: a closed group under a record-valued field is
-- | `group # field @l`, the background carried like any leaf's.
field
  :: forall @l p f f' b s s'
   . IsSymbol l
  => Cons l f b s
  => Cons l f' b s'
  => Strong p
  => p f f' -> p { | s } { | s' }
field = prop (Proxy @l)

-- | Feed a canonically-labeled component a **function of the whole input**:
-- | `projected f` turns a single-field component into one fed a bare `a`,
-- | with `f a` flowing in as its field — the label derived from the leaf's
-- | own row, so whole-value reads name what they show:
-- | `text @"summary" # projected summaryText` (`projected identity` for
-- | verbatim). `lcmap`-only.
projected :: forall l p a b o cr. RowToList cr (RL.Cons l b RL.Nil) => IsSymbol l => Lacks l () => Cons l b () cr => Profunctor p => (a -> b) -> p { | cr } o -> p a o
projected f = lcmap \a -> Record.insert (Proxy @l) (f a) {}

-- | Mark a type-changing selector (`{ l :: Maybe a } → { l :: a }`) as
-- | **always selected**: the `Maybe` input exists for the unselected
-- | display state, so when the model guarantees a selection it is vacuous —
-- | every model value shows as chosen. The result is a **whole-row
-- | citizen** `p { l :: a | rest } { l :: a | rest }`, the same shape
-- | `field @l` gives an editor: the selector's field is wrapped in `Just`
-- | on the way in and re-attached over the retained background on the way
-- | out. The label is not repeated: the selector's closed singleton rows
-- | state it once, and `RowToList`'s row-to-list functional dependency
-- | reads it back out.
-- | Its dual — a selector left possibly-unselected, the model keeping the
-- | `Maybe` — is `PUI.optional` (carrier-level: it must complete the leaf's
-- | `Just`-only echo, which no `dimap` can).
required :: forall l p a b s si so. RowToList si (RL.Cons l (Maybe a) RL.Nil) => IsSymbol l => Lacks l () => Cons l (Maybe a) () si => Cons l a () so => Cons l a b s => Strong p => p { | si } { | so } -> p { | s } { | s }
required w = field @l (dimap (\v -> Record.insert (Proxy @l) (Just v) {}) (Record.get (Proxy @l)) w)

-- | Feed a **structural** UI component the bare field `l` (closed singleton
-- | row) — the non-display sibling of `projection` (which formats a
-- | display's field in place): a packaged collection reads its array
-- | (`… # muted # atField @"entries"`, the packaged-collection-display
-- | protocol), nested chrome reads its sub-rows
-- | (`… # foreach @"name" identity # atField @"dishes"`).
atField :: forall @l p a o r. IsSymbol l => Profunctor p => Lacks l () => Cons l a () r => p a o -> p { | r } o
atField = lcmap (Record.get (Proxy @l))

-- | `atField`'s **open-row** sibling, exactly as `forProperty` is
-- | `forProperty`'s: feed a structural UI component the bare field `l` of a *wider*
-- | row, the background labels untouched. `lcmap`-only. The open row is
-- | legal here because a record input is **shared** — every operand may read
-- | every field — which is the same law that permits `forProperty` and
-- | forbids an open-row read at `[ | s ]`.
atProperty :: forall @l p a o t r. IsSymbol l => Profunctor p => Cons l a t r => p a o -> p { | r } o
atProperty = lcmap (Record.get (Proxy @l))

-- | Retype a display's field **through a formatter**, label untouched:
-- | `text @"bid" # projection (show <<< _.current)` shows the quantity's
-- | current value as field `bid`. The leaf states the business label once;
-- | `RowToList`'s fundep reads it back out of the closed singleton row, so
-- | no label is repeated and no canonical label exists. Verbatim reads need
-- | no `projection` at all (`text @"prompt"`).
-- |
-- | `lcmap`-only; a display owns no output fields. Whole-value reads are
-- | `projected f`; context-pinned wider rows are `forProperty f`.
projection :: forall l p a b ia ib o. RowToList ia (RL.Cons l a RL.Nil) => IsSymbol l => Lacks l () => Cons l a () ia => Cons l b () ib => Profunctor p => (b -> a) -> p { | ia } o -> p { | ib } o
projection f = lcmap (\r -> Record.insert (Proxy @l) (f (Record.get (Proxy @l) r)) {})

-- | `projection`'s **open-row** sibling (the display-side `field @l`: the
-- | background is carried), for positions whose row the context already
-- | pins — collection items, pane payloads. The label is the leaf's own,
-- | read back out of its row: `text @"label" # forProperty identity` on a
-- | collection element, `text @"score" # forProperty show`.
forProperty :: forall l p a b t r cr o. RowToList cr (RL.Cons l b RL.Nil) => IsSymbol l => Lacks l () => Cons l b () cr => Cons l a t r => Profunctor p => (a -> b) -> p { | cr } o -> p { | r } o
forProperty f = lcmap (\r -> Record.insert (Proxy @l) (f (Record.get (Proxy @l) r)) {})

-- | Adopt a **canonically-labeled** component (`{ value :: a }` in and out,
-- | the citizenship-carrying scalar interface) as business field `l`: a pure
-- | relabeling, `dimap`-only — merge-gate exactness untouched,
-- | annotation-free as a merge operand (closed singleton rows on both
-- | sides). Where `field @l` lifts a scalar under `l`, `asField` renames
-- | the canonical `value` to `l` — the packaged-control rename.
asField :: forall @c @l p a b s t ci co. IsSymbol c => IsSymbol l => Profunctor p => Lacks c () => Cons c a () ci => Cons c b () co => Lacks l () => Cons l a () s => Cons l b () t => p { | ci } { | co } -> p { | s } { | t }
asField = dimap (\r -> Record.insert (Proxy @c) (Record.get (Proxy @l) r) {}) (\r -> Record.insert (Proxy @l) (Record.get (Proxy @c) r) {})

-- | Introduce a **bare** output as field `l` — `rmap`-only, the transpose of
-- | `RecordToVariant.toCase`, mirroring how the deliberately-absent
-- | `+ → +` case wrap is `atCase @l # toCase @l' f`.
-- | The closed singleton row is what a **record output** admits: the side is
-- | *owned*, so a field may be built alone only when it is the whole row —
-- | an open-row build would have to produce the other fields from nothing,
-- | which only `field @l`'s retained background can supply over `Strong`.
-- | The canonical-row rename on this side needs no combinator of its own: it
-- | is `toField @l _.value`, exactly as `asCase` is `toCase` at the canonical
-- | eliminator.
toField :: forall @l p i a b s. IsSymbol l => Profunctor p => Lacks l () => Cons l b () s => (a -> b) -> p i a -> p i { | s }
toField f = rmap (\a -> Record.insert (Proxy @l) (f a) {})

-- | The **counit**: render, and **deliberately discard** the component's
-- | output — `rmap`-only, the explicit form of what no stage may ever do
-- | silently. The duoidal reading (see `PUI`'s header and
-- | doc/collections-profunctor-algebra.md §0): a fulfillment-gated display
-- | (`shownAs` and its rungs) carries the comultiplication (render *and*
-- | release), `muted` only the counit (render and drop). Wherever a
-- | genuinely emitting assembly (a `foreach` forwarding its elements, a
-- | packaged collection display echoing its array) is used purely as a
-- | display, the discard is written (`# muted` inside the gated stage).
-- | Loss of information is legal only in writing.
muted :: forall p i o. Profunctor p => p i o -> p i {}
muted = rmap (const {})

-- | Settle a stage's emissions through a **total, type-preserving**
-- | normalization — the round-trip rule's mechanism made a word: a lossy
-- | adjustment belongs in the model, on the whole-row stage, where the loop
-- | makes it a transaction — `formula # settled commit`.
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
-- |
-- | Its job is **genuine dispatch**: a payload that is *computed* (a
-- | bounded quantity assembled for a pane) or a fold that does real work
-- | (an undo transaction, a map over a collection). It is *not* for the
-- | identity fold — a field that exists only in one mode is a whole-row
-- | editor with gated existence (`PUI.Web.HTML.inCase`), whose `field @l`
-- | lift already re-attaches the rest of the row; `# provided paneOf
-- | # updated (informed setField)` with `setField` the identity was
-- | completion rebuilt by hand, and that shape is gone.
informed
  :: forall pay small u fed extra
   . Union pay small u
  => Union fed extra u
  => ({ | fed } -> { | small })
  -> { | pay }
  -> { | small }
  -> { | small }
informed g pay small = g (unsafeCoerce (Record.union pay small))


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
