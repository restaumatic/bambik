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
-- |     over bare `Profunctor`: the adopters `atField` (read a field, closed
-- |     row), `projection`/`forProperty` (retype/read one into the canonical
-- |     display row), `projected` (read the whole) and `asField` (the
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
-- | pempty = identity @{}                    -- on every Category carrier
-- | ```
-- |
-- | The unit is the **wire at the unit row**: it owns no field, so a
-- | lawful merge treats its side as known from the start and ignores
-- | whatever it emits — a contribution of zero fields is no contribution.
-- | That is what makes `identity @{}` the unit exactly (not up to an echo),
-- | and the same wire is `VariantToVariant`'s unit at `Variant ()`.
-- | Pointing (one emission at registration) is not the unit's job: it is
-- | `Seeding`'s `announce`, and `with`/`mvu` below close over that.
module Data.Profunctor.Row.RecordToRecord
  ( bind
  , recordToRecord
  , class RecordToRecord
  , discard
  , blank
  , with
  , mvu
  , settled
  , feedback
  , asField
  , atField
  , projection
  , forProperty
  , projected
  , required
  , field
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
import Data.Profunctor.Seeding (class Seeding, announce, seeded)
import Data.Profunctor.Strong (class Strong, first)
import Control.Semigroupoid ((>>>))
import Data.Function (const)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record (get, insert, modify, union) as Record
import Record.Unsafe.Union (unsafeUnion)
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
  -- | On every `Category` carrier it is the wire at the unit row,
  -- | `pempty = identity @{}`; it stays a class member so carriers without
  -- | a `Category` can still state their unit.
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

-- | The **faceless leaf**: reads nothing — stated as subsumption in its
-- | own signature, like the gated displays' — and contributes nothing. The
-- | unit's `lcmap`-closure,
-- |
-- | ```
-- | blank = lcmap (const {}) pempty    -- accept any record input
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
-- | (`with a w = announce a >>> w`, so `with a identity = announce a`; the
-- | point `announce` is `Seeding`'s primitive). The residual input row of a pipeline is exactly
-- | what is *not yet known* at t=0; `with` (and `PUI.mvu`, its looping
-- | sibling) turns that obligation into `{}`, the one self-pointed record.
-- | For a pass-through seeding *stage* (feed once, then keep forwarding
-- | inputs), use the `seeded` wire directly: `seeded a >>> w`.
-- | Only the *input* side is record-shaped (the announcement is a row of
-- | fields); the output rides through untouched, so `with` closes a
-- | record pipeline and seeds a `× → +` emitter's replay value alike —
-- | the leaf leads, the plumbing trails: `button { … } # with patch`.
with :: forall p a o. Seeding p => { | a } -> p { | a } o -> p {} o
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
mvu :: forall p model. Looping p => Seeding p => { | model } -> p { | model } { | model } -> p {} { | model }
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
projected :: forall l p a b o cr. RowToList cr (RL.Cons l b RL.Nil) => IsSymbol l => Cons l b () cr => Profunctor p => (a -> b) -> p { | cr } o -> p a o
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
required :: forall l p a b s si so. RowToList si (RL.Cons l (Maybe a) RL.Nil) => IsSymbol l => Cons l (Maybe a) () si => Cons l a () so => Cons l a b s => Strong p => p { | si } { | so } -> p { | s } { | s }
required w = field @l (dimap (\v -> Record.insert (Proxy @l) (Just v) {}) (Record.get (Proxy @l)) w)

-- | Feed a **structural** UI component the bare field `l` (closed singleton
-- | row) — the non-display sibling of `projection` (which formats a
-- | display's field in place): a packaged collection reads its array
-- | (`… # muted # atField @"entries"`, the packaged-collection-display
-- | protocol), nested chrome reads its sub-rows
-- | (`… # foreach @"name" identity # atField @"dishes"`).
atField :: forall @l p a o r. IsSymbol l => Profunctor p => Cons l a () r => p a o -> p { | r } o
atField = lcmap (Record.get (Proxy @l))

-- | Retype a display's field **through a formatter**, label untouched:
-- | `text @"balance" # projection euros` shows the amount formatted as
-- | field `balance`. The leaf states the business label once;
-- | `RowToList`'s fundep reads it back out of the closed singleton row, so
-- | no label is repeated and no canonical label exists. Verbatim reads need
-- | no `projection` at all (`text @"prompt"`).
-- |
-- | `lcmap`-only; a display owns no output fields. Whole-value reads are
-- | `projected f`; context-pinned wider rows are `forProperty`.
projection :: forall l p a b ia ib o. RowToList ia (RL.Cons l a RL.Nil) => IsSymbol l => Cons l a () ia => Cons l b () ib => Profunctor p => (b -> a) -> p { | ia } o -> p { | ib } o
projection f = lcmap (Record.modify (Proxy @l) f)

-- | `projection`'s **open-row** sibling (the display-side `field @l`: the
-- | background is carried), for positions whose row the context already
-- | pins — collection items, pane payloads. The label is the leaf's own,
-- | read back out of its row, and the field passes verbatim:
-- | `text @"label" # forProperty` on a collection element. A formatted
-- | read is `projection`'s job, composed before it —
-- | `text @"score" # projection show # forProperty` — which is why this
-- | takes no function of its own.
forProperty :: forall l p b t r cr o. RowToList cr (RL.Cons l b RL.Nil) => IsSymbol l => Cons l b () cr => Cons l b t r => Profunctor p => p { | cr } o -> p { | r } o
forProperty = lcmap (\r -> Record.insert (Proxy @l) (Record.get (Proxy @l) r) {})

-- | Adopt a **canonically-labeled** component (`{ value :: a }` in and out,
-- | the citizenship-carrying scalar interface) as business field `l`: a pure
-- | relabeling, `dimap`-only — merge-gate exactness untouched,
-- | annotation-free as a merge operand (closed singleton rows on both
-- | sides). Where `field @l` lifts a scalar under `l`, `asField` renames
-- | the canonical `value` to `l` — the packaged-control rename.
asField :: forall @c @l p a b s t ci co. IsSymbol c => IsSymbol l => Profunctor p => Cons c a () ci => Cons c b () co => Cons l a () s => Cons l b () t => p { | ci } { | co } -> p { | s } { | t }
asField = dimap (\r -> Record.insert (Proxy @c) (Record.get (Proxy @l) r) {}) (\r -> Record.insert (Proxy @l) (Record.get (Proxy @c) r) {})

-- | The **counit**: render, and **deliberately discard** the component's
-- | output — `rmap`-only, the explicit form of what no stage may ever do
-- | silently. The duoidal reading (see `PUI`'s header and
-- | doc/collections-profunctor-algebra.md §0): a fulfillment-gated display
-- | (`shown` and its rungs) carries the comultiplication (render *and*
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
-- | Idempotence is the other half of the contract: an editor stage echoes
-- | every fed row, so the normalizer runs on every loop turn, not only on
-- | the edit — it states an invariant of the value (meeting-booker's
-- | `seatsInRoom`, order-form's `staleDistanceForgotten`), never a reaction
-- | to the edit, which the next re-broadcast would undo.
-- |
-- | The normalizer **subsumes** (like `PUI.updated`'s handler): it may read
-- | and rebuild a sub-row of the emission, merged back over the full value,
-- | so a normalization states its exact footprint in its own signature
-- | (`Union small rest big`: the emission is the footprint plus the rest).
-- | With `small ≡ big` this is the plain whole-row form.
settled
  :: forall p small rest big i
   . Profunctor p
  => Union small rest big
  => ({ | small } -> { | small })
  -> p i { | big }
  -> p i { | big }
settled f = rmap (\big -> unsafeUnion (f (unsafeCoerce big)) big :: { | big })

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
