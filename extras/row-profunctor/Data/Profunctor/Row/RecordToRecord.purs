-- | `Record → Record` row profunctors, organized as:
-- |
-- |   * **strength** — `Strong` (ecosystem class, imported): the unary power,
-- |     minimal and interop-friendly.
-- |
-- | The adopters here (`forProperty`/`required`)
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
-- |     row), `forProperty` (read one field of a context-pinned wider row
-- |     into a label-indexed leaf: selection, never formatting — copy is a
-- |     read function at the leaf, doc/research-copy-is-a-function.md) and
-- |     `asField` (the canonical-row rename for packaged controls);
-- |     over the co-strength `Costrong`: `feedback` (the ×-trace at row
-- |     granularity — a state sub-record loops from output to input, the
-- |     `Colens` optic's row form; the optic itself is in
-- |     `Data.Lens.Colens`).
-- |
-- | ## Laws of the `×→×` shape
-- |
-- | These are the three laws of Data.Profunctor.Row ("The laws, stated
-- | once") read at `×→×`, on the **nine axes** every shape's law set
-- | shares — each line's title is the axis, its subtitle this shape's
-- | reading (the floor's grid has all four; the two laws that read the
-- | same at every shape, symmetry-and-associativity and monotonicity, are
-- | stated once on the floor) — kept spelled out here because each line
-- | is what a test or a starvation message names.
-- |
-- | For a citizen `w :: p { | i } { | o }` — an **editor** of knowledge, or
-- | at `o = {}` a **display** — with `feed x` a feed, `emit y` an
-- | emission, `≈` observational equivalence and `⊑` refinement
-- | (doc/observational-semantics.md, whose §3.2 derives the gate from
-- | these; §3.1 is this shape's modality, **must** answer). The first
-- | three are **protocol obligations** the type cannot enforce; the rest
-- | are what a carrier **guarantees** given them.
-- |
-- | **Citizen laws** — repetition, emission, answer:
-- |
-- |   1. **Repetition — twice is once.** `feed x ; feed x ≈ feed x`: a
-- |      record is knowledge, and knowledge is idempotent.
-- |   2. **Emission — a whole retained row.** Every `emit y` is a whole
-- |      `{ | o }` from retained knowledge, withheld until that knowledge
-- |      exists: a one-sided change is completed from what is retained,
-- |      never fabricated and never sent partial. `field @l` is the leaf
-- |      instance — the background re-attached.
-- |   3. **Answer — exactly once.** If `o` is non-empty, every `feed x` is
-- |      answered by exactly one `emit y`. Nothing at registration —
-- |      except that a citizen with input `{}` counts registration as its
-- |      feed, since `{}` is always known: that is `announce`. If `o` is
-- |      empty, nothing is owed (a display may answer `{}` or stay silent).
-- |
-- | **Merge laws** — for `m = recordToRecord w1 w2`, inputs shared
-- | (`SharedRecordInputs`), outputs owned (`OwnedRecordOutputs`):
-- |
-- |   4. **Unit — the wire at `{}`.** Conditional on the carrier — *if* `p`
-- |      is a `Category`, `identity :: p {} {}` is the unit **exactly**, not
-- |      up to an echo:
-- |
-- |      ```
-- |      recordToRecord identity g = g = recordToRecord g identity
-- |      ```
-- |
-- |      The merge has no unit of its own; the wire at `{}` owns no field,
-- |      so a lawful merge treats its side as known from the start and
-- |      ignores whatever it echoes (the floor's monotonicity law: an operand
-- |      owning no field refines nothing). The same wire is
-- |      `VariantToVariant`'s unit at `Variant ()`. Pointing is not a
-- |      unit's job: it is `Seeding`'s `announce`, which `with`/`mvu`
-- |      below close over. The unary form — the merge pinned at its unit —
-- |      is the operand itself, `recordToRecord g identity = g`, so this
-- |      shape names no introducer; where the unit is `silence` (`×→+`,
-- |      `+→×`) the pinned merge is a real word, `recordToCase`/`reduce`.
-- |   5. **Input side — broadcast.** Every feed of `m` reaches both
-- |      operands, whole and in one step. Neither operand is fed anything
-- |      else (law 9).
-- |   6. **Output side — gate.** `m`'s emission is the union of the
-- |      operands' last contributions, released once both have spoken and
-- |      withheld before.
-- |   7. **Exactness — enforced.** An operand counts only at its declared
-- |      fields, and the carrier must see to it:
-- |      `recordToRecord w1 w2 ≈ recordToRecord (rmap exact w1) w2` — the
-- |      runtime trim `exactRow`, the evidence `OwnedRecordOutputs`
-- |      carries — so a runtime copy of a sibling's field never shadows
-- |      the sibling.
-- |   8. **Closure — the step and the pre-feed drop.** If `w1`, `w2` satisfy
-- |      1–3, so does `m` from its first feed on. Law 3 for `m` is the
-- |      **step**: one release per feed, after both operands answered — a
-- |      feed changing several fields emits one fresh row, never a torn
-- |      one. Law 2 for `m` is **retention**: a one-sided emission
-- |      completed with the sibling's last contribution. Before the first
-- |      feed `m` drops, never fabricates — the primed equivalence, the
-- |      named `Strong` deviation.
-- |   9. **Independence — the loop is `looped`.** The operands receive no
-- |      feeds but `m`'s, and an emission goes downstream, never to the
-- |      sibling. Cross-feed is the loop's — `looped m` — and only there.
-- |
-- | Every cell has a probe in test/Main.purs: 1 (`repetition ×→×`),
-- | 2 (`field: emission over carried background`, `×→× gating: incomplete
-- | record withheld`), 3 (`answer ×→×`, `one feed, one release`,
-- | `announce`, `display beside the wire` for the `{}` clause), 4 (`unit
-- | law ×→×`, `zero-field law ×→×`), 5 (`×→× associativity: the script
-- | reached every operand`, `one feed, one release`), 6 (`×→× gating`),
-- | 7 (`×→× exactness`), 8 (`one feed, one release`, `disjoint operands …
-- | release once`, `Strong deviation` for the drop), 9 (`independence
-- | ×→×`, with `looped` as the cross-feed contrast); the floor's two
-- | shared laws at this shape are `×→× symmetry`/`×→× associativity` and
-- | `enrichment: p ⊑ p' ⇒ p ⊗ r ⊑ p' ⊗ r`. Beyond the probes, laws 4–8,
-- | the shared laws and the merge's own law 1 are checked over **every
-- | script** to length 6 (two operands) or 8 (three) with a fresh token
-- | per event, and the effectful gate against `PUI.Gate`'s pure step, in
-- | test/Exhaustive.purs — complete, not sampled
-- | (doc/observational-semantics.md §9). Starvation reads off the set: a
-- | merge silent after its first feed has an operand breaking or
-- | refining law 3; one silent before any feed is unprimed.
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
  , forProperty
  , required
  , field
  , muted
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
import Control.Category (class Category, identity, (>>>))
import Data.Function (const)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record (get, insert, union) as Record
import Record.Unsafe.Union (unsafeUnion)
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

-- | The timeless carrier: both operands read the shot, their disjoint exact
-- | outputs union — the (×,×)-monoid on plain functions, which makes the
-- | merge's unit, associativity and symmetry laws pure equalities
-- | (test/Main.purs). The exactness trim mirrors the gated carriers': an
-- | operand echoing its coercion-widened input must not shadow its sibling's
-- | field.
instance RecordToRecord (->) where
  recordToRecord p1 p2 i = Record.union (exactRow (widenRecordInput p1 i)) (exactRow (widenRecordInput p2 i))

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

-- | The **faceless leaf**: reads nothing and contributes nothing. The
-- | wire's `lcmap`-closure (so a `Category` carrier), defined at **every**
-- | input because `{}` is terminal — there is one map to it from anything:
-- |
-- | ```
-- | blank = lcmap (const {}) identity    -- accept any input
-- | ```
-- |
-- | At record input it is the display that reads `()` of the fed row —
-- | stated as subsumption, like the gated displays' — the leaf for
-- | elements whose whole face is decorators, a channel-fed SVG shape or
-- | styled `div` (`circle >>> attrWith "fill" f $ blank`). At variant
-- | input it is the status that renders no occurrence: `action`'s progress
-- | slot when a vocabulary has no indicator (`blank # action f`). One word
-- | for both, since neither the record merges nor the variant merges await
-- | a zero-field contribution. An *element* with nothing in it is the other
-- | static, `PUI.static` (an ocular applied to the wire at `{} → {}`).
blank :: forall p a. Category p => Profunctor p => p a {}
blank = lcmap (const {}) identity

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
-- |
-- | How to read one: stages compose with `Category.do`, every emission
-- | travels left to right through them, and `mvu` loops the final emission
-- | back to the top — so a stage placed *before* another is not "above" it;
-- | all stages see every model value on the next loop turn. A counter (a
-- | `shown` display, then an emitter `# applied increment`, under
-- | `mvu { count: 0 }`) runs so:
-- |
-- |  1. registration: the seed `{ count: 0 }` is fed to the first stage;
-- |  2. the display shows `0` and releases the fed row, which flows on and
-- |     arms the emitter's replay value and `applied`'s retained state;
-- |  3. the user acts: the emitter fires, `applied` steps the retained
-- |     model by `increment` and emits `{ count: 1 }`;
-- |  4. the loop re-feeds `{ count: 1 }` to the top; the display
-- |     re-renders; the re-feed's own echoes are swallowed by the loop's
-- |     re-entrancy guard, so exactly one turn happens per event.
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
-- |
-- | Law (**background transparency**): `subStrong w` acts as `identity` on
-- | every field outside `w`'s focus row — a feed's background fields are
-- | re-attached to `w`'s emission verbatim, `w` never seeing them. This is
-- | `Strong`'s `first` law read at the row, a law of the strength rather
-- | than of the merge (which is why it is not among the header's nine);
-- | the parcel demo is its contract.
subStrong
  :: forall p f f' f'l b s s'
   . Strong p
  => ExclusiveRows f b s
  => ExclusiveRows f' b s'
  => RowToList f' f'l
  => FieldNames f'l f' f'
  => p { | f } { | f' }
  -> p { | s } { | s' }
subStrong g =
  dimap (\s -> Tuple (unsafeCoerce s) (unsafeCoerce s))
        -- `Record.union` is left-biased and does not nub. `ExclusiveRows f' b s'`
        -- keeps the typed halves disjoint, and `exactRow` trims the emission to
        -- its declared row first: `g` may answer a feed *later* than the feed
        -- that stocked the retained background (a debounced inner stage), so a
        -- fat echo's runtime copies of background fields can be genuinely stale
        -- — the same hazard the gated merges trim (runtime-exactness).
        (\(Tuple f' b) -> Record.union (exactRow f') b)
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
-- | sub-composites: a closed sub-form under a record-valued field is
-- | `subForm # field @l`, the background carried like any leaf's — and the
-- | MDC vocabularies fuse this nesting with the card surface and heading
-- | as the labelled group, `group @l` (`PUI.Web.MDC2`/`.MDC3`).
field
  :: forall @l p f f' b s s'
   . IsSymbol l
  => Cons l f b s
  => Cons l f' b s'
  => Strong p
  => p f f' -> p { | s } { | s' }
field = prop (Proxy @l)

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
-- | row) — the structural read (a display's field arrives ready to draw,
-- | so there is nothing to format): a packaged collection reads its array
-- | (`… # muted # atField @"entries"`, the packaged-collection-display
-- | protocol), nested chrome reads its sub-rows
-- | (`… # foreach @"name" identity # atField @"dishes"`).
atField :: forall @l p a o r. IsSymbol l => Profunctor p => Cons l a () r => p a o -> p { | r } o
atField = lcmap (Record.get (Proxy @l))

-- | The display-side `field @l` (the background is carried), for positions
-- | whose row the context already pins — collection items, pane payloads.
-- | The label is the leaf's own, read back out of its row, and the field
-- | passes **verbatim** — selection, never formatting: under the
-- | presentation-model rule a formatted text is a field of the row the
-- | producing business function writes — which is why this takes no
-- | function of its own.
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
  :: forall p i il o fb iw ow
   . Seeding p
  => Costrong p
  => ExclusiveRows i fb iw
  => ExclusiveRows o fb ow
  => RowToList i il
  => FieldNames il i i
  => { | iw }
  -> p { | iw } { | ow }
  -> p { | i } { | o }
feedback seed g =
  unfirst
    (dimap
      -- the join is left-biased; `exactRow` trims the fresh input to its
      -- declared row so a fat upstream emission cannot shadow the looped
      -- state fields with stale runtime copies (runtime-exactness)
      (\(Tuple i fb) -> Record.union (exactRow i) fb)
      -- coerce-split, as in `subStrong`: safe because `ExclusiveRows o fb ow`
      -- guarantees the two typed views are disjoint
      (\ow -> Tuple (unsafeCoerce ow) (unsafeCoerce ow))
      (seeded seed >>> g))
