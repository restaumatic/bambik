-- | A **row profunctor** is a profunctor `p a b` where type parameters `a`
-- | and `b` are **row types** under a carrier:
-- |
-- |   * **`Record`** — the product `×`: every row label present at once.
-- |   * **`Variant`** — the sum `+`: exactly one row label at a time.
-- |
-- | So `p (Record a) (Variant b)` (in short `p {|a} [|b]`) instantiates
-- | profunctor parameters as Record of row `a` and Variant of row `b`.
-- |
-- | Choosing a carrier for each parameter gives the four **row profunctor
-- | shapes**, one module each. Each shape indexes a unary
-- | **strength**/**co-strength** and a binary **merge** typeclass. Each
-- | strength and co-strength alike generates an **optic**.
-- |
-- | ```
-- | shape        strength     strength optic  co-strength    co-strength optic  merge
-- | -----------  -----------  --------------  -------------  -----------------  -----------------
-- | p {|a} {|b}  Strong       Lens            Costrong **    Colens *           RecordToRecord *
-- | p [|a] [|b]  Choice       Prism           Cochoice       Coprism *          VariantToVariant *
-- | p {|a} [|b]  Resolving *  Shutter *       Coresolving *  Coshutter *        RecordToVariant *
-- | p [|a] {|b}  Retaining *  Reel *          Coretaining *  Coreel *           VariantToRecord *
-- | ```
-- |
-- | `*` marks what this library introduces; the rest is the ecosystem's.
-- | `**`: the row form is built on the pointed trace `PointedCostrong` *
-- | (`Data.Profunctor.PointedCostrong` — `unfirst` with its state channel
-- | started at a given value), because the raw `unfirst` composite is dead
-- | on a gated carrier and priming it from inside would need an input seed
-- | too
-- | (`Strong`/`Choice` with their `Lens`/`Prism`, and the duals
-- | `Costrong`/`Cochoice` — whose optics `Colens`/`Coprism`, however, the
-- | ecosystem never built). For the ecosystem pairs the optics follow by
-- | Pastro–Street (`Strong`/`Choice` are lawful Tambara modules, so
-- | `Lens`/`Prism` — and, dually, `Colens`/`Coprism` — are representation
-- | theorems). The four **coined** optics are profunctor-encoded classes
-- | with **sound existential constructors and no completeness claim**: the
-- | mixed strengths carry no unit or composition coherence
-- | (`Data.Profunctor.Resolving`), so Pastro–Street does not apply — and
-- | indeed `identity` inhabits `Shutter a b a b` while no existential
-- | shutter produces it (every existential carries an escape `s → t`).
-- | Neither the classes nor the optics
-- | mention a row, so neither lives in `Data.Profunctor.Row.*`. Both follow
-- | the ecosystem's own layout: one class per module beside `Strong`/`Costrong`
-- | (`Data.Profunctor.Resolving`/`.Coresolving`/`.Retaining`/`.Coretaining`),
-- | and one optic per module beside `Data.Lens.Lens`/`.Prism`
-- | (`Data.Lens.Colens`/`.Coprism`/`.Shutter`/`.Coshutter`/`.Reel`/`.Coreel`).
-- | Claiming those ecosystem names is a claim about what the modules are, so
-- | they sit outside `src/` altogether, under the `extras/profunctor` and
-- | `extras/lenses` source roots: complements of the ecosystem's families,
-- | mentioning no `PUI`, no row and no carrier. A `Data.Profunctor.Row.*`
-- | module holds only what is about rows — the merge, its unit, and the
-- | placements and trace row forms below.
-- |
-- | The row layer itself is a **third** source root, `extras/row-profunctor`,
-- | which is a different claim again: these modules are bambik's own
-- | invention rather than anyone's complement, but they are still
-- | carrier-agnostic — the algebra of merging labelled rows, with `PUI` only
-- | one carrier that satisfies it. What remains in `src/` is the carrier and
-- | its vocabularies (`PUI`, `PUI.Web.*`), so the split is: `src/` is the UI
-- | library, `extras/` is the algebra it stands on.
-- |
-- | The **pure** shapes' `Strong`/`Choice` are Tambara modules for the `×` and
-- | `+` actions. On the **mixed** shapes the background *crosses* carriers
-- | (`resolve :: p a b -> p (Tuple a c) (Either b c)`), which is not a Tambara
-- | action — hence the coinage, and hence `PUI m` instances but no `(->)`:
-- | `resolve` needs quiescence (time), `retain` needs memory (state).
-- |
-- | What makes such a profunctor a row profunctor is not its shape alone but
-- | the structure that shape supports: for each shape, a **merge** combining
-- | two profunctors over labelled rows into one over the
-- | combined row, with a forced unit (the table under *The laws*) — so every
-- | shape is a monoid on labelled rows, written with qualified-do
-- | (`RecordToRecord.do`), and the labels of the merged output are exactly
-- | the labels the operands own.
-- |
-- | Around each merge sit the functions that place a profunctor **into** a
-- | row. They divide by what each needs — the same three columns as the
-- | table above, so a function's column is its power — and a function lives
-- | in the module of the sides it constrains: one polymorphic on one side
-- | sits in the diagonal module of the side it constrains.
-- |
-- | ```
-- | shape        Profunctor only                 over the strength            over the co-strength
-- | -----------  ------------------------------  ---------------------------  --------------------
-- | p {|a} {|b}  asField, muted, settled         subStrong, focusField       feedback
-- | p [|a] [|b]  atCase, toCase, forCase         subChoice, focusCase         iterate
-- | p {|a} [|b]  silenced                        subResolving                 folding
-- | p [|a] {|b}  fold                            subRetaining                 unfolding
-- | ```
-- |
-- | The **left** column is `dimap` alone: renaming and rewrapping labels, with
-- | nothing threaded and no state. The **middle** column carries a
-- | **background** the strength threads. The sub-row family is named for the
-- | strength it stands on, so each name is the first constraint in its own
-- | signature (`subStrong`/`subChoice`/`subResolving`/`subRetaining`) — a
-- | strength names the carrier *pair*, so no side is privileged, where a
-- | carrier word would be honest on the pure shapes and half-true on the
-- | mixed ones. The pure shapes add their ecosystem single-label optic
-- | (`focusField`, the Lens, also the leaf lift making every label-indexed
-- | editor a whole-row citizen; `focusCase`, the Prism). The mixed shapes
-- | have none: a single label, or a single label's complement, is the
-- | sub-row focus at a singleton row plus an adopter. The **right** column
-- | ties a state channel off with the co-strength — one trace row form per
-- | shape, each seeded with its state's starting value but `iterate`
-- | (entities pre-exist, events occur).
-- |
-- | The **left** column is generated by two choices: which side is
-- | reshaped (`lcmap` or `rmap`) and that side's carrier.
-- |
-- | ```
-- |                          input ×      input +    output ×   output +
-- | -----------------------  -----------  ---------  ---------  ------------
-- | bare, closed singleton   —            atCase     —          toCase
-- |                                       forCase
-- | ```
-- |
-- | Business functions are arguments of leaves, never adopters: a display
-- | takes its **read function** (`text lineOf`), a status its business case
-- | and copy function (`snackbar @"booked" bookedLine`), and an emitter emits its own
-- | case, which the fold consumes (doc/research-copy-is-a-function.md). So
-- | the grid holds only the structural `atCase`/`toCase`, and `forCase @l` —
-- | the status face's plumbing, reading the face's own case back out of its
-- | closed singleton row via `RowToList`'s fundep, as `focusField` is the
-- | editor face's. The rename `asField` survives
-- | only where a packaged control fuses a canonical core to a surface
-- | label.
-- |
-- | The record columns are empty. Output `×` is owned, so a bare value could
-- | become a field only as the whole row, and a label-indexed leaf already
-- | emits its labelled row (`focusField @l` lifts an editor into it); turning an
-- | occurrence into a field value needs the rest of the row, which only
-- | `focusField @l`'s retained background can supply over `Strong`. Input `×`
-- | is read by the leaf's own read function or through `focusField @l`, so the
-- | record-input readers were pruned (L14). The one entry outside the grid
-- | is `splitVariant`, a plain function rather than a placement.
-- |
-- | The merge's two obligations are per-side and dual, and they are what the
-- | constraint vocabulary below spells out: on an **input** side, where does
-- | each label's value come from; on an **output** side, who is allowed to
-- | produce it. Records share their input (every operand is fed the whole
-- | row) and own their output (each field has exactly one producer);
-- | variants own their input (each case has exactly one handler) and share
-- | their output (any operand may emit any case). A shared record input is
-- | **one row** — the operands' inputs are the merge's, and an operand's
-- | business functions are typed at that row (guardrails L18); a shared variant output is **inclusive**
-- | (`InclusiveRows`); ownership is exclusive (`ExclusiveRows`) — so a merge
-- | signature is two words, one per side.
-- |
-- | `Data.Profunctor.Acting` extends the family one step past rows: rows are
-- | the finitary μ-free fragment of the container grammar, and `Array` is
-- | one `μ` later.
-- |
-- | The shared floor of the row layer — what every shape's module
-- | (`Data.Profunctor.Row.*`) stands on:
-- |
-- |   * **row-constraint vocabulary** — `InclusiveRows` (overlapping rows,
-- |     deduped union: variant outputs), `ExclusiveRows`
-- |     (disjoint partition: variant inputs, record outputs),
-- |     `DispatchableVariants` (runtime tag evidence for variant dispatch).
-- |     Their meanings come from the row-profunctor reading: everyone may
-- |     read a record field / offer a variant case, but each variant case
-- |     must have exactly one handler and each record field exactly one
-- |     producer. `MergeableRecords` adds the **runtime-exactness** evidence the
-- |     gated merges use to trim operand emissions to their declared
-- |     output rows (`exactRow`).
-- |   * **reshapings** — `dimap`-only structural adapters that grow or
-- |     shrink one row-typed side, with nothing flowing through the added
-- |     or dropped labels.
-- |
-- | Everything needs only `Profunctor`; the strengths
-- | (`Strong`/`Choice`/`Resolving`/`Retaining`) and the merges build above.
-- |
-- | ## The laws
-- |
-- | Six statements, stated here once and read at each shape in the four
-- | shape modules ("Laws at `×→×`" and its siblings). All are stated
-- | against doc/observational-semantics.md §1–2: a **script** is a finite
-- | interleaving of registration, feeds (`feed x`, at a record input),
-- | occurrences (`occur e`, at a variant input) and inner firings; an
-- | **emission** is an output on the boundary channel; a **step** is one
-- | feed's synchronous cascade; `≈` is equality of boundary emission
-- | streams under every script — up to stutter (consecutive duplicates)
-- | on a record channel, exact on a variant channel; `⊑` is "emits a
-- | subsequence of, under every script". A record is **knowledge** (a
-- | value between inputs), a variant an **event** (none).
-- |
-- | **Component laws** — what a component `w` owes its carrier. Both are
-- | obligations of a **record input**: a record input is shared, one feed
-- | reaches every operand of a merge, so the boundary can be owed at most
-- | one thing back. A variant input is dispatched to one owner and owes
-- | nothing. The type cannot carry either law, so they are the protocol a
-- | vocabulary provider discharges at its leaves.
-- |
-- |   1. **Repetition.** `feed x ; feed x ≈ feed x`.
-- |   2. **Answer.** Every `feed x` is answered within its step:
-- |      at `×→×` by a row — at least one emission, every emission of the
-- |      step equal to the step's last (no torn row), and counting
-- |      renderings rather than up to stutter, exactly one;
-- |      at `×→+` by nothing — a feed arms a source and never fires it.
-- |
-- | **Merge laws** — for each of the four merge classes, `m = merge w1 w2`,
-- | `u` the shape's unit and `π_k` the projection of the merge's input onto
-- | operand `k`: the whole row at a record input, the occurrences of `k`'s
-- | own cases at a variant input.
-- |
-- |   3. **Monoid.** `merge u w = w = merge w u`, on the nose;
-- |      `merge w1 w2 ≈ merge w2 w1`;
-- |      `merge (merge w1 w2) w3 ≈ merge w1 (merge w2 w3)`.
-- |   4. **Projection.** Each operand sees exactly its projection and
-- |      counts exactly at its declared labels: the inner feed stream of
-- |      `w_k` under `m` is `π_k` of `m`'s boundary input stream — a
-- |      sibling's emission never reaches it — and
-- |      `merge w1 w2 ≈ merge (exact w1) w2`, `exact` trimming an emission
-- |      to the operand's declared output labels.
-- |   5. **Preservation.** If `w1` and `w2` satisfy 1 and 2, so does `m`.
-- |   6. **Monotonicity.** `w1 ⊑ w1'` implies `merge w1 w2 ⊑ merge w1' w2`.
-- |
-- | Read at the four shapes — the slots the shapes differ in:
-- |
-- | ```
-- |                 ×→×               ×→+             +→+                     +→×
-- | --------------  ----------------  --------------  ----------------------  ---------------------
-- | 1 repetition    owed              owed            —                       —
-- | 2 answer        a row, once       nothing         —                       —
-- | 3 unit u        the {} wire       silence         identity @(Variant ())  lcmap case_ identity
-- | 4 π_k           whole row         whole row       own cases               own cases
-- |   exact         trim              identity        identity                trim
-- | 5 preservation  of 1 and 2        of 1 and 2      vacuous                 vacuous
-- | 6 monotonicity  the same at every shape
-- | ```
-- |
-- | `exact` is the identity at a variant output because a variant carries
-- | its one tag: `widenVariantOutput` is `rmap expand`, and
-- | `SharedVariantOutputs` carries no evidence. At a record output the
-- | trim is `exactRow`, the runtime evidence `OwnedRecordOutputs` carries,
-- | so an operand's stale runtime copy of a sibling's field never shadows
-- | the sibling. Law 2's liveness half is a **leaf** law: a stage may
-- | refine it in time (`confirmed` releases on confirmation, the gather
-- | gate once every element has spoken), and by 6 the merge refines with
-- | it. `{}` is always known, so a `{}` output is answered by `{}` — which
-- | no gate awaits — and a `{}`-input component counts registration as
-- | its feed (`announce`, the point). The `×→×` unit is the `{}` wire at
-- | the merge's own input, `lcmap (const {}) identity` (`blank`): the
-- | operands of a `×→×` merge share one input row, so the unit is the
-- | terminal arrow out of it — `identity @{}` is that wire only at `{}`.
-- |
-- | **What is not a law here.** That an emitted `{ | o }` is whole is the
-- | type. That `focusField @l` re-attaches the background, that `clicked`
-- | replays the row last fed, that `settled`'s normalizer is idempotent
-- | are laws of those words, stated at them. That an occurrence twice is
-- | two, that a handler answers any number of times, that a fold releases
-- | when its state allows, is the absence of a quotient at a variant
-- | input, not an obligation. Broadcast, dispatch, gate and passage are
-- | how `PUI` satisfies 3–6 (below), not what the classes demand: `(->)`
-- | satisfies the two diagonal merges' laws with none of them.
-- |
-- | **How `PUI` discharges them.** Input, law 4's first half: broadcast in
-- | one step at `×`, dispatch to the one owner at `+`. Output: at `×` the
-- | gate `PUI.Gate.gateStep` — retain each side's last contribution,
-- | release their union once every owned side has spoken, once per step;
-- | before that withhold, and **drop** rather than delay (the primed
-- | equivalence, doc §4); at `+` passage — each emission exits as it
-- | occurs, nothing retained. The gate is the one canonical way to pair
-- | two streams into a stream of pairs, so every `(·,×)` shape gates and
-- | no `(·,+)` shape does — the container action's `Array b` included,
-- | gathered by the same machine over the fed keys as labels
-- | (`Data.Profunctor.Acting`) — and the unit is forced, not designed: a wire
-- | into the unit object wherever a wire fits (a zero-field side is born
-- | spoken), `silence` at the one shape no wire reaches. Counting
-- | renderings a gated merge is premonoidal — interchange at the inner
-- | surfaces holds as `⊑` — and counting channels monoidal; at the
-- | boundary interchange holds on the nose because a feed is one step
-- | (doc §4). Since operands share one input row, interchange is stated
-- | with the projections explicit: `merge (f >>> h) (g >>> k)` against
-- | `merge f g >>> merge (π₁ h) (π₂ k)`, `π` the widening
-- | (`widenRecordInput`) of each second stage to the middle row. Starvation reads off the laws: a gated merge silent after
-- | every owned side has been fed has an operand breaking 2; one silent
-- | before that has an unprimed owned field (`with`/`mvu`, `seeded`, the
-- | seed of a trace form).
-- |
-- | **Coverage.** Laws 3, 4 and 6 and the gate's conformance to its pure
-- | step are checked over every script to a bound at every shape in
-- | test/Exhaustive.purs — complete, since the gate is a data-independent
-- | machine with finite control (doc §9); so is 5 wherever it has content
-- | (arming at `×→+`; repetition and the one untorn release per feed at
-- | `×→×`). The component laws at each shape's own wire or source, the
-- | `{}` clauses and the `looped` contrast are named probes in
-- | test/Main.purs, each carrying its law and shape (`repetition ×→×`,
-- | `answer ×→+`, `projection +→+`, `monotonicity at +→×`). Outside the
-- | laws, being about the carrier: the trace asymmetry (doc §5) and the
-- | named ecosystem deviations (doc §4). See
-- | doc/collections-profunctor-algebra.md §1.
-- |
-- | Reshape vs focus: a
-- | reshape *drops* the complement — extra record fields are simply never
-- | read (free coercion), extra variant cases are never emitted (`expand`)
-- | — while a focus *threads* it (`Strong`/`Choice`).
module Data.Profunctor.Row
  ( class InclusiveRows
  , class ExclusiveRows
  , splitVariant
  , class DispatchableVariants
  , class MergeableRecords
  , class FieldNames
  , class SharedRecordInputs
  , class SharedVariantOutputs
  , class OwnedVariantInputs
  , class OwnedRecordOutputs
  , class DisjointLabels
  , class LabelAbsent
  , class LabelAbsentK
  , class LabelsDoc
  , class NoDuplicateLabels
  , class NoDuplicateLabelsK
  , class RowLabels
  , exactRow
  , fieldNames
  , rowLabels
  , widenRecordInput
  , widenVariantOutput
  )
  where

import Prelude (identity, (<<<), (<>))

import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (class Contractable, contract, expand)
import Effect.Exception.Unsafe (unsafeThrow)
import Data.Variant.Internal (class VariantTags)
import Prim.Ordering (Ordering, LT, EQ, GT)
import Prim.Row (class Cons, class Lacks, class Nub, class Union) as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList (Cons, Nil) as RL
import Prim.Symbol (class Append, class Compare) as Symbol
import Prim.TypeError (class Fail, Above, Beside, Text)
import Record (get) as Record
import Record.Builder (Builder)
import Record.Builder (buildFromScratch, insert) as Builder
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- =====================================================================
-- Row-constraint vocabulary
-- =====================================================================

-- r1 and r2 may overlap; r is their deduped union; both r1 ⊆ r and r2 ⊆ r.
-- Witness rows: r12 = r1 ∪ r2 (pre-nub), r1x = r ∖ r1, r2x = r ∖ r2.
class InclusiveRows :: forall k. Row k -> Row k -> Row k -> Row k -> Row k -> Row k -> Constraint
class
  ( Row.Union r1 r2 r12
  , Row.Nub r12 r
  , Row.Union r1 r1x r
  , Row.Union r2 r2x r
  ) <= InclusiveRows r1 r2 r r12 r1x r2x

instance
  ( Row.Union r1 r2 r12
  , Row.Nub r12 r
  , Row.Union r1 r1x r
  , Row.Union r2 r2x r
  ) => InclusiveRows r1 r2 r r12 r1x r2x

-- r1 and r2 are disjoint; their union is r.
class ExclusiveRows :: forall k. Row k -> Row k -> Row k -> Constraint
class
  ( Row.Union r1 r2 r
  , Row.Union r2 r1 r
  ) <= ExclusiveRows r1 r2 r

instance
  ( Row.Union r1 r2 r
  , Row.Union r2 r1 r
  ) => ExclusiveRows r1 r2 r

-- Variants r1 and r2 carry runtime tag info for dispatch.
-- Witness lists: r1l = RowToList r1, r2l = RowToList r2.
class DispatchableVariants :: forall k1 k2. Row k1 -> Row k2 -> RowList k1 -> RowList k2 -> Constraint
class
  ( RowToList r1 r1l
  , VariantTags r1l
  , RowToList r2 r2l
  , VariantTags r2l
  ) <= DispatchableVariants r1 r2 r1l r2l

instance
  ( RowToList r1 r1l
  , VariantTags r1l
  , RowToList r2 r2l
  , VariantTags r2l
  ) => DispatchableVariants r1 r2 r1l r2l

-- =====================================================================
-- Runtime exactness
-- =====================================================================

-- | Rebuild a record field-by-field so its **runtime** shape is exactly its
-- | row — no more, no less. A record's type never guarantees its runtime
-- | object carries only the declared labels: the widening reshapings above
-- | are coercions, so a UI component that echoes or lens-rebuilds its input emits
-- | an object runtime-carrying every field of the *merged* row while typed
-- | at its own narrow slice. The gated merges use `exactRow` to trim each
-- | operand's emission to its declared output row before the left-biased
-- | `Record.union`, so stale runtime copies of *sibling* fields can never
-- | shadow the siblings' genuine contributions.
exactRow :: forall r rl. RowToList r rl => FieldNames rl r r => { | r } -> { | r }
exactRow r = Builder.buildFromScratch (fieldNames (Proxy @rl) r)

-- | Rows o1 and o2 carry runtime rebuild evidence for the gated merges'
-- | exactness trim (`exactRow`). Witness lists: o1l = RowToList o1,
-- | o2l = RowToList o2 — the `DispatchableVariants` pattern, so the merge
-- | instances can discharge `exactRow`'s constraints from the givens'
-- | superclasses.
class MergeableRecords :: Row Type -> Row Type -> RowList Type -> RowList Type -> Constraint
class
  ( RowToList o1 o1l
  , FieldNames o1l o1 o1
  , RowLabels o1l
  , RowToList o2 o2l
  , FieldNames o2l o2 o2
  , RowLabels o2l
  ) <= MergeableRecords o1 o2 o1l o2l

instance
  ( RowToList o1 o1l
  , FieldNames o1l o1 o1
  , RowLabels o1l
  , RowToList o2 o2l
  , FieldNames o2l o2 o2
  , RowLabels o2l
  ) => MergeableRecords o1 o2 o1l o2l

-- | `RowList`-indexed worker for `exactRow`: copies exactly the listed
-- | labels out of `from` into a freshly built record.
class FieldNames :: RowList Type -> Row Type -> Row Type -> Constraint
class FieldNames rl from to | rl -> to where
  fieldNames :: Proxy rl -> { | from } -> Builder {} { | to }

instance FieldNames RL.Nil from () where
  fieldNames _ _ = identity

instance
  ( IsSymbol l
  , Row.Cons l a fromRest from
  , Row.Cons l a toRest to
  , Row.Lacks l toRest
  , FieldNames rl from toRest
  ) => FieldNames (RL.Cons l a rl) from to where
  fieldNames _ r = Builder.insert (Proxy @l) (Record.get (Proxy @l) r) <<< fieldNames (Proxy @rl) r

-- | Reify a `RowList`'s labels as runtime strings — the evidence the gated
-- | merges' **starvation diagnostics** use to *name* the fields a gate is
-- | still waiting for (the compile-time sibling of `FieldNames`, which
-- | copies the fields' values).
class RowLabels :: forall k. RowList k -> Constraint
class RowLabels rl where
  rowLabels :: Proxy rl -> Array String

instance RowLabels RL.Nil where
  rowLabels _ = []

instance (IsSymbol l, RowLabels rest) => RowLabels (RL.Cons l a rest) where
  rowLabels _ = [ reflectSymbol (Proxy @l) ] <> rowLabels (Proxy @rest)

-- =====================================================================
-- Side vocabulary: one constraint per merge side
-- =====================================================================
--
-- The four merges' constraints factor exactly by side, under one law:
-- **sharing is open, responsibility is exclusive** — a shared record input
-- is one row every operand is fed whole, a shared variant output an
-- inclusive union any operand may emit into — and runtime
-- label evidence appears only on the exclusive sides, where the merge's
-- runtime action is label-driven (dispatch, union) rather than
-- label-blind (broadcast, expand). Records are read-shared but
-- write-owned; variants are emit-shared but handle-owned. Each merge
-- signature is then two words, one per side:
--
--   recordToRecord   : SharedRecordInputs  + OwnedRecordOutputs
--   recordToVariant  : SharedRecordInputs  + SharedVariantOutputs
--   variantToVariant : OwnedVariantInputs  + SharedVariantOutputs
--   variantToRecord  : OwnedVariantInputs  + OwnedRecordOutputs
--
-- **What a shared input side obliges.** A shared record input is a
-- *broadcast*: one feed reaches both operands, so both may answer it. That
-- is the sole cause of the two feed laws, and it states them as one
-- sentence — **one feed in, at most one thing out, and if it is a record,
-- a whole one** — differentiated only by what the output side can carry:
--
--   * output `×` — one thing, whole: the gated merge batches the broadcast
--     and releases **once** (`steppedFeed` on the carrier), so a feed
--     changing several fields never emits a torn row.
--   * output `+` — one thing: an operand must not turn a feed into an
--     occurrence (no synchronous **event** echo), or the pass-through
--     broadcast would manufacture a second emission.
--
-- An exclusive variant input side carries neither obligation, and this is
-- a consequence rather than a coincidence: `DisjointLabels` gives every
-- case exactly one handler, so a feed is *dispatched*, not broadcast, and
-- exactly one operand answers. There is no "one feed, several answers"
-- situation to discipline — nothing to tear, nothing to duplicate.
--
-- The two axes are therefore independent, and the mechanisms follow both:
-- the **input** side says whether the obligation exists at all (broadcast
-- merges have it, dispatch merges do not), the **output** side says what
-- discharges it (a record output gates and retains, so "one thing" also
-- means "whole"; a variant output passes through, so it only means "do
-- not manufacture a second"). `variantToRecord` is the case that separates
-- them: dispatched input, so no broadcast to batch, yet a record output, so
-- it gates and retains exactly like `recordToRecord`.

-- | A merge's **record-input side**: every operand is fed the merge's whole
-- | row — the operands' input rows *are* the merge's (an equality, no
-- | `Union`). The merge action is a label-blind broadcast, so no runtime
-- | evidence is needed. An operand's business functions are typed at the row
-- | (`countLine :: { count :: Int } -> String`, the view's hint verbatim)
-- | and checked by unification, which a bare hole never leaves stuck
-- | (guardrails L18); the
-- | equality carries the context's row down into the operands.
class SharedRecordInputs :: Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class SharedRecordInputs i1 i2 i i12 i1x i2x

instance (TypeEquals (Record i1) (Record i), TypeEquals (Record i2) (Record i)) => SharedRecordInputs i1 i2 i i12 i1x i2x

-- | A merge's **variant-output side**: anyone may emit a case, so operand
-- | rows may overlap. The merge action is a label-blind `expand` — no
-- | runtime evidence needed.
class SharedVariantOutputs :: Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class InclusiveRows o1 o2 o o12 o1x o2x <= SharedVariantOutputs o1 o2 o o12 o1x o2x

instance InclusiveRows o1 o2 o o12 o1x o2x => SharedVariantOutputs o1 o2 o o12 o1x o2x

-- =====================================================================
-- Custom diagnostics
-- =====================================================================
--
-- A duplicated label on an owned merge side otherwise dies deep inside the
-- exactness evidence as an anonymous `Lacks` failure. This detector
-- walks both label lists and, on the first shared
-- label, fails with a message that *names* it — and, so the offending
-- operand can be found at a glance, renders **both operands' full label
-- sets** into the error via `LabelsDoc`.

-- | Render a `RowList`'s labels as one type-level `Symbol` — `"a, b, c"` —
-- | for use inside `Fail` messages (the `Text`-level sibling of
-- | `RowLabels`).
class LabelsDoc :: forall k. RowList k -> Symbol -> Constraint
class LabelsDoc rl s | rl -> s

instance LabelsDoc RL.Nil ""
else instance LabelsDoc (RL.Cons l a RL.Nil) l
else instance (LabelsDoc rest s, Symbol.Append ", " s s', Symbol.Append l s' out) => LabelsDoc (RL.Cons l a rest) out

-- The walker threads the *original* two lists alongside (callers pass the
-- lists twice: `DisjointLabels l1 l2 l1 l2`), so the failure instance can
-- render both operands' complete label sets.
class DisjointLabels :: forall k1 k2. RowList k1 -> RowList k2 -> RowList k1 -> RowList k2 -> Constraint
class DisjointLabels walk l2 own other

instance DisjointLabels RL.Nil l2 own other
instance (LabelAbsent l l2 own other, DisjointLabels rest l2 own other) => DisjointLabels (RL.Cons l a rest) l2 own other

class LabelAbsent :: forall k1 k2. Symbol -> RowList k2 -> RowList k1 -> RowList k2 -> Constraint
class LabelAbsent l rl own other

instance LabelAbsent l RL.Nil own other
instance (Symbol.Compare l l' ord, LabelAbsentK ord l rest own other) => LabelAbsent l (RL.Cons l' a rest) own other

class LabelAbsentK :: forall k1 k2. Ordering -> Symbol -> RowList k2 -> RowList k1 -> RowList k2 -> Constraint
class LabelAbsentK ord l rest own other

instance
  ( LabelsDoc own ownDoc
  , LabelsDoc other otherDoc
  , Fail
      ( Above
          (Beside (Beside (Text "Two merge operands own the label \"") (Text l)) (Text "\"."))
          (Above
            (Beside (Beside (Beside (Beside (Text "One operand owns { ") (Text ownDoc)) (Text " }, the other { ")) (Text otherDoc)) (Text " }."))
            (Above
              (Text "On an owned merge side each label belongs to exactly one operand: every record-output field has ONE producer, every variant-input case has ONE handler.")
              (Text "Look for the duplicated `asField`/`focusField`/`atCase` label in this `do` block.")))
      )
  ) => LabelAbsentK EQ l rest own other
instance LabelAbsent l rest own other => LabelAbsentK LT l rest own other
instance LabelAbsent l rest own other => LabelAbsentK GT l rest own other

-- The same defect can also surface *within* one operand's inferred row:
-- unification can build a single row carrying a label twice (e.g. the tail
-- of a `do` block against a pinned total row). `RowToList` sorts, so
-- duplicates are adjacent — one pass catches them; the original list rides
-- along (callers pass the list twice: `NoDuplicateLabels rl rl`) so the
-- failure names the whole row.

class NoDuplicateLabels :: forall k. RowList k -> RowList k -> Constraint
class NoDuplicateLabels walk orig

instance NoDuplicateLabels RL.Nil orig
else instance NoDuplicateLabels (RL.Cons l a RL.Nil) orig
else instance (Symbol.Compare l l' ord, NoDuplicateLabelsK ord l (RL.Cons l' b rest) orig) => NoDuplicateLabels (RL.Cons l a (RL.Cons l' b rest)) orig

class NoDuplicateLabelsK :: forall k. Ordering -> Symbol -> RowList k -> RowList k -> Constraint
class NoDuplicateLabelsK ord l rest orig

instance
  ( LabelsDoc orig origDoc
  , Fail
      ( Above
          (Beside (Beside (Text "A merge operand's row owns the label \"") (Text l)) (Text "\" twice."))
          (Above
            (Beside (Beside (Text "The row is { ") (Text origDoc)) (Text " }."))
            (Above
              (Text "On an owned merge side each label belongs to exactly one operand: every record-output field has ONE producer, every variant-input case has ONE handler.")
              (Text "Look for the duplicated `asField`/`focusField`/`atCase` label in this `do` block.")))
      )
  ) => NoDuplicateLabelsK EQ l rest orig
instance NoDuplicateLabels rest orig => NoDuplicateLabelsK LT l rest orig
instance NoDuplicateLabels rest orig => NoDuplicateLabelsK GT l rest orig

-- | A merge's **variant-input side**: every case has exactly one handler
-- | (disjoint rows), and routing a value to its handler is label-driven —
-- | `DispatchableVariants` supplies the runtime tags `contract` compares.
class OwnedVariantInputs :: Row Type -> Row Type -> Row Type -> RowList Type -> RowList Type -> Constraint
class
  ( NoDuplicateLabels i1l i1l
  , NoDuplicateLabels i2l i2l
  , DisjointLabels i1l i2l i1l i2l
  , ExclusiveRows i1 i2 i
  , DispatchableVariants i1 i2 i1l i2l
  ) <= OwnedVariantInputs i1 i2 i i1l i2l

instance
  ( RowToList i1 i1l
  , RowToList i2 i2l
  , NoDuplicateLabels i1l i1l
  , NoDuplicateLabels i2l i2l
  , DisjointLabels i1l i2l i1l i2l
  , ExclusiveRows i1 i2 i
  , DispatchableVariants i1 i2 i1l i2l
  ) => OwnedVariantInputs i1 i2 i i1l i2l

-- | A merge's **record-output side**: every field has exactly one producer
-- | (disjoint rows), and combining contributions is label-driven —
-- | `MergeableRecords` supplies the runtime field names `exactRow` trims
-- | with before the gates' union.
class OwnedRecordOutputs :: Row Type -> Row Type -> Row Type -> RowList Type -> RowList Type -> Constraint
class
  ( NoDuplicateLabels o1l o1l
  , NoDuplicateLabels o2l o2l
  , DisjointLabels o1l o2l o1l o2l
  , ExclusiveRows o1 o2 o
  , MergeableRecords o1 o2 o1l o2l
  ) <= OwnedRecordOutputs o1 o2 o o1l o2l

instance
  ( RowToList o1 o1l
  , RowToList o2 o2l
  , NoDuplicateLabels o1l o1l
  , NoDuplicateLabels o2l o2l
  , DisjointLabels o1l o2l o1l o2l
  , ExclusiveRows o1 o2 o
  , MergeableRecords o1 o2 o1l o2l
  ) => OwnedRecordOutputs o1 o2 o o1l o2l

-- =====================================================================
-- Whole-row reshapings
-- =====================================================================

widenRecordInput :: forall p r1 r a.
  Profunctor p =>
  p { | r1 } a -> p { | r } a
widenRecordInput = lcmap unsafeCoerce

widenVariantOutput :: forall p a v1 v2 v.
  Profunctor p =>
  Row.Union v1 v2 v =>
  p a [ | v1 ] -> p a [ | v ]
widenVariantOutput = rmap expand

-- | Dispatch a shot into the focused sub-variant or the background — a
-- | plain row function, no profunctor in sight, which is why it sits on the
-- | floor rather than in a shape module. `subChoice`, `iterate` and
-- | `subRetaining` all split with it.
splitVariant
  :: forall v1 b v
   . ExclusiveRows v1 b v
  => Contractable v v1
  => Contractable v b
  => [ | v ]
  -> Either [ | v1 ] [ | b ]
splitVariant v = case contract v of
  Just f -> Left f
  Nothing -> case contract v of
    Just b -> Right b
    Nothing -> unsafeThrow "splitVariant: case in neither focus nor background"
