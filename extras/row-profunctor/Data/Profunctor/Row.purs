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
-- | p {|a} {|b}  Strong       Lens            Costrong       Colens *           RecordToRecord *
-- | p [|a] [|b]  Choice       Prism           Cochoice       Coprism *          VariantToVariant *
-- | p {|a} [|b]  Resolving *  Shutter *       Coresolving *  Coshutter *        RecordToVariant *
-- | p [|a] {|b}  Retaining *  Reel *          Coretaining *  Coreel *           VariantToRecord *
-- | ```
-- |
-- | `*` marks what this library introduces; the rest is the ecosystem's
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
-- | combined row, with `Category`'s `identity` at the empty row as unit — so
-- | every shape is a monoid on labelled rows, written with qualified-do
-- | (`RecordToRecord.do`), and the labels of the merged row are exactly the
-- | labels of the operands.
-- |
-- | Around each merge sit the functions that place a profunctor **into** a
-- | row. They divide by what each needs — the same three columns as the
-- | table above, so a function's column is its power:
-- |
-- | ```
-- | shape        Profunctor only                 over the strength            over the co-strength
-- | -----------  ------------------------------  ---------------------------  --------------------
-- | p {|a} {|b}  atField, forProperty,           subStrong, field, required  feedback
-- |              asField
-- | p [|a] [|b]  atCase, splitVariant            subChoice, focusCase         iterate
-- | p {|a} [|b]  toCase, recordToCase,           subResolving,                folding
-- |              toCases                         backgroundProperty
-- | p [|a] {|b}  forCase, forCases              subRetaining, focusCase      unfolding
-- |                                              backgroundCase
-- | ```
-- |
-- | The **left** column is `dimap` alone: renaming and rewrapping labels, with
-- | nothing threaded and no state — `atField` reads, `asField`/`forCase` rename
-- | a canonical row, `toCase` introduces one. The **middle** column carries a
-- | **background** the strength threads. The sub-row family is named for the
-- | strength it stands on, so each name is the first constraint in its own
-- | signature (`subStrong`/`subChoice`/`subResolving`/`subRetaining`) — a
-- | strength names the carrier *pair*, so no side is privileged, where a
-- | carrier word would be honest on the pure shapes and half-true on the
-- | mixed ones. The rest name a single label (`field`/`focusCase`)
-- | or that label's complement (`backgroundProperty`/`backgroundCase`);
-- | `field` is also the leaf lift, making every label-indexed editor a
-- | whole-row citizen, and `required` its selector sibling. The
-- | **right** column ties a state channel off with
-- | the co-strength — one trace row form per shape, each seeded but `iterate`
-- | (entities pre-exist, events occur).
-- |
-- | The **complement** cells are blank on the pure shapes for a reason:
-- | `ExclusiveRows f b s` is symmetric, so `subStrong`/`subChoice` may be
-- | pointed at either half of a split, and "hold `l`, transform the rest" is
-- | already one of them at the singleton complement. Only on the mixed shapes
-- | do the two halves differ, because the escaping half must cross carriers,
-- | and there are two ways to cross: wrapped whole at a synthetic label
-- | (`subResolving` sends the background across as case `w`) or,
-- | when what escapes is a single label, injected under its own
-- | (`backgroundProperty`/`backgroundCase`). So `background*` is not the
-- | complement of `focus*` so much as the **label-preserving** crossing, and
-- | it exists only at single-label granularity.
-- |
-- | The **left** column is generated by three choices: which side is
-- | reshaped (`lcmap` or `rmap`), that side's carrier, and whether the
-- | wrapped side is a bare value, the canonical row, or the whole row.
-- |
-- | ```
-- |                          input ×      input +    output ×   output +
-- | -----------------------  -----------  ---------  ---------  ------------
-- | bare, closed singleton   atField      atCase     —          toCase
-- | bare, open row           —            —          —          recordToCase
-- | one field of a wider row forProperty  forCase    —          —
-- | whole row                —            forCases   —          toCases
-- | ```
-- |
-- | These readers carry **no label
-- | argument**: the leaf states the business label once, as its own type
-- | argument, and the adopter reads it back out of the closed singleton
-- | row via `RowToList`'s fundep. There is no formatter cell: a display
-- | whose content is copy takes its **read function** at the leaf
-- | (`text lineOf`), never an adopter bracket
-- | (doc/research-copy-is-a-function.md). Renames (`asField`-style) survive only
-- | where a packaged control fuses a canonical core to a surface label.
-- |
-- | The blanks are the **merge law** restated one layer down. A *shared* side
-- | may be touched partially; an *owned* side must be handled or produced
-- | whole — records share their input and own their output, variants own
-- | their input and share their output. So open-row adopters exist at
-- | record-input and variant-output and nowhere else: a partial variant read
-- | is not total, and a partial record build would have to invent the
-- | remaining fields, which only `field @l`'s retained background can
-- | supply over `Strong`.
-- |
-- | The output-`×` bare-value cells are deliberately empty: a record output
-- | is owned, so a bare value could become a field only as the whole row,
-- | and a label-indexed leaf already emits its labelled row (`field @l`
-- | lifts an editor into it) exactly as a label-indexed emitter is
-- | `recordToCase @l` — the closed-singleton field build had nothing left
-- | to do and was pruned (L14). `asField` is the fused both-side rename packaged controls
-- | want (a fixed core row renamed at the surface), and the deliberately
-- | absent `+ → +` fusion is `atCase @l # toCase @l' f`. The one entry
-- | outside the grid is `splitVariant`, a plain function rather than a
-- | placement (`required`'s dual `optional` needs the carrier and lives in
-- | `PUI`).
-- |
-- | The merge's two obligations are per-side and dual, and they are what the
-- | constraint vocabulary below spells out: on an **input** side, where does
-- | each label's value come from; on an **output** side, who is allowed to
-- | produce it. Records share their input (every operand may read every
-- | field) and own their output (each field has exactly one producer);
-- | variants own their input (each case has exactly one handler) and share
-- | their output (any operand may emit any case). Sharing is inclusive
-- | (`InclusiveRows`), ownership is exclusive (`ExclusiveRows`) — so a merge
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
-- |     deduped union: record inputs, variant outputs), `ExclusiveRows`
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
-- | ## The laws, stated once
-- |
-- | One principle and three laws; the four shape modules' law sets
-- | (`Data.Profunctor.Row.RecordToRecord` and its siblings, each header's
-- | "Laws of the shape") are these read at one shape — **nine lines each,
-- | on the nine axes of the grid below**, each line titled by its axis and
-- | subtitled by the shape's reading, plus **two laws stated only here**
-- | because they read the same at every shape — kept per shape because
-- | each line there is what a test or a starvation message names. Every
-- | cell of the grid has a probe in test/Main.purs, listed in each
-- | header's footer, and the merge laws are checked over every script to
-- | a bound in test/Exhaustive.purs — complete, since the gate is a
-- | data-independent machine with finite control (doc/observational-
-- | semantics.md §9).
-- |
-- | **The kinds.** A record is **knowledge**, a variant is an **event**.
-- | Knowledge is idempotent (twice is once), has a value between inputs,
-- | and is whole or nothing. An event counts (twice is two), has no value
-- | between occurrences, and is caused or nothing.
-- |
-- |   1. **Nothing fabricated.** An emission carries only what its citizen
-- |      has. A record output is a whole row from retained knowledge,
-- |      withheld until that knowledge exists. A variant output is an
-- |      event with a cause, and a feed is knowledge, not a cause — so a
-- |      feed never emits, and what an event carries is the whole row last
-- |      fed (replay).
-- |   2. **Nothing private.** What a record-output citizen would release,
-- |      it releases when it changes. At a feed the feed *is* the change,
-- |      so every feed is answered, once; at an occurrence, release when
-- |      the state changed. No fields, nothing owed.
-- |   3. **The merge.** Knowledge is shared on the way in and owned on the
-- |      way out; events are owned on the way in and shared on the way out.
-- |      Given operands obeying 1 and 2, the merge obeys them, takes each
-- |      field only from its owner, feeds neither operand with the other's
-- |      emissions, and is symmetric and associative up to `≈`. Its unit is
-- |      the shape's wire at the unit object, exact — except at `×→+`,
-- |      where no wire exists and the unit is `silence`.
-- |
-- | Read at the four shapes:
-- |
-- | ```
-- |              record out                                  variant out
-- | -----------  ------------------------------------------  ---------------------------------
-- | record in    must answer, once, whole — the gate,        must not emit — arming, replay
-- |              retention, the step, the pre-feed drop
-- | variant in   may release — dispatch in, gate out,        may respond — causality,
-- |              no torn row possible                        stateless dispatch
-- | ```
-- |
-- | The four law sets are the same **nine lines** — three on the citizen,
-- | six on the merge — each line one axis read at one shape, the axis its
-- | title and the reading its subtitle:
-- |
-- | ```
-- |  #  axis                    ×→×             ×→+             +→+             +→×
-- | --  ----------------------  --------------  --------------  --------------  --------------
-- |  1  repetition              twice is once   twice is once   twice is two    twice is two
-- |  2  emission                whole row       row last fed    a response      whole row
-- |  3  answer                  exactly once    never           any number      on change
-- |  4  unit                    wire at {}      silence         wire at 1_+     wire 1_+ → {}
-- |  5  input side              broadcast       broadcast       dispatch        dispatch
-- |  6  output side             gate            passage         passage         gate
-- |  7  exactness               enforced        free            free            enforced
-- |  8  closure                 step, drop      stateless       stateless       withhold, no tear
-- |  9  independence            looped          coresolve       iterate (raw)   unfolding
-- | ```
-- |
-- | Axes 1 and 5 read off the **input** kind, 2, 6 and 7 off the
-- | **output** kind — two readings each, dual across the anti-diagonal
-- | (`×→+` and `+→×` are each other's transposes line by line); 3, 4, 8
-- | and 9 read off both kinds and have four. The three citizen lines are
-- | the kinds and the modality table above: repetition *is* the kind of
-- | the input; emission is law 1 at the two output kinds; the answer line
-- | is law 2. On the merge, exactness is "owned out" needing runtime
-- | evidence, and free at "shared out", where a variant carries its one
-- | tag. Tearing is the one cell with shared-in and owned-out, and the
-- | step is law 2 applied to that merge.
-- |
-- | **Two laws read the same at every shape and are stated only here.**
-- | For `m = merge w1 w2` at any of the four:
-- |
-- |   * **Symmetry and associativity**, up to `≈`:
-- |     `merge w1 w2 ≈ merge w2 w1` and
-- |     `merge (merge w1 w2) w3 ≈ merge w1 (merge w2 w3)` — operand order
-- |     and nesting are not observable at the boundary.
-- |   * **Monotonicity.** `w1 ⊑ w1'` implies `m ⊑ m'`. Vacuous for an
-- |     operand owning no field — a display `w1 ⊑ w1'` gives `m ≈ m'`
-- |     outright — which is law 1 at the merge (a withholding owner cannot
-- |     be fabricated around) and law 2's zero-field clause, and why a
-- |     display never enters a gate and a status never withholds a fold.
-- |
-- | Both are probed at every shape in test/Main.purs (`×→× symmetry` and
-- | its three siblings, the four `… associativity` probes, `enrichment:
-- | p ⊑ p' ⇒ p ⊗ r ⊑ p' ⊗ r` and `enrichment at ×→+`/`+→+`/`+→×`) and
-- | checked over every script to a bound in test/Exhaustive.purs.
-- |
-- | Two facts stay outside, being about the carrier rather than the
-- | shape: the pre-feed
-- | emission is *dropped* rather than delayed (the named `Strong`
-- | deviation), and only the `+→+` loop's retraction holds raw (the trace
-- | asymmetry theorem) — doc/observational-semantics.md §4–5.
-- |
-- | Law 3's consequences, seen per shape (the four merges here and the
-- | container action in `Data.Profunctor.Acting` alike), all decided by the
-- | **output side**:
-- |
-- |   * **units are forced, not designed** — a shape's nullary merge is
-- |     `Category`'s `identity` at the unit object wherever a wire fits
-- |     (`1_× = {}`, `1_+ = Variant ()`; the record gates treat a
-- |     contribution of zero fields as no contribution, so the wire is the
-- |     unit exactly), and `silence` for the one shape no wire reaches,
-- |     `×→+` (terminal → initial). Pointing — emitting the canonical
-- |     value of the inhabited `1_×` — is `Seeding`'s `announce`, not a
-- |     unit's. Every starvation symptom is a sum-behaviour standing where
-- |     a product-behaviour was required.
-- |   * **gates are the cost of laxity over streams** — pairing two output
-- |     streams into one stream of pairs has one canonical implementation:
-- |     retain each side's last value, withhold until every side has spoken.
-- |     So every (·,×)-shape gates and retains (`recordToRecord`,
-- |     `variantToRecord`, `acted`'s gather) and no (·,+)-shape does
-- |     (injections need no pairing).
-- |   * **the gates' further price is bifunctoriality — at the inner
-- |     surfaces**: interchange `(f ⊗ g) >>> (h ⊗ k) = (f >>> h) ⊗ (g >>> k)`
-- |     fails on the nose when stage feeds are observed (the merged-first
-- |     side withholds `h` until every operand has spoken) and holds there
-- |     as refinement `⊑` in feed timing; at the **boundary**, for operands
-- |     honoring the component protocol, it holds **on the nose**. Counting
-- |     only channels the gated merge is monoidal; counting renderings it
-- |     is premonoidal — unit, associativity and symmetry hold outright
-- |     either way (doc/observational-semantics.md §4; both levels tested
-- |     in test/Main.purs).
-- |   * **a feed is one step** — on a stateful carrier a gated merge's
-- |     broadcast is batched and released **once**: a feed that changes
-- |     several fields emits one row with every field fresh, never a
-- |     `{ a: fresh, b: stale }` between two echoes; a user emission,
-- |     arriving outside any step, releases at once. It is what makes the
-- |     boundary interchange exact rather than up to stutter, and the merge
-- |     the exact product of Mealy machines (one input, one output pair).
-- |
-- | See doc/collections-profunctor-algebra.md §1.
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
-- **sharing is inclusive, responsibility is exclusive** — and runtime
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
-- **What an inclusive input side obliges.** A shared record input is a
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

-- | A merge's **record-input side**: everyone may read a field, so operand
-- | rows may overlap. The merge action is a label-blind broadcast — no
-- | runtime evidence needed.
class SharedRecordInputs :: Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class InclusiveRows i1 i2 i i12 i1x i2x <= SharedRecordInputs i1 i2 i i12 i1x i2x

instance InclusiveRows i1 i2 i i12 i1x i2x => SharedRecordInputs i1 i2 i i12 i1x i2x

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
              (Text "Look for the duplicated `asField`/`field`/`atCase` label in this `do` block.")))
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
              (Text "Look for the duplicated `asField`/`field`/`atCase` label in this `do` block.")))
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

widenRecordInput :: forall p narrow extra wider o.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p { | narrow } o -> p { | wider } o
widenRecordInput = lcmap unsafeCoerce

widenVariantOutput :: forall p i narrow extra wider.
  Profunctor p =>
  Row.Union narrow extra wider =>
  p i [ | narrow ] -> p i [ | wider ]
widenVariantOutput = rmap expand

-- | Dispatch a shot into the focused sub-variant or the background — a
-- | plain row function, no profunctor in sight, which is why it sits on the
-- | floor rather than in a shape module. `subChoice`, `iterate` and
-- | `subRetaining` all split with it.
splitVariant
  :: forall f b s
   . ExclusiveRows f b s
  => Contractable s f
  => Contractable s b
  => [ | s ]
  -> Either [ | f ] [ | b ]
splitVariant v = case contract v of
  Just f -> Left f
  Nothing -> case contract v of
    Just b -> Right b
    Nothing -> unsafeThrow "splitVariant: case in neither focus nor background"
