# The observational semantics: what the laws are stated against

*Normative companion to the algebra's module headers (2026-09-08, from the
category-theoretic audit). Every law in `PUI`, `Data.Profunctor.*` and
`Data.Profunctor.Row.*` that is not a plain equality of pure values is stated
against the definitions here. The headers cite this file rather than
restating it; test/Main.purs exercises each named law and each named
deviation.*

## 1. The protocol: construction, registration, streaming

A `PUI m i o` value denotes, **per instantiation**, one process with two
channels, reached in three phases:

0. **Construction** (`unwrap`, in the carrier's `m`): the process and its
   state come into being. Every `Ref` a stateful instance owns is allocated
   *here, in `m`* — which is why the stateful instances (`Strong`, `Choice`,
   the trace quartet, `Category`'s wire, the gated merges, `updated`,
   `every`, `action`…) require `MonadEffect m`: state is a construction-time
   effect and is typed as one, never smuggled in through laziness. The
   constraint is informative in the other direction too — `Profunctor`,
   `Semigroupoid`, `Cochoice`, the ungated merges keep `Functor`/`Apply`,
   which says *no state here*.
1. **Registration**: `fromUser` is called **exactly once**, wiring the
   emission channel. Registration-time emissions (announcements, seeds) are
   part of this phase — `compose` registers downstream first so they are
   heard.
2. **Streaming**: `toUser` feeds and channel emissions interleave, each a
   finite synchronous cascade. A feed into a gated merge is one **step**:
   the broadcast runs with the gate batching and the gate releases once
   afterwards (`steppedFeed`), so one feed is one moment — never a sequence
   of half-updated ones (§4, *one feed, one release*).

The single-registration rule is not a convention but part of what a morphism
*is*: the instances keep per-instantiation `Ref`s whose meaning assumes one
subscriber (`identity`'s wire, the merge gates' one-shot subscriptions).
A morphism is a single-use process; reuse is re-instantiation.

## 2. Equivalence and refinement

- **Observational equivalence `≈`**: two values are equivalent when, driven
  by the same script under the protocol (same registration, same interleaved
  feeds and inner-component firings), their **boundary channels** carry the
  same streams. Laws over *inner* feed streams (what a wrapped component was
  fed) are stated up to **consecutive duplication** — the quotient that
  feed-idempotence (§3) licenses.
- **Refinement `⊑`**: `p ⊑ q` when under every script `p`'s streams are
  withholding-subsequences of `q`'s — `p` emits what `q` emits, possibly
  withholding prefixes or interleavings that `q` releases. Gates produce
  refinement, never divergence: once every gate has been fed, the residuals
  agree.
- **Primed equivalence**: equality of the residual streams after every gate
  in both values has been fed. A gate **drops** what it withholds — a
  pre-priming difference is permanent, not a delay. This is deliberate UI
  semantics: an event that fired before any model existed refers to nothing
  and must not replay against a later model.
- **Stutter**: on a **record-shaped** channel a consecutive duplicate
  emission is no observation. A behavior is a value at every moment, so
  repeating the current value is the same function of time, and boundary
  streams on behavior channels are compared **up to stutter**. An
  **variant-shaped** channel carries occurrences; there every emission
  counts. (Record means behaviour and variant means event, for every
  component — §3.) This is the inner-feed quotient
  above seen from the other side, and the same fact as feed-idempotence
  (§3): to be a behavior *is* to be stutter-invariant. The carrier's own
  combinators no longer produce stutter — a merge's broadcast is one step,
  released once (§4) — so the quotient is carried by the component protocol
  alone, not needed to make the merges lawful.
- **Observation levels**. Two are natural, and laws are tagged by which
  they hold at: the **boundary** (the composite's own two channels) and the
  **inner surfaces** (what each stage was fed, and when — operationally, the
  renderings). Several deviations in §4 are level-relative: the gate is
  invisible at the boundary and visible on the screen.

Where a law needs one of these, its header says which; `=` with no
qualification is equality of boundary streams under every script.

## 3. The component protocol

Three laws every component owes the carrier. They are *load-bearing*: named
combinator laws below fail without them.

1. **Feed-idempotence** — `toUser x; toUser x ≈ toUser x`. Feeding the same
   value twice is observationally one feed. Needed by: `looped`'s
   idempotence (nesting `looped` duplicates re-feeds — tested), `debounced`
   (its `coresolve` re-feeds the last input on every emission), and
   `seeded a >>> seeded a ≈ seeded a`. The focus-guarded text fields and
   `settled`'s idempotence contract are both instances of this law.
2. **Record-echo totality** — a *record-shaped* citizen answers every
   feed with one emission of the **whole** fed row (displays release it,
   editors echo it with the background re-attached, `identity` is the echo
   wire — every record-shaped leaf). What keeps the gated merges live
   and what the seeded `×`-retraction laws quantify over. The law has two
   halves of different strength (2026-09-11, from an audit of every
   `toUser` in the six vocabularies): **row totality** — whatever is
   released is the whole fed row — holds on the nose for every published
   stage; **feed liveness** — every feed is answered, once — holds for
   every leaf and the instant rungs and is *refined*, not broken, at
   exactly two sorts of place: in **time** by the witness rung
   (`confirmed` releases on confirmation; a declined reading withholds) and
   the gather gate (`acted` releases once every element has spoken), and in
   **value** by the type-changing selectors, whose `Just`-only echo is
   silent on `Nothing` and is completed by `required`/`optional` before the
   leaf can be a stage — the type forbids a bare selector in any
   `Category.do` over one row. "Once" is exact since the same audit:
   `inCase` echoes only while its pane is detached (attached, the editor's
   own echo is the release) and `drawer` sequences its nav into its content
   instead of fanning one feed out to two echoing sides.
3. **No synchronous event echo** — a *variant-shaped* citizen (an occurrence
   source: `button`, `clicked`, `menuItem`, a status's event input) never
   emits from inside its own `toUser`. Events are occurrences, not
   responses; this is the termination argument for `Cochoice`'s re-entry
   (an event loop, not a busy loop) and for `iterate`, and what lets
   `updated`/`applied` *arm* an emitter by feeding it without firing it.
   Forwarding an occurrence that *arrived* — `identity`, `observed` and
   `subChoice`'s background cases at `+→+` all emit inside `toUser` — is a
   response to an input event, not an echo of a feed, and is not what this
   law forbids; the law is about sources (§3.1).

   The law is about **shape**, and every published component has the
   shape of its behaviour (2026-09-15). Two used not to. `bracketed`
   returned a variant *channel* that carried sum-typed *state* — its
   looped record ensemble echoed every feed, projected into a variant
   emission — and `clicked` returned a record channel that carried
   clicks. Both are fixed at the type: `bracketed @l` lifts its editor
   into the record field it edits, so a variant stands at channel position
   only as an event and inside a field only as a value; and every
   occurrence source — `clicked @l f`, `listOf @l f`, `onClickedXY @l`,
   the HTML floor's `button @l` — emits as case `l`, so no record-shaped
   source exists and `identity` and `clicked` no longer share a type. What
   remains a protocol obligation is narrower: the type cannot stop a
   `× → +` leaf from emitting inside its own `toUser`, so the proof burden
   is the finite set of occurrence primitives — the vocabulary provider's
   conformance, not a structural ambiguity in the algebra.

   **Why the carrier cannot take this one on.** Laws 2 and 3 are the two
   halves of a single obligation an *inclusive* record input carries — one
   feed reaches both operands, so a broadcast merge owes the boundary at
   most one thing back (§8.0). At a record output the carrier discharges it
   itself, by gating and releasing once (`steppedFeed`); at a variant output
   it cannot, because an event has no value between occurrences and so
   nothing can be retained or gated. That is the whole reason this law sits
   on components while its `×` sibling sits in the carrier — not a
   difference in how much each is trusted, but in what a pass-through can
   absorb.

**How the headers state these.** `Data.Profunctor.Row` ("The laws") states
the three as **two laws of a record input** — *Repetition* (law 1) and
*Answer* (laws 2 and 3 as one law read by output shape: a row at `×→×`,
nothing at `×→+`) — beside four merge laws (monoid, projection,
preservation, monotonicity); a variant input owes nothing, since dispatch
reaches one owner. The headers also make one thing precise that "once"
above leaves loose: at the boundary `≈` is up to stutter on a record
channel, so it cannot count. What `≈` states is that every feed is
answered and every emission of its step equals the step's last (no torn
row); "exactly once" is the renderings-level guarantee of the step (§4),
and the liveness half is a leaf law a stage may refine in time (`confirmed`,
the gather gate).

### 3.1 Echo by shape: must, must not, may, may

The three laws read off per direction give one **modality** each — what a
component of that shape owes on `toUser` (2026-09-11, from the vocabulary
audit; "echo" is an emission produced by a feed, "release" the same act
over retained state):

| shape | on input | modality | what stays obligatory | unit |
| --- | --- | --- | --- | --- |
| `×→×` | echo | **must** | every feed answered once, with the whole row (law 2) | `identity`, the echo wire |
| `×→+` | emit | **must not** | a feed never emits; the fed row leaves only as **replay** on an occurrence (`clicked`'s protocol, `armed`, `# with patch`) or at quiescence (`resolve`) (law 3) | `silence` |
| `+→+` | emit | **may** | a handler may forward, transform or end the case; it never *originates* — every emission is caused by an input occurrence, which is `iterate`'s well-foundedness | `identity`, the forward wire |
| `+→×` | release | **may** | an occurrence may or may not change the state; whatever *is* released is whole, and the retained state **is** the released state, so a change can never stay private | `lcmap case_ identity`, the never-fed wire |

The criterion behind the column: **the output shape decides whether anything
is owed, the input shape decides how it is discharged.** A record output must
be whole however its input arrived. With a feed on the input side the
background is the retained feed and the wire is echo (`field @l` re-attaches
it); with an occurrence there is no feed to re-attach, so the background is
the retained *contributions* and the wire is retention (`retain` withholds
until its state channel has a value, `variantToRecord` retains the other
side's last contribution, `accumulated`/`unfolding` take a seed). A variant
output cannot be re-said, so no echo can be mandated: from a feed it would be
a fabricated event, forbidden; from an occurrence it is a response,
permitted. The two off-diagonal "wires", replay and retention, are carrier
structure rather than `Category`'s — one needs time, the other a `Ref` —
which is §5's asymmetry seen from the components.

The two record-output rows differ in modality for one reason. At `×→×` the
input *is* the state, so an unanswered feed is state hidden from downstream
and the echo is owed. At `+→×` the input is an occurrence, and whether it
changes anything is the handler's decision; an occurrence that leaves the
state unchanged has nothing to say, and by law 1 saying it anyway is a
no-op. The carrier's policy of releasing on every occurrence once the row is
whole is therefore a **permitted choice** (made so that no row ever needs
`Eq`), not the law, and a carrier that released only on change would
satisfy the same laws. The degenerate case is every status, `[ event ] → {}`:
state that never changes, each occurrence rendered to the user, nothing to
the channel — a zero-field contribution is pre-satisfied and inert. The same
inertness means the `{}`-output displays' per-feed `{}` emission is never
awaited by any **gate**: the rung (`shown`) releases the row, not the leaf.
It is still the display's lawful answer under law 2 (the whole of a `{}`
output row is `{}`), and *sequencing* may depend on it — `simpleDialog`'s
confirm replays what its content last answered, so a bare `text` content
is what arms it (§3.2).
The modality is also a **detector**: a `×→×` display whose field no model
owns is a status in disguise. `action`'s progress slot was one until
2026-09-13 — `{ busy :: Boolean } → {}`, fed `true` then `false` around the
`Aff`, every `×→×` obligation vacuous (a `{}` echo of a row nobody awaited)
— and is now `[ started :: {}, ended :: {} ] → {}`, the run's two
occurrences dispatched to an indicator that owes nothing back; `blank`, the
faceless leaf, stands at that input as at any other, since `{}` is terminal.

One fact sits outside the table. `action` is the one `+→+` form that can be
*misplaced*: it responds through an `AVar` inside `launchAff_`, so an `Aff`
that completes without suspending emits within the feed — a response under
`# atCase`, where every demo puts it, but an echo if an application placed
it after an editor in a `Category.do`; the guard is writing.md's rule that
effects run on occurrences, not the type.

### 3.2 What a `×→×` gate waits for

A `×→×` gate is the set of **owned** output fields not yet known. A feed
shrinks it as one step and releases once when it is empty; a display never
enters it; an emitter or status never sees it. Stated for the carrier as it
stands (2026-09-14, after `action`'s progress slot left the `×` side):

- **Two granularities, one principle.** `recordToRecord` gates on owned
  fields across its operands: each side's last contribution is retained and
  the union released only when both are known. `field @l` gates the
  background around one field: `Strong.first` withholds until the pair
  state has been fed once, then re-attaches what it retained. A leaf is a
  whole-row citizen because the lens supplies the background and the merge
  supplies the sibling's fields — the same rule at two granularities.
- **A feed is one step.** The inclusive input side makes a feed a
  broadcast; it runs with the gate batching and the gate releases once
  afterwards, if anything arrived (`steppedFeed`). A user emission arrives
  outside any step and releases at once with the sibling's retained
  contribution. Each contribution is trimmed to its declared row before the
  union, so a stale runtime copy on an echo wire never shadows the sibling
  (runtime-exactness).
- **A zero-field side is born satisfied.** Its slot is primed with `{}` at
  construction and its emissions neither open nor re-fire the gate.
  `identity @{}`, `blank`, an announcing static and a `{}`-output display
  are indistinguishable as operands (the zero-field law, test/Main.purs).
- **No cross-feed inside the merge.** An operand's emission goes
  downstream, not to its sibling. Freshness across siblings is the
  enclosing loop's job: `looped` re-broadcasts every emission and each lens
  re-retains its background within the turn.
- **A display never enters the gate.** It owns no field. Its `{}` per feed
  is its answer under law 2, inert to every gate and consumed only by
  sequencing (§3.1); as a *stage* a display is integrated by the rung,
  `recordToRecord content identity`, and it is the rung that releases the
  row. An emitter (`×→+`) never sees a `×→×` gate — its fed row leaves only
  as replay — and neither does a status (`+→×`), which is dispatched.
- **Starvation is a priming failure of an owned field**, always: an editor
  or source never fed, never seeded, or sitting after a stage that never
  released. The cure is `with`/`mvu`/`seeded` or a trace form's seed
  argument — never an echo added to a display, which no gate would hear.
  The watchdog names the missing fields for exactly this reason.
- **The named cost.** Every `×`-side gate drops, not delays, a pre-feed
  emission, so the ecosystem `Strong` law holds only as primed equivalence
  (§4). The `+` side has no such gate — §5's asymmetry seen from the merge.

### 3.3 What the laws guarantee

Read as a contract: the provider owes §3's protocol at every leaf (the
headers' *Repetition* and *Answer*), the carrier owes the four merge laws
(monoid, projection, preservation, monotonicity — `Data.Profunctor.Row`,
"The laws"), and the application gets, for free and at every depth:
a merge is again a lawful component; operand order and nesting are not
boundary-observable; an operand is fed exactly its projection of the input
and never a sibling's emission; a multi-field feed is released once and
whole; a feed never fires a source, so re-broadcast loops settle; a
quieter stage substituted anywhere only withholds; and a silent gate is
either an operand breaking *Answer* (after its owned fields were fed) or
an unprimed owned field (before), which the watchdog names. What stays
outside: leaf conformance itself, the payload contracts of `clicked` and
`field`, rendering counts (the boundary laws hold up to stutter; the step's
one release per feed is this carrier's, §4), and a variant input's response
policy. The application-side statement is writing.md *What the laws
guarantee*.

## 4. Named deviations from ecosystem laws

- **`Strong`**: `lmap fst = rmap fst <<< first` holds only as primed
  equivalence — the gate drops a pre-feed emission (tested as the "Strong
  deviation" pair in test/Main.purs). Same for `second`, `Costrong`,
  `Resolving`, `Retaining`, `Coresolving`: every knowledge gate trades the
  unconditional ecosystem law for the primed one. `Choice` and `Cochoice`
  (the `+` channel) deviate nowhere.
- **Interchange is observation-level-relative.** For the gated merges,
  `(f ⊗ g) >>> (h ⊗ k) = (f >>> h) ⊗ (g >>> k)`:
  - at the **inner surfaces** it fails on the nose — the merged-first side
    synchronizes at the middle gate, so `h` sees nothing until *every*
    operand of the first merge has spoken — and holds one-directionally as
    refinement in feed timing: at every moment the merged-first side's stage
    feeds are a prefix of the free side's
    (`(f ⊗ g) >>> (h ⊗ k) ⊑ (f >>> h) ⊗ (g >>> k)`). That is the lax duoidal
    interchange of doc/collections-profunctor-algebra.md §0, meaningful
    exactly because `⊑` exists;
  - at the **boundary**, for operands honoring the component protocol
    (echo-total, feed-idempotent), it **holds on the nose**: a feed is one
    step — the broadcast runs batched and the gate releases once
    (`steppedFeed`) — so neither side stutters and the streams are equal
    (tested with the exact common stream in test/Main.purs). Before the
    step both sides stuttered: the free side emitted the torn
    `{ c: 20, d: 101 }` between `h`'s and `k`'s echoes, the merged-first side
    once more per middle-gate release — the stutter quotient was carrying
    what the step now guarantees.

  So the answer to "is the gated merge monoidal?" is "which category?":
  counting only channels, it is **monoidal**; counting renderings, it is
  premonoidal — unit, associativity and symmetry hold outright (tested, all
  four merges), interchange is bought back only as `⊑`. Symmetry itself is a
  boundary-stream fact — operand order stays observable in feed order to
  the operands, inside the `≈` quotient.
- **One feed, one release** — the merge law the step states: a broadcast
  that changes several fields emits **exactly once**, every field fresh; a
  user emission arrives outside any step and releases at once; nested
  merges release once, the inner step's release landing in the outer step
  (all tested). Denotationally this makes the gated merge the *exact*
  product of Mealy machines — one input, one output pair — where before it
  was that product up to stutter.
- **Enrichment.** `⊑` is a genuine 2-cell because composition and the merges
  are **monotone** in it: `p ⊑ p′ ⇒ p >>> q ⊑ p′ >>> q` and
  `p ⊑ p′ ⇒ p ⊗ r ⊑ p′ ⊗ r` (tested, with the gated/ungated pairs as
  `p ⊑ p′`). Without monotonicity the order would be a remark; with it,
  `PUI` is a poset-enriched category and every "lax" above is a real
  inequality in it.
- **The container action is lax the same way.** `actedBy k p >>> actedBy k q`
  re-feeds every `q`-element whenever any `p`-element emits (the whole
  array passes through the second reconciler), where `actedBy k (p >>> q)`
  feeds only the element that spoke — an inner-surface difference; the
  boundary agrees (tested). Every laxity in the library points the same way:
  more synchronization, more re-feeding, same boundary.

## 5. The trace asymmetry theorem

`PUI` is **genuinely traced over `+` and only pointed-traced over `×`**:

| trace | raw retraction | status |
| --- | --- | --- |
| `Cochoice` | `unleft (left g) = g` | holds raw — no seed |
| `Costrong` | `unfirst (first g)` | **dead**: each gate waits on the other |
| `Coresolving` | `coresolve (resolve g)` | **input-dead** |
| `Coretaining` | `coretain (retain g)` | **output-dead** |

The three `×`-side composites cannot be primed from outside — the deadlocks
are tested. The laws therefore hold in **seeded** form (state must enter
somewhere, and the seed is where):

```
unfirst   (seeded (Tuple a0 c0) >>> first g)  ≈  seeded a0 >>> g   -- g echo-total
coresolve (resolve g >>> seeded (Right c0))   ≈  debounced g
coretain  (seeded (Right c0) >>> retain g)    ≈  g
unleft    (left g)                            =  g                 -- raw
```

Each is what the corresponding row form builds (`feedback`, `folding`,
`unfolding`; `iterate` needs no seed — events occur, they don't pre-exist).
This is the operational face of a classical fact: coproduct iteration
(Elgot) is free, while product feedback (Conway/Hasegawa fixpoints) needs a
starting point — **the seeds are the operational ⊥**. It is also why
`Looping` is a class of its own: the gated `unfirst` cannot self-feed, so
the diagonal self-trace is carrier structure, not a `Costrong` derivation.

**The denotational home.** A `PUI` component is a Mealy machine with an
asynchronous emission channel — state, a feed transition, an output stream —
and the seeded `×`-trace is exactly the **feedback** of Katis–Sabadini–
Walters' bicategory of processes, whose feedback operator *requires an
initial state*: bambik's seed is that state, so the pointed-trace theorem
above is KSW's `Circ` construction rediscovered from the UI side. In that
model `≈` is **bisimilarity** and the gate is the product of two partial
behaviors, defined from the later of their start times — a feature becomes
the operational shadow of an isomorphism. Today every law here is checked as
a script against probes (§9); stating them as bisimulation proofs in the
KSW model, with its up-to techniques, is the open item that would turn the
test suite into a proof.

## 6. What the coined strengths are, exactly

`Resolving`/`Retaining` are **single-application mixed strengths**, not
Tambara modules: naturality in `a, b` and dinaturality in `c` hold (free by
parametricity); the unit coherence fails deliberately (at `c := 1` it would
erase the loop — time is essential), and multiplicativity fails *in
principle* (input residuals compose as `c × d`, output residuals as
`c + d`; no single channel carries both). Consequences:

- The classes alone are property-light — degenerate instances exist
  (`Cont`'s always-`Done` `resolve`); their equational content is the
  seeded retraction with their co-strengths (§5).
- Pastro–Street does not apply to the four coined optics: `shutterE` &c.
  are **sound constructors**, completeness is not claimed, and the gap is
  real — `identity` inhabits `Shutter a b a b` while every existential
  shutter carries an escape `s → t` (`Data.Lens.Shutter`). The ∃-to-∃
  dualities (`Coshutter s t a b ≅ Reel b a t s`, pair swaps) do hold.

## 7. Sums into products: `bracketed`'s law

The sum-typed field editor `bracketed @l stateOf caseOf` embeds a sum into
the product of its summands, projects back, and lifts the result into field
`l`. Its arguments owe one law, stated in
`Data.Profunctor.Row.RecordToRecord` and tested on the demos' pair:

```
caseOf (stateOf v) = v
```

— a section–retraction. The embedding `Σᵢ Aᵢ → Πᵢ Aᵢ` is not canonical in
general; it exists here because every summand is **pointed** (a default
payload for each absent case — the "seeding absent payloads" of the doc is
exactly that pointing, the same ⊥ as §5's seeds). The other composite,
`stateOf ∘ caseOf`, is deliberately *not* the identity: its kernel — the
other cases' payloads — is what the editor retains across a selection change.

## 8. Glitch-freedom is a style theorem

### 8.0 One cause: an inclusive input side

§3's no-synchronous-event-echo law and §4's one-feed-one-release are not two
facts about two diagonals. They are **one obligation, read off at two output
shapes**, and their shared cause sits on the *input* side of the merge.

A shared record input (`SharedRecordInputs`) is **inclusive**: one feed
reaches both operands, so both may answer it. A merge that broadcasts owes
the boundary at most one thing back per feed:

> **one feed in, at most one thing out — and if it is a record, a whole one.**

What discharges that depends on what the output can carry:

| output | what "one thing" needs | mechanism | where it lands |
| --- | --- | --- | --- |
| `×` | one emission, **whole** | gate + batch the broadcast, release once | the **carrier** (`steppedFeed`) |
| `+` | one emission | nothing to gate — an event has no value between occurrences | the **operands** (no synchronous event echo) |

The asymmetry in *where the law lands* is forced by the same fact. A record
output can be retained, so the carrier can hold contributions and release a
complete row itself. A variant output cannot be retained — a retained click
is a click that already happened — so the merge is a bare pass-through with
no way to absorb a second emission, and the obligation falls on the operands
as a protocol law the type cannot enforce.

**The prediction, and it holds.** Exactly the two merges with inclusive
record input carry a feed law; the two with `OwnedVariantInputs` carry
neither. That is a consequence, not a coincidence: `DisjointLabels` gives
every case exactly one handler, so a variant-input feed is *dispatched* —
exactly one operand answers — and there is no "one feed, several answers"
situation to discipline. Nothing to tear, nothing to duplicate.

|  | input | broadcast? | feed law |
| --- | --- | --- | --- |
| `×→×` | `SharedRecordInputs` | yes | one feed, one release (`steppedFeed`) |
| `×→+` | `SharedRecordInputs` | yes | no synchronous event echo |
| `+→×` | `OwnedVariantInputs` | no — dispatched | none |
| `+→+` | `OwnedVariantInputs` | no — dispatched | none |

The two axes are independent, and `+→×` is the case that separates them:
**input** inclusivity says whether the obligation exists, **output** shape
says what discharges it. `variantToRecord` dispatches its input (so it has
no torn-row hazard — nothing coalesces, at most one operand answers a case)
yet still gates and retains its output, because a record must be whole
however its input arrived. Its `steppedFeed` is therefore not discharging a
broadcast obligation; it is kept for re-entrant coalescing. `variantToVariant`
has neither side's obligation, which is the structural reason it is the one
merge needing no gate, no step, and no `MonadEffect`.

### 8.1 Two sources of tearing

Tearing has two sources, and they are settled by two different authorities.

**Within a merge** the carrier now guarantees freedom: a broadcast that
changes several fields releases once (§4, *one feed, one release*), so no
`{ a: fresh, b: stale }` ever leaves the gate. The first version of this
section argued that away from the independence of operands — a "stale"
sibling being the current value of an unedited field — and that argument is
sound only for *user edits*, where one operand changes at a time. A feed
that changes two fields *did* tear under the per-operand release, and every
stage between the merge and the loop end saw the phantom row (a `settled`
invariant running against a state that never existed). The step closes it.
Honest scope, and it is narrower than it first looks. No shipped demo ever
built such a merge — every demo ensemble *sequences* its whole-row editors
with `Category.do`, where each stage echoes once and nothing tears. That is
not merely idiom: **the merge type forbids the parallel-editor shape
outright**. `OwnedRecordOutputs` demands the two operands own *disjoint*
label sets, while `field @l` makes every editor a whole-row citizen
`p { l | rest } { l | rest }` — input and output the same row `r`, the
background retained and re-attached per emission. Two whole-row citizens
therefore both claim the whole row, and no annotation satisfies the
disjointness: `RecordToRecord.do` over two editors is a type error
(`No type class instance … Prim.Row.Union`), not a lurking runtime hazard.

So a torn row needs operands owning disjoint rows *and* a feed that changes
several of those fields at once — the shape of the value-level probes here,
reachable by a library-level or vocabulary-level assembly (a packaged
control merging two disjoint sub-displays it also feeds), not by an
application merging its editors. The law is pinned at the value level for
that reason, and **not** by the demo suite: a demo exhibiting it would have
to be written against the algebra rather than against the writing contract,
which is precisely the thing guardrails L15 keeps the demos from becoming.

**Across merges** — a *diamond*, one upstream field feeding two transforming
stages whose outputs re-merge downstream — the algebra still permits torn
intermediates, and **idiomatic bambik never builds them**: copy-is-a-function
and the one-`settled`-normalization rule (writing.md) put all derivation in
a single stage, so every merge's operands are independent sources. There,
glitch-freedom is guaranteed by the writing contract, not by the carrier — a
diamond an application builds anyway is a style violation before it is a
runtime surprise.

## 9. The gate as a Mealy machine, and why a bounded check is complete

The output gate the two record-output merges run on is one **pure step**,
`PUI.Gate.gateStep :: GateConfig -> GateState -> GateInput -> Tuple GateState
GateOutput`, in a module that imports no `Effect`. The effectful part of the
gate is a single function, `driveGate` in `PUI`: read the state, step, write
the new state, act on the output (a `Released` row goes downstream and marks
the starvation guards fed; a `Withheld` contribution arms the guard of the
side that spoke and is traced; `Quiet` is nothing). `recordToRecord` and
`variantToRecord` differ only in what drives the step — a broadcast
bracketed by `StepBegun`/`StepEnded`, or one dispatched operand between the
same brackets. The two variant-output merges have no state at all: their
`toUser`/`fromUser` are pure routing (`contract` and sequencing) and hold
nothing between events.

**Data independence with finite control.** No branch of `gateStep` inspects
a payload: the two retained contributions are stored and re-emitted, never
compared or read. The control is whether each side has spoken (born
satisfied when it owns no field), the step depth, and whether a
contribution landed during the step. Outside a step, the reachable control
states are one per subset of sides that has spoken.

**Why a bounded exhaustive check is a proof.** Two deterministic machines
driven in lockstep by one script differ, if at all, on a script no longer
than the number of reachable *joint* control states: a shortest
distinguishing script never revisits a joint state, or the loop could be
cut. Driven with a **fresh token per event**, a data-independent machine's
output values are stored tokens re-emitted, so whether two outputs agree is
decided by control and by which event's token each slot holds — and every
rig compared stores the latest token of each operand, so with agreeing
control the slots agree. The joint control states are therefore one per
subset of operands that has spoken: **four** for a two-operand law,
**eight** for a three-operand one. Scripts of length six and eight exhaust
them with margin.

**What is checked** (test/Exhaustive.purs, run from `spago test`, about 35
seconds): at each of the four shapes, symmetry, associativity, both unit
laws and `⊑`-monotonicity (a `quieter` operand, minus its first emission);
at the two record-output shapes, exactness against an operand whose every
emission carries a stale runtime copy of the sibling's field, and
**conformance** — the effectful merge's boundary stream equals the pure
`gateStep` driven by the same script; at `×→+`, arming (replacing every feed
by a no-op leaves the stream unchanged); at `×→×`, feed-idempotence of the
merge (deleting a `FeedAgain` directly after a feed leaves the stream
unchanged up to stutter) and the answer law (every feed of the merge is
answered by exactly one release — at least one, none torn); at every
shape, projection's input half (each operand's inner feed stream is
exactly its projection of the boundary feeds — the whole stream at a
record input, its own cases at a variant one). Thirty-one laws, about
three hundred thousand scripts, no distinguishing script. Because the pure step is the very
function the carrier runs, and conformance pins the wrapper to it, a law
that holds on the step holds on the merge.

**What stays outside.** Scripts drive the boundary and the operands; they do
not contain re-entrant feeds during a release, so the nested-step path
(`depth > 1`) is exercised only by the `looped` probes. Registration
ordering, that the widening coercion only forgets and never fabricates,
`Effect`'s sequencing, and the timed instance `resolve` are checked by the
named probes of §10 and by inspection, not by enumeration. The pure module
is also the port target for a mechanised proof: its four constructors and
one step function transcribe to Agda or Lean unchanged, and theorems there
are about the function the carrier runs.

## 10. Where the tests live

test/Main.purs, in order: the merge unit/zero-field/exactness/gating laws; the
trace quartet and its row forms; the Category laws; the container-action
laws; then the audit section — seeded retractions and the three raw
deadlocks, the `Looping` triple (yanking, conjugation with the `dimap f f`
counterexample, idempotence with the feed-duplication quotient made
visible), the Strong deviation, the interchange deviation at the inner
surface and its refinement direction, merge symmetry (`×→×`, `+→×`) and
mixed-merge associativity (`+→×`, `×→+`), the container action's wire law,
and the `(->)` diagonal-merge instances' laws as pure equalities; then the
observation-level section — boundary interchange on the nose with the exact
common stream, one-feed-one-release (a two-field broadcast releasing once
with no torn row, a user emission releasing at once, nested merges
releasing once, the disjoint-operand shape it is reachable from, and §8.0's
prediction — `+→×` dispatch carrying every ingredient of the torn row except
a broadcast, and provably not tearing: each operand sees only its own cases,
a re-fed case releases once beside its retained sibling), `⊑`-monotonicity of `>>>` and `⊗` (the merge at all four shapes), the
shape laws' remaining cells (Data.Profunctor.Row, "The laws": the component
laws at each shape on its own wire, replay source or merge —
`repetition`/`emission`/`answer ×→×`, `×→+`, `+→+`, `+→×`, the `emission`
probes pinning the leaf contracts (replay, nothing at registration) — the
`×→+` and `+→+` symmetries, the identity `exact` at `×→+`, and
`projection` at all four with `looped` as the cross-feed contrast), the
bounded exhaustive check of §9 (test/Exhaustive.purs), the
container action's laxity at the inner surface, `bracketed`'s retraction on the
order-form pair, the Ocular admission law for a node-wrapping ocular and its
failure for a capturing decorator, and `announce`'s naturality.
