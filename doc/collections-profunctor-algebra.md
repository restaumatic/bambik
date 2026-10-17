# The collection problem in general profunctor algebra

*Design note. Rejects the bespoke-class route (a `Sequencing` shape, or a
family of them) an earlier note had taken. Instead it locates
what the library already does — merges, units, gates, strengths, co-strengths,
optics, traces — inside **standard algebra**: monoidal profunctors, Tambara
modules, container/polynomial actions, species, traced monoidal structure.
The collection then needs no new concept: it is a corollary of structures
already present. Carrier-general throughout; `PUI Web` appears only as "the
stateful carrier".*

## 0. The skeleton: two compositions, duoidally related

`PUI` composes two ways: **sequentially** (`Semigroupoid.do`, call it `⊳`)
and **in parallel** (the merges, call them `⊗`). Categories with both, and a
lax interchange `(f ⊗ g) ⊳ (h ⊗ k) → (f ⊳ h) ⊗ (g ⊳ k)`, are **duoidal**;
the pipeline/merge distinction is not bambik-specific bookkeeping but this
standard structure. The 2-cell the lax interchange needs is real and named:
the refinement order `⊑` of doc/observational-semantics.md — at the inner
surfaces (what the stages are fed, when) the interchange fails as an
equality and holds as `⊑` in exactly the direction written above; at the
boundary, for protocol-respecting operands, it holds on the nose — a feed is
one step, released once. Which category `PUI` is depends on what one counts
as an observation — both are tested.

One starvation bug becomes a theorem here. Broadcasting one input to two
`⊳`-composed stages requires the first stage to be a **comonoid**: it must
*duplicate* — pass its input through — not merely consume it. A display is
exactly that comonoid (render *and* release the row it shows,
`recordToRecordWiring`); a status has only the counit (render and *discard*).
That is the general reason a counit-only stage followed by a collection
starves under `Semigroupoid.do` while a display followed by one flows
(a counit-only display word, `muted`, went for it; CHANGELOG 2026-10-13):
chrome in a pipeline needs comultiplication, chrome in a merge needs
nothing (the merge broadcasts). Duoidal algebra names when `⊳` can emulate
`⊗` — only through comonoids.

At **equal types** no third combination is admitted. The ungated
juxtaposition — `ArrowPlus`'s `<+>` at the profunctor kind, input shared
by the diagonal, outputs interleaved on one channel — combines no
*contributions* (no union of fields, no dispatch of cases — none of the
(M, N)-monoids of §1), so last-writer-wins would be carrier scheduling,
not algebra, and every use is two whole-row citizens over one row, which
compose *sequentially* — each is a wire that also edits (it was carried as
`Joining`/`joint` and withdrawn; CHANGELOG 2026-08-27).

## 1. The four merges are the four (M, N)-monoidal structures

For monoidal structures `M, N ∈ {×, +}` on the value category, say a
profunctor is **(M, N)-monoidal** when it carries

```purescript
par  :: p a b -> p c d -> p (a `M` c) (b `N` d)
unit :: p 1_M 1_N
```

with unit, associativity and symmetry coherence (all tested in
test/Main.purs, on `PUI Effect` probes and — for the diagonal shapes — as
pure equalities on `(->)`). **Interchange is observation-level-relative**:
on the gated merges `(f ⊗ g) ⊳ (h ⊗ k) = (f ⊳ h) ⊗ (g ⊳ k)` fails on the
nose at the inner surfaces and holds there one-directionally as refinement
`⊑` — the lax duoidal interchange of §0 — while at the boundary, for
protocol-respecting operands, it holds on the nose, because a feed is one
step: the broadcast is batched and the gate releases once
(doc/observational-semantics.md §4; both tested in test/Main.purs). So a
gated merge is premonoidal counting renderings and monoidal counting
channels. All four shape classes are instances:

| shape | (M, N) | binary form on plain types |
|---|---|---|
| `recordToRecord` | (×, ×) | `p a b -> p c d -> p (a × c) (b × d)` |
| `recordToVariant` | (×, +) | `p a b -> p c d -> p (a × c) (b + d)` |
| `variantToRecord` | (+, ×) | `p a b -> p c d -> p (a + c) (b × d)` |
| `variantToVariant` | (+, +) | `p a b -> p c d -> p (a + c) (b + d)` |

**Rows are the label-strictified tensors.** `Record r` is the labeled n-ary
`×`; `Variant r` the labeled n-ary `+`. Labels do what associators and
symmetries do in the unbiased picture — `Union`/`Nub`/`DisjointLabels` are the
strictification bookkeeping, which is why the qualified-do merges need no
nesting parentheses and no reassociation lemmas. The row layer is not a fifth
concept beside the monoidal one; it is the same monoidal algebra presented
strictly.

**Units are forced, not designed.** At the diagonal shapes the unit is
`Category`'s `identity` at the unit object — `p {} {}` and
`p (Variant ()) (Variant ())` — exactly, because a contribution of zero
fields is no contribution and the record gates treat it so; `1_+ =
Variant ()` is *initial and uninhabited*, so that wire can neither receive
nor emit and is silence by parametricity. `1_× = {}` is *terminal and
inhabited*: it has a canonical global element, which is what **pointing**
emits — `announce a`, `PUI`'s point, a map out of `1_×` — but pointing is
not a unit's job, and the units carry no information. The one merge whose
unit no wire reaches is `×→+` (terminal → initial), and its unit is the
carrier's silent component — `PUI`'s private `silent`, `Cont`'s `mempty`
— named by no class, law-tested per carrier. Every starvation symptom in
the library's history is a use of a `+`-behaviour where a `×`-behaviour
was required.

**Gates are the cost of laxity over time.** In a pure synchronous carrier the
laxator `p a b ⊗ p c d → p (a×c) (b×d)` is trivial. `PUI`'s channels are
*event streams*; pairing two output streams into a stream of pairs has one
canonical implementation — retain each side's last value, emit on change,
withhold until both have spoken, and release **once per feed** (a broadcast
is one step, so the product is computed atomically per moment, never torn).
That is the knowledge gate, and it appears
wherever a product is assembled from several operands (`recordToRecord`;
`variantToRecord` assembles nothing — its operands release whole shared
rows, so it is a copairing) and in none of the (·, +) ones (injections
need no pairing). The gate is what the product of behaviors costs when
computed over streams — carrier machinery for a universal construction, not
an ad-hoc feature.

## 2. Strengths are Tambara modules; optics follow by Pastro–Street

A **Tambara module** for a monoidal action `⊙ : M × C → C` is a profunctor
with coherent strengths `p a b → p (m ⊙ a) (m ⊙ b)`. This is the established
general notion behind the library's strength classes:

- `Strong` = Tambara for the `×` self-action; `Choice` = Tambara for `+`.
  The Pastro–Street correspondence then *generates* the optics: lenses are
  the optics of the `×` action, prisms of the `+` action — `focusField`
  and `focusCase` are their row-strict forms.
- `Resolving`/`Retaining` are the library's genuine addition: Tambara-*like*
  structures for **mixed actions that only a stateful, temporal carrier
  supports** (`resolve` needs a notion of quiescence — time; `retain` needs
  memory — state) — "like" carrying real weight: they are
  **single-application mixed strengths**, with naturality and dinaturality
  but provably without the unit and composition coherences (the mixed
  channels compose as `c × d` on one side and `c + d` on the other — no
  single channel carries both; doc/observational-semantics.md §6). So their
  optics — `Shutter`, `Reel` — are *defined* by the profunctor encoding,
  with **sound existential constructors and no completeness claim**: the
  representation theorem does not carry over, and the gap is inhabited —
  `identity` lives in the ∀-form of `Shutter a b a b` while every
  existential shutter carries an escape `s → t` (`Data.Lens.Shutter`). The
  honest general statement: *a duplex, asynchronous, stateful profunctor
  supports more strengths than a pure one, but the mixed ones are
  operations, not actions* — and the library's mixed strengths chart that
  extra territory with their content living in the seeded retractions (§5).

## 3. The collection: the algebra is closed under containers

**Containers** (polynomial functors) `F a = Σ (s : S). a^(P s)` are generated
from constants, identity, `×`, `+`, and least fixpoints `μ`. Two members of
that grammar are already in the library's vocabulary:

- `Maybe a = 1 + a` — whose action the library carried (`optioned`) and
  withdrew as unreachable: no row holds a `Maybe`, an absent state being a
  variant case a pane gates on (CHANGELOG 2026-10-14);
- `Array a = μ x. 1 + a × x` — the collection.

The relevant general theorem (Jaskelioff–O'Connor; the profunctor-optics
literature): **a profunctor Tambara for both `×` and `+`, with enough
recursion, is Tambara for every finitary container** —

```purescript
acting :: p a b -> p (F a) (F b)
```

for polynomial `F`. At `F = Array` this is `Traversing`/`wander`. So the
collection is **not a fifth shape and needs no bespoke class**: it is the
statement that the existing shape algebra is *closed under `μ`*. `Strong`
and `Choice` — which `PUI` already has — generate it. Rows are the finitary,
`μ`-free, label-strict fragment of the same container grammar; `Array` is
what the grammar produces one `μ` later. One algebra, two fragments.

The unit and gate stories of §1 now *derive* for collections instead of being
re-legislated: `Array b` at the empty shape is the `1` summand of the `μ` —
inhabited, so the `(·, ×)` collection form announces `[]`; the collapsed
event form has no output at zero elements — sum-flavored, silent, ungated.
And the gather of `acting` at a `×`-output must retain every position's last
output — the §1 gate again: the traversal framing does not avoid retention,
it requires it, which is laxity-over-streams once more.

What the general theorem does **not** hand over is the *collapsed* event form
`p a o -> p (Array a) o` and its "which element fired" semantics: that is
temporal (a fact about *when*, like `Resolving`), so it exists only on
stateful carriers — the same boundary as §2, in the same place for the same
reason.

## 4. Keys: from polynomials to species; reconciliation is naturality

A static row indexes its positions by compile-time labels. The keyed
collection indexes them by **runtime keys**: its container is not the
anonymous `Σ n. a^n` but the **species-shaped**

```
F a = Σ (K ∈ Pfin(Key)). a^K
```

— shapes are finite key *sets*, positions are the keys themselves (Joyal's
analytic-functor form). This single move explains the reconciler:

- A new feed with key set `K′` meets the previous `K` through the span
  `K ↩ K ∩ K′ ↪ K′`: the middle leg is the **survivors** (re-fed in place),
  `K ∖ K′` the **leavers** (torn down), `K′ ∖ K` the **entrants** (built).
  Reconciliation is the functorial action along **partial injections of key
  sets** — the reconciler's `Map` is the presheaf's value, and
  **identity-follows-key is naturality**, not a DOM heuristic.
- Unique keys = shapes are *sets*, not lists — the runtime `DisjointLabels`,
  degraded from compile error to invariant (watchdog territory, like the
  starvation warning).
- The key is the species label. It lives in the container's shape
  (carrier-private reconciler state) or in the emission's tag (a `(Key, o)`
  channel — note a variant *is* a (tag, payload) pair with a static tag
  set, so `(Key, o)` is its runtime degradation) — never on the carrier's
  public surface: no `data-key` attribute, no dispatch performed on the
  DOM, since the keyed collection gives the label its lawful home.

## 5. The co-side: the trace quartet is traced monoidal structure

The co-strengths are not a bambik invention either — they are **traces**:

- `Costrong`/`unfirst :: p (a × c) (b × c) -> p a b` is the trace of the `×`
  structure; `Cochoice`/`unleft :: p (a + c) (b + c) -> p a b` the trace of
  `+` — Elgot/tail-recursive iteration, which is why `coprism` is literally
  `tailRec` at the optic level.
- The library's retraction laws are the yanking law of traced monoidal
  categories — **stated seeded**, because on this carrier the raw `×`-side
  composites are provably dead (each gate waits on the other; the deadlocks
  are tested): `unfirst (seeded (Tuple a0 c0) >>> first g) ≈ seeded a0 >>> g`,
  `coresolve (resolve g >>> seeded (Right c0)) ≈ debounced g`,
  `coretain (seeded (Right c0) >>> retain g) ≈ g` — while `unleft (left g) = g`
  holds raw. That asymmetry is the theorem: **`PUI` is genuinely traced over
  `+` and only pointed-traced over `×`** — coproduct iteration (Elgot) is
  free, product feedback (Conway/Hasegawa fixpoints) needs a starting point,
  and the seeds are the operational ⊥
  (doc/observational-semantics.md §5).
- By Hasegawa's correspondence (traces on cartesian structure ↔ Conway fixed
  points), `looped` *is* a fixpoint operator — the app's loop is the
  Conway fixpoint of its fold stage.
- Collection traces come free once §3 exists: trace over the container action
  = the homogeneous ensemble (the cells grid under `looped` — elements
  cross-feeding with per-key retention, which is the gate again); the keyed
  Mealy and generators are the `+`-trace over the species action. No new
  classes; the same traces over one more action.

## 6. What this replaces

Instead of `Sequencing` (or a family of sequence shapes), the general
inventory is:

1. **Duoidal skeleton** — `⊳` and the `(M, N)`-monoidal `⊗`s, with comonoids
   (the displays) mediating interchange; the units-are-forced and
   gates-are-laxity facts documented once, at this level.
2. **Tambara modules per action** — `Strong`, `Choice` (ecosystem);
   `Resolving`, `Retaining` (the stateful carrier's extra actions).
3. **One container-action class** in place of any collection shape:
   `actedBy :: Ord k => (a -> k) -> p a b -> p (Array a) (Array b)` for the
   keyed `Array` container, the species refinement for stateful carriers.
   Its `(->)` instance is `map`, so the laws (singleton retraction =
   yanking at the container, the empty release, the wire law) are
   value-testable. The collapsed event form and the keyed Mealy remain
   carrier-level, typed as such — the same boundary that already separates
   `Strong`/`Choice` from `Resolving`/`Retaining`.
4. **Traces** — the quartet as-is, plus their action-indexed lifts (ensemble,
   generators) which are theorems, not features.

The rejected route multiplied bespoke classes to cover the collection's
cases; this route covers them with three standard notions the library de
facto instantiates already (the bespoke `Sequencing` shape it replaced is
deleted). The library's real novelty stays visible and gets
sharper: it is not the shapes or the collection — it is being a **duplex,
asynchronous, stateful Tambara module**, where more actions (mixed,
temporal, keyed) exist than pure profunctor algebra provides, and where
laxity has a price (the gate) that pure treatments never see.

## Where it lives

- **`extras/row-profunctor/Data/Profunctor/Acting.purs`** holds the pure
  algebra: `class Acting`/`actedBy`, `instance Acting (->)`
  (`actedBy _ = map`), and the row forms `acted @l` and `actedAt @l @t`.
  The carrier machinery lives with the carriers, like the merge instances:
  `PUI` carries `class Hosting` (instantiate one element at runtime, detach
  a leaver, restack survivors), the shared keyed reconciler and the one
  generic `instance Hosting m node => Acting (PUI m)`; `PUI.Web` carries
  `instance Hosting Web (Array Node)` (DOM placement over the run of nodes
  an element appended, moved by `appendChild`, so identity follows the key).
  Laws in the module header: empty, singleton retraction, gather gate,
  identity-follows-key, wire — composition deliberately not preserved (two
  reconcilers double-gate), so whole element pipelines are lifted, never
  stages.
- **Five forms** divide the ground by input (everything at once → `×`;
  one entity at a time → `+`) and output. The `×`-members are keyed by a
  **materialized identity field** `@l` of the element row (rows carry their
  identity; `actedBy` keeps the function form as the minimal carrier
  obligation); the `+`-members take the `{ key, value }` envelope, identity
  being the structural tag that arrives in the input, so no key function.

  | Form | Shape | Behaviour |
  | --- | --- | --- |
  | `foreach @l` | `(a -> Array { \| r }) -> p { \| r } { \| r } -> p a a` | the list of displays: keyed, retaining reconciliation; the elements' releases stay inside and the fed row passes on, so the list is a stage |
  | `anyOf @l` | `(a -> Array { \| r }) -> p { \| r } [ \| v ] -> p a [ \| v ]` | the emitting list: the same reconciler, every element emission forwarded onto one channel, ungated, silent on empty (`anyOfAt @l @t` over a stored array) |
  | `acted @l` | `p { \| r } { \| r1 } -> p (Array { \| r }) (Array { \| r1 })` | the container action: the element typed at its whole row, key included, the carrier re-setting the key on every emission from the input row through the `Strong` state channel, so identity is unforgeable; output is the gather gate (`actedAt @l @t` over a stored array) |
  | `dispatched` | `(i -> { key :: k, value :: a }) -> p a b -> p i { key :: k, value :: b }` | `+→+`: an unknown key instantiates a new case, a known key re-feeds its instance, emissions leave tagged |
  | `accumulated` | `(i -> { key :: k, value :: a }) -> p a a -> p i (Array a)` | `+→×`, the keyed Mealy: grows per new key in first-appearance order, updates known slots, emits the whole array at once, input-primed |

  The `+`-members never detach or restack: absence of a key is no signal,
  so removal and ordering stay array-level concerns upstream.
- **The gather gate is the record gate.** `acted`'s gather runs the one
  pure machine `PUI.Gate.gateStep`, whose participants are runtime keys:
  the record merge enrols its owned field labels, the collection the fed
  keys, rekeyed per feed (`Rekeyed` — survivors keep their slot, entrants
  unknown, leavers forgotten). So the `×`-output collection is the
  `×`-output merge over a runtime row — `[]` is the zero-participant
  release, the empty law and L6's zero-field clause the same consequence
  of enrolling nothing, a feed one step at both (a reconcile whose elements
  echo gathers once, whole). `accumulated` is input-primed and never
  withholds, so it needs no gate; `anyOf` and `dispatched` are
  variant-output and gate nothing.
- **Tests and demos.** Value-level laws run in `test/Main.purs` on
  `PUI Effect` probes (empty announces `[]` and nothing at registration,
  singleton retraction, gather gate, identity-follows-key, `anyOf` silent
  on empty), `acted`'s conformance to the step, monotonicity, wire law,
  feed-idempotence and one release per feed over every script to a bound in
  test/Exhaustive.purs (doc/observational-semantics.md §9), and DOM
  identity in `scripts/smoke/tests/acting-laws.mjs`. Focused demos:
  potluck (`acted @"name"`, per-guest dish selectors, the table's state a
  classifier over the gathered rows), reorder (`acted @"id"`, a playlist
  whose DOM nodes move with their keys; `actedAt @"id" @"Setlist"` in its
  HTML twin), departures (`dispatched`) and scoreboard (`accumulated`),
  both fed by the `clock` source; cells' grid is `anyOfAt @"key" @"cells"`
  inside `anyOf @"rowKey"`.
- The two component laws of §1 are stated once in `Data.Profunctor.Row`'s
  header; the duoidal reading of §0 in `PUI`'s.

## Pointers

Tambara, *Distributors on a tensor category*; Pastro–Street, *Doubles for
monoidal categories* (optics ↔ Tambara modules); Clarke–Elkins–Gibbons–
Loregian–Milewski–Pillmore–Román, *Profunctor optics: a categorical update*;
Jaskelioff–O'Connor, *A representation theorem for second-order functionals*
(Traversing from Strong + Choice over containers); Abbott–Altenkirch–Ghani,
*Categories of containers*; Joyal, species / analytic functors;
Joyal–Street–Verity, *Traced monoidal categories*; Hasegawa, traces ↔ Conway
fixpoints; Aguiar–Mahajan, duoidal categories; Earnshaw–Hefford–Román, *The
produoidal algebra of process decomposition* (sequential ⊗ parallel
normalization for profunctors); Katis–Sabadini–Walters, *Bicategories of
processes* / *Feedback, trace and fixed-point semantics* (Mealy machines
with feedback requiring an initial state — the seeded `×`-trace's
denotational home, doc/observational-semantics.md §5).
