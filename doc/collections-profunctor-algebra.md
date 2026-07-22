# The collection problem in general profunctor algebra

*Design note. Rejects the bespoke-class route (a `Sequencing` direction, or a
family of them) taken by `collections-sequence-merge.md`. Instead it locates
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
standard structure.

One session bug becomes a theorem here. Broadcasting one input to two
`⊳`-composed stages requires the first stage to be a **comonoid**: it must
*duplicate* — pass its input through — not merely consume it. `displayed`
equips a stage with exactly that comultiplication (render *and* forward);
`muted` has only the counit (render and *discard*). That is the general
reason `muted`-then-collection starved under `Semigroupoid.do` while
`displayed`-then-collection worked: chrome in a pipeline needs
comultiplication, chrome in a merge needs nothing (the merge broadcasts).
Duoidal algebra names when `⊳` can emulate `⊗` — only through comonoids.

## 1. The four merges are the four (M, N)-monoidal structures

For monoidal structures `M, N ∈ {×, +}` on the value category, say a
profunctor is **(M, N)-monoidal** when it carries

```purescript
par  :: p a b -> p c d -> p (a `M` c) (b `N` d)
unit :: p 1_M 1_N
```

with the evident coherence. All four direction classes are instances:

| direction | (M, N) | binary form on plain types |
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

**Units are forced, not designed.** `1_× = {}` is *terminal and inhabited*:
the unit `p {} {}` has a canonical global element to emit, so the lawful unit
**announces** — `pempty = announce {}` is the only coherent choice, not a
design decision. `1_+ = Variant ()` is *initial and uninhabited*: there is no
value to emit, so the unit is **silence** — by parametricity, not by policy.
Every starvation symptom in the library's history is a use of a `+`-output
unit where a `×`-output unit was required.

**Gates are the cost of laxity over time.** In a pure synchronous carrier the
laxator `p a b ⊗ p c d → p (a×c) (b×d)` is trivial. `PUI`'s channels are
*event streams*; pairing two output streams into a stream of pairs has one
canonical implementation — retain each side's last value, emit on change,
withhold until both have spoken. That is the knowledge gate, and it appears
in **every** (·, ×)-monoidal structure (`recordToRecord`,
`variantToRecord`'s retention) and in none of the (·, +) ones (injections
need no pairing). The gate is what the product of behaviors costs when
computed over streams — carrier machinery for a universal construction, not
an ad-hoc feature.

## 2. Strengths are Tambara modules; optics follow by Pastro–Street

A **Tambara module** for a monoidal action `⊙ : M × C → C` is a profunctor
with coherent strengths `p a b → p (m ⊙ a) (m ⊙ b)`. This is the established
general notion behind the library's strength classes:

- `Strong` = Tambara for the `×` self-action; `Choice` = Tambara for `+`.
  The Pastro–Street correspondence then *generates* the optics: lenses are
  the optics of the `×` action, prisms of the `+` action — `field`,
  `focusRecord`, `case_` are their row-strict forms.
- `Resolving`/`Retaining` are the library's genuine addition: Tambara-like
  structures for **mixed actions that only a stateful, temporal carrier
  supports** (`resolve` needs a notion of quiescence — time; `retain` needs
  memory — state). Their optics — `Shutter`, `Reel` — arise by the same
  correspondence. The honest general statement: *a duplex, asynchronous,
  stateful profunctor is a Tambara module for more actions than a pure one*,
  and the library's mixed strengths chart that extra territory.

## 3. The collection: the algebra is closed under containers

**Containers** (polynomial functors) `F a = Σ (s : S). a^(P s)` are generated
from constants, identity, `×`, `+`, and least fixpoints `μ`. Two members of
that grammar are already in the library's vocabulary:

- `Maybe a = 1 + a` — and `provided :: p a b -> p (Maybe a) b` is (up to the
  output collapse) the action of that container;
- `Array a = μ x. 1 + a × x` — the collection.

The relevant general theorem (Jaskelioff–O'Connor; the profunctor-optics
literature): **a profunctor Tambara for both `×` and `+`, with enough
recursion, is Tambara for every finitary container** —

```purescript
acting :: p a b -> p (F a) (F b)
```

for polynomial `F`. At `F = Array` this is `Traversing`/`wander`. So the
collection is **not a fifth direction and needs no bespoke class**: it is the
statement that the existing direction algebra is *closed under `μ`*. `Strong`
and `Choice` — which `PUI` already has — generate it. Rows are the finitary,
`μ`-free, label-strict fragment of the same container grammar; `Array` is
what the grammar produces one `μ` later. One algebra, two fragments.

The unit and gate stories of §1 now *derive* for collections instead of being
re-legislated: `Array b` at the empty shape is the `1` summand of the `μ` —
inhabited, so the `(·, ×)` collection form announces `[]`; the collapsed
event form has no output at zero elements — sum-flavored, silent, ungated.
And the gather of `acting` at a `×`-output must retain every position's last
output — the §1 gate, resurfacing exactly where the branch-B PoC found it
("the traversal framing does not avoid retention; it requires it"). What that
PoC observed empirically is just laxity-over-streams again.

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
- The `data-key` epilogue, generally: the key is the species label. It lives
  in the container's shape (carrier-private reconciler state) or in the
  emission's tag (a `(Key, o)` channel — note a variant *is* a (tag, payload)
  pair with a static tag set, so `(Key, o)` is its runtime degradation) —
  never on the carrier's public surface. The delegated `closest("[data-key]")`
  was case dispatch performed by the carrier because the algebra hadn't been
  given the label; the keyed `foreach` gave it a lawful home.

## 5. The co-side: the trace quartet is traced monoidal structure

The co-strengths are not a bambik invention either — they are **traces**:

- `Costrong`/`unfirst :: p (a × c) (b × c) -> p a b` is the trace of the `×`
  structure; `Cochoice`/`unleft :: p (a + c) (b + c) -> p a b` the trace of
  `+` — Elgot/tail-recursive iteration, which is why `coprism` is literally
  `tailRec` at the optic level.
- The library's law "each co-strength is its strength's retraction,
  `co (strength g) ≅ g` once the state channel is primed" **is the yanking
  law** of traced monoidal categories — with priming marking that a duplex
  stateful carrier is traced only on the primed part (feedback needs a first
  token).
- By Hasegawa's correspondence (traces on cartesian structure ↔ Conway fixed
  points), `looped`/`mvu` *is* a fixpoint operator — the model-view-update
  loop is the Conway fixpoint of the update stage.
- Collection traces come free once §3 exists: trace over the container action
  = the homogeneous ensemble (the cells grid under `mvu` — elements
  cross-feeding with per-key retention, which is the gate again); the keyed
  Mealy and generators are the `+`-trace over the species action. No new
  classes; the same traces over one more action.

## 6. What this replaces

Instead of `Sequencing` (or a family of sequence directions), the general
inventory is:

1. **Duoidal skeleton** — `⊳` and the `(M, N)`-monoidal `⊗`s, with comonoids
   (`displayed`) mediating interchange. Already implemented; rename nothing,
   but *document* the units-are-forced and gates-are-laxity facts once, at
   this level.
2. **Tambara modules per action** — `Strong`, `Choice` (ecosystem);
   `Resolving`, `Retaining` (the stateful carrier's extra actions). Already
   implemented.
3. **One container-action class** in place of any collection direction:
   `acting :: p a b -> p (F a) (F b)` for finitary containers, with the
   species/keyed refinement for stateful carriers. Its `(->)` instance is
   `map` — the laws (singleton retraction = yanking at the container,
   `[]`-announcement, fusion) become value-testable in `test/Main.purs`;
   `provided` (Maybe) and `foreach` (keyed Array) become two instances of one
   thing. The collapsed event form and the keyed Mealy remain carrier-level,
   typed as such — the same boundary that already separates
   `Strong`/`Choice` from `Resolving`/`Retaining`.
4. **Traces** — the quartet as-is, plus their action-indexed lifts (ensemble,
   generators) which are theorems, not features.

The rejected route multiplied bespoke classes to cover the collection's
cases; this route *deletes* the bespoke class it had and covers the cases
with three standard notions the library de facto instantiates already. The
library's real novelty stays visible and gets sharper: it is not the
directions or the collection — it is being a **duplex, asynchronous, stateful
Tambara module**, where more actions (mixed, temporal, keyed) exist than pure
profunctor algebra provides, and where laxity has a price (the gate) that
pure treatments never see.

## Rolled out

§6 is implemented:

- **`src/Data/Profunctor/Acting.purs`** — `class Acting`/`acted` (the keyed
  `Array` container action), instances for `(->)` (`acted _ = map`),
  `PUI Effect` (the probe carrier), and `PUI Web` (DOM hooks); one shared
  keyed reconciler carries both emission modes — `acted` gathers
  (knowledge-gated, announces `[]` on an empty feed), `collapsed` forwards
  (ungated, silent on empty); `optioned` derives the `Maybe` action.
  `Data.Profunctor.Row.Sequence`/`Sequencing` is deleted;
  `PUI.HTML.foreach = collapsed` — surface and demo behavior unchanged.
- **Value-level laws** in `test/Main.purs` on `PUI Effect` probes: empty
  announces `[]` (and nothing at registration), singleton retraction, gather
  gate (withhold → complete → retain-last), identity-follows-key (zero
  rebuilds on re-feed/permutation/drop; output order follows fed key order),
  collapsed silent-on-empty. DOM-identity laws stay in
  `scripts/smoke/tests/sequence-laws.mjs` (green).
- **`demo/nguis/potluck/`** — the focused `acted` demo: per-guest dish
  editors lifted with `# acted _.name`, the menu summary withheld by the
  gather gate until every guest has chosen, retain-last on re-choices, row
  nodes surviving every re-feed (`scripts/smoke/tests/potluck.mjs`).
- The two laws of §1 are stated once in `Data.Profunctor.Row`'s header; the
  duoidal reading of §0 (displayed = comultiplication, muted = counit) in
  `PUI`'s header.

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
normalization for profunctors).
