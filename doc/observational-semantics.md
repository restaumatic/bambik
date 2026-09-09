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
   finite synchronous cascade.

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
- **Stutter**: on a **record** channel a consecutive duplicate emission is
  no observation. A record channel carries a *behavior* — a value at every
  moment, so repeating the current value is the same function of time — and
  boundary streams on record channels are compared **up to stutter**. A
  **variant** channel carries *events*; there every emission counts. This
  is the inner-feed quotient above seen from the other side, and the same
  fact as feed-idempotence (§3): to be a behavior *is* to be
  stutter-invariant.
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
2. **Record-echo totality** — a `×`-output citizen answers every feed with
   at least one emission (displays release the fed row, editors echo,
   `identity` is the echo wire). What keeps the gated merges live and what
   the seeded `×`-retraction laws quantify over.
3. **No synchronous variant echo** — a `+`-output citizen never emits from
   inside its own `toUser`. Events are occurrences, not responses; this is
   the termination argument for `Cochoice`'s re-entry (an event loop, not a
   busy loop) and for `iterate`.

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
    (echo-total, feed-idempotent), it **holds up to stutter** — and only up
    to stutter: each middle-gate release broadcasts to both `h` and `k`,
    each echoes, and the merged-first side emits a duplicate where the free
    side emits once (tested with the exact streams in test/Main.purs).

  So the answer to "is the gated merge monoidal?" is "which category?":
  counting only channels, it is monoidal up to stutter; counting renderings,
  it is premonoidal — unit, associativity and symmetry hold outright (tested,
  all four merges), interchange is bought back only as `⊑`. Symmetry itself
  is a boundary-stream fact — operand order stays observable in feed order
  to the operands, inside the `≈` quotient.
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

The variant editor `bracketed stateOf caseOf` embeds a sum into the product
of its summands and projects back. Its arguments owe one law, stated in
`Data.Profunctor.Row.VariantToVariant` and tested on the demos' pair:

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

The gated merges emit on every contribution, so a *diamond* — one upstream
field feeding two transforming stages whose outputs re-merge — emits torn
intermediate rows. The algebra permits diamonds; **idiomatic bambik never
builds them**: copy-is-a-function and the one-`settled`-normalization rule
(writing.md) put all derivation in a single stage, so every merge's operands
are independent sources and a "stale" sibling field is genuinely the current
value of an unedited field. Glitch-freedom is guaranteed by the writing
contract, not by the carrier — a diamond an application builds anyway is a
style violation before it is a runtime surprise.

## 9. Where the tests live

test/Main.purs, in order: the merge unit/silence/exactness/gating laws; the
trace quartet and its row forms; the Category laws; the container-action
laws; then the audit section — seeded retractions and the three raw
deadlocks, the `Looping` triple (yanking, conjugation with the `dimap f f`
counterexample, idempotence with the feed-duplication quotient made
visible), the Strong deviation, the interchange deviation at the inner
surface and its refinement direction, merge symmetry (`×→×`, `+→×`) and
mixed-merge associativity (`+→×`, `×→+`), the container action's wire law,
and the `(->)` diagonal-merge instances' laws as pure equalities; then the
observation-level section — boundary interchange up to stutter with the
exact stuttering stream, `⊑`-monotonicity of `>>>` and `⊗`, the container
action's laxity at the inner surface, `bracketed`'s retraction on the
order-form pair, the Ocular admission law for a node-wrapping ocular and its
failure for a capturing decorator, and `announce`'s naturality.
