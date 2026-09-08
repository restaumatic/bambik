# The observational semantics: what the laws are stated against

*Normative companion to the algebra's module headers (2026-09-08, from the
category-theoretic audit). Every law in `PUI`, `Data.Profunctor.*` and
`Data.Profunctor.Row.*` that is not a plain equality of pure values is stated
against the definitions here. The headers cite this file rather than
restating it; test/Main.purs exercises each named law and each named
deviation.*

## 1. The two-phase protocol

A `PUI m i o` value denotes, **per instantiation** (per `unwrap` under the
carrier's `m`), one process with two channels and two phases:

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
- **Interchange fails — the merges are not monoidal.** For the gated merges,
  `(f ⊗ g) >>> (h ⊗ k) = (f >>> h) ⊗ (g >>> k)` fails on the nose: the
  merged-first side synchronizes at the middle gate, so `h` sees nothing
  until *every* operand of the first merge has spoken. The failure is
  one-directional — the merged-first side **refines** the other
  (`(f ⊗ g) >>> (h ⊗ k) ⊑ (f >>> h) ⊗ (g >>> k)`), which is the lax duoidal
  interchange of doc/collections-profunctor-algebra.md §0, meaningful
  exactly because `⊑` exists. So `⊗` is a premonoidal-style tensor: unit,
  associativity and symmetry hold (tested, all four merges), interchange is
  bought back only as `⊑`. Symmetry itself is a boundary-stream fact —
  operand order stays observable in feed order to the operands, inside the
  `≈` quotient.

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

## 7. Glitch-freedom is a style theorem

The gated merges emit on every contribution, so a *diamond* — one upstream
field feeding two transforming stages whose outputs re-merge — emits torn
intermediate rows. The algebra permits diamonds; **idiomatic bambik never
builds them**: copy-is-a-function and the one-`settled`-normalization rule
(writing.md) put all derivation in a single stage, so every merge's operands
are independent sources and a "stale" sibling field is genuinely the current
value of an unedited field. Glitch-freedom is guaranteed by the writing
contract, not by the carrier — a diamond an application builds anyway is a
style violation before it is a runtime surprise.

## 8. Where the tests live

test/Main.purs, in order: the merge unit/silence/exactness/gating laws; the
trace quartet and its row forms; the Category laws; the container-action
laws; then the audit section — seeded retractions and the three raw
deadlocks, the `Looping` triple (yanking, conjugation with the `dimap f f`
counterexample, idempotence with the feed-duplication quotient made
visible), the Strong deviation, the interchange deviation and its
refinement direction, merge symmetry (`×→×`, `+→×`) and mixed-merge
associativity (`+→×`, `×→+`), the container action's wire law, and the
`(->)` diagonal-merge instances' laws as pure equalities.
