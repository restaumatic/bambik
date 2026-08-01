# Guardrails

*The normative document. [why-bambik.md](why-bambik.md) tells the story;
this states the rules. Every MUST and MUST NOT below is strict: a change
that violates one is wrong even if it works, ships, and demos well. Each
rule cites its grounding — a design note, a law, or a precedent where the
library already paid to learn it.*

The rules exist because bambik is a bet, and a bet pays only if it is not
hedged:

> A widget is a profunctor. Everything is structure **on** the one core
> type — instances, plain combinators, laws — never machinery beside it.
> Refuse anything that does not come from the algebra.

Every guardrail is a consequence of taking that seriously.

---

## Part I — The library

### L1. One core type. Everything is structure on it.

```purescript
newtype PUI m i o = PUI (m { toUser :: i -> Effect Unit, fromUser :: (o -> Effect Unit) -> Effect Unit })
```

- Every feature MUST be an instance of a class on `PUI m`, a plain
  combinator over such instances, or a law relating them. There MUST NOT
  be a runtime beside the type: no scheduler, no store, no context
  registry, no lifecycle machinery that the wiring record cannot express.
- Before a bespoke class or combinator is admitted, it MUST be located in
  standard algebra first (monoidal structure, Tambara modules, traces,
  container actions — [collections-profunctor-algebra.md](collections-profunctor-algebra.md)).
  A bespoke concept is admitted only with a proof of irreducibility.
  *Precedents:* the `Sequencing` direction was rejected for the standard
  container action (`Acting`); `synced`/`latch` dissolved into `looped`;
  debouncing turned out to be a theorem (`coresolve (resolve g) =
  debounced g`), not a gadget. `looped` is the model irreducibility proof
  (a gated `unfirst` cannot self-feed), and `Resolving`/`Retaining` the
  model honest coinage (Tambara-like structures for actions only a
  temporal, stateful carrier supports).
- The novelty budget is spent in exactly one place: the **carrier** — a
  duplex, asynchronous, stateful profunctor supports more actions (mixed,
  temporal, keyed) than a pure one. New capability MUST chart that
  territory, not invent abstractions beside it.

### L2. Carrier polymorphism, honest instances.

- The algebra layer (`Data.Profunctor.*`) MUST stay carrier-generic.
  `Web` is one carrier; the laws MUST be value-testable on a probe
  carrier (`PUI Effect`, `(->)` where lawful) without a DOM.
- A class MUST NOT be given an instance a carrier cannot honestly
  support. `(->)` has no `Retaining` (a stateless function has nowhere to
  keep memory), no `Resolving` (no notion of quiescence), no `Seeding` (a
  timeless carrier has no registration moment). A missing instance is a
  theorem about the carrier, and it MUST be kept missing. Faking one —
  a trivial always-`Done` `resolve`, a `Default`-fabricated `retain` —
  is the cardinal sin: it makes the types lie.

### L3. The compass is closed: four directions, and only four.

- Models come in exactly two shapes — Record (`×`, all-at-once, entity)
  and Variant (`+`, one-at-a-time, event) — giving exactly four
  directions. There MUST NOT be a fifth: anything that looks like one is
  either a corollary of closure (collections = the algebra closed under
  `μ`) or wrongly conceived.
- Every component MUST be a citizen of exactly one direction, speaking a
  canonical row (`{ value :: String }`, `[ clicked :: r ]`,
  `[ event :: String ]`), adopted per business label
  (`asField`/`asCase`/`forCase`). Components MUST NOT have scalar or
  polymorphic model interfaces; raw scalar leaves stay private or in
  optic positions.

### L4. The merge law: sharing is inclusive, responsibility is exclusive.

- Record fields and variant emissions may overlap (data copies freely);
  variant handling and record production MUST be disjoint (responsibility
  never splits). Runtime evidence appears exactly where responsibility
  does (`DispatchableVariants`, `MergeableRecords`) and nowhere else.
- Merge operands' emissions MUST be runtime-exact: trimmed to their
  declared row before the gates combine them, so no stale sibling field
  can shadow a fresh value. Exactness is enforced by the merges, never
  left to convention.

### L5. Units are forced, not designed.

- The `×`-output unit MUST announce (`{}` is terminal and inhabited — the
  only lawful choice); the `+`-output unit MUST be silence (`Variant ()`
  is uninhabited — parametricity, not policy). No future combinator may
  invert this. Every starvation bug in the library's history was a
  `+`-behaviour where a `×`-behaviour was required; the units are the
  fixed points that make that a diagnosable error.

### L6. Knowledge gates: nothing flows until it is genuinely known.

- A gate MUST withhold until every field has a producer that has spoken,
  and MUST retain last-known values thereafter. A gate MUST NOT be
  papered over with invented data. Three designs are permanently
  rejected ([pointedness-entities-vs-events.md](pointedness-entities-vs-events.md)):
  - **no `Initial`/`Default`-style type-derived seeds** — initial state
    is business data, a term; a class invents wrong values with right
    types;
  - **no auto-announcing leaves** — the union of rendered leaf-initials
    is a bogus first model, and it would flow;
  - **no `Maybe initial` slot in the wiring** — re-encoding optionality
    the types were supposed to eliminate.
- The gate is UX, not overhead (potluck's withheld menu). Any proposal
  whose effect is "gates open at registration" is wrong by construction.

### L7. Pointedness is a typing discipline, supplied by terms.

- A record input row is an initial-state obligation; `{}` is the one
  self-pointed record. `body` MUST demand a closed app (`PUI Web {} o`),
  and every knot-tying record-channel form (`feedback`, `folding`,
  `unfolding`, `mvu`/`with`) MUST take its t=0 value as an argument the
  caller cannot omit.
- Events MUST NOT be primed — no canonical first occurrence exists;
  `iterate` stays seedless deliberately (events occur, they don't
  pre-exist). The carrier tells you the shape axis, not the time axis:
  record-carried, event-natured channels (`select` before first pick,
  `acted`'s withheld aggregate) stay unpointed.

### L8. Values on the wire are plain business values.

- No metadata rides the channel: no transiency flags, no dirty bits, no
  wrapper types annotating emissions. Temporal semantics MUST be derived
  from time itself (quiescence, registration moment), never encoded in
  the payload. *Precedent:* the `New` continuity-`Boolean` wrapper that
  once rode every wire — dissolved by deriving the `resolve` branch from
  time. Any proposal reintroducing a wrapper on the wire repeats that
  mistake.

### L9. Identity is data; reconciliation is naturality.

- Collection identity MUST be materialized: a model field (`@l` on the
  `×`-members) or the structural tag (`{ key, value }` on the
  `+`-members) — never a DOM annotation (`data-*`), never a render
  index, never carrier-private guesswork.
- Identity MUST be unforgeable where the carrier owns it: in
  `acted`/`edits` the element's output row excludes the key and the
  carrier re-attaches it. Stateful carriers MUST make identity follow
  the key (nodes move with their keys; matched elements re-feed in
  place). Keys are labels, never rendered content.

### L10. Direct wiring. No virtual DOM, ever.

- The library MUST NOT diff trees, re-render to reconcile, or
  reconstruct at runtime a correspondence the program already wrote
  down. `toUser` updates exactly what the wiring reaches; code order is
  DOM order.
- Structure computed from data stays in the algebra: fixed structure fed
  as data through the retaining collection members (build once, update
  in place via `text`/`attrWith`); wholesale rebuild (`foreachWith`,
  `dynamic`, `each`) is admitted only where structure genuinely varies
  with the data (markdown blocks), and each rebuild owns its container.
- There MUST NOT be a markup DSL, a template language, or an HTML
  string surface in the public vocabulary (`staticHTML` is internal
  chrome plumbing only). UI structure is `PUI` values all the way down.

### L11. No global anything.

- No global state store, no dispatcher, no message bus, no subscription
  registry, no context providers, no portals, no ref escape hatches.
  All communication travels the wires: composition (`⊳`), merges (`⊗`),
  and the sanctioned loops (the trace quartet, `looped`). Local state
  exists only as the algebra's residuals (`Retaining`'s durable `c`,
  `Resolving`'s ephemeral `c`, the seeded knots) — invisible in the
  pipeline's types by co-optic hiding, never by side channel.

### L12. The vocabulary never forces a nominal type.

- Canonical rows adopted by label; anonymous-record configs
  (`{ floatingLabel }`); durations as `{ ms :: Number }` (never
  `Milliseconds` in a widget signature); unit payloads as `{}` (no
  `Default Unit` instance, deliberately); selector options carry
  variant-row values, never strings-as-enums (the `labeled` helper was
  removed because it existed only to feed that antipattern).

### L13. Laws before API; failures have names.

- A combinator without laws stated in its module header is not
  vocabulary. Value-level laws run in `spago test` on probe carriers;
  carrier-only laws (DOM identity, live gates) run in the smoke
  harness. The module headers are the single source of truth for
  contracts (`npm run api-docs` renders them).
- Wrong programs MUST fail with names, as early as possible: compile
  errors where the types can carry the judgment (unclosed app at `body`,
  duplicated label via `DisjointLabels`, archetypes catalogued in
  [type-errors.md](type-errors.md)); named runtime watchdogs where they
  cannot (gate starvation names the gate and the missing fields). A
  blank screen with no diagnosis is a library bug, always.

### L14. The library stays small by rule, not by accident.

- **Demo-reachability:** every export is reached by a demo or a law
  test; what nothing reaches gets pruned (the pruned combinators live on
  in the design notes, where the algebra needs their names).
- **Subsumption:** when a new form makes an old one derivable, the old
  one is deleted, not deprecated alongside (`synced`/`latch`,
  `Sequencing`, `labeled` — all gone). Convergence is the health metric:
  a design is right when the algebra starts subsuming its own features.
- **Scope:** bambik owns model↔UI wiring and its algebra. It MUST NOT
  grow a router, an animation system, a theme engine, a CSS-in-PS
  layer, a state-management add-on, or an HTTP client. Styling is
  ordinary CSS plus the design-system vocabularies; effects enter
  through the carrier (`action`/`affAdapter`) and stay at the edge.
  Anything in this list that ever seems needed must first be shown to
  be a corollary of the algebra — the collection standard applies.

### L15. Demos are the contract. They must keep working, whatever changes.

- The demo suites (demo/7guis/, demo/nguis/) are bambik's compatibility
  fixtures — living applications that every library change is tested
  against. A change to the library, however internal, MUST leave every
  demo compiling, bundling, and **behaving correctly**, and this is
  verified by running the full stack, never assumed:
  1. `spago build` — library, tests, and all demos compile together;
  2. `spago test` — the value-level laws on probe carriers;
  3. `npm run bundle-demos` — every demo bundles;
  4. `npm run smoke` — the headless-Chrome harness walks the live pages
     (seeded renders, gates, wizards, reconciliation, toasts).
  All four green is the precondition for merging anything. A demo that
  compiles but misbehaves is a failure the same as a compile error —
  which is why behaviour lives in smokes, not in eyeballs.
- A demo may change only when the demo itself is the subject of the
  change. A library change that forces demo edits to stay green is an
  **API break** and must be treated as one: deliberate, documented,
  applied across all demos and docs in the same change — never a silent
  drive-by fix in one demo to get the build past.
- Coverage grows with the vocabulary and never shrinks: every new
  combinator or component lands with a demo reaching it *and* a law
  test or smoke asserting its behaviour, so "the demos pass" is a
  guarantee that tightens over time rather than decays.

---

## Part II — The approach (applications built on bambik)

### A1. No nominal types in UI.

The design rule, wholesale ([no-nominal-types-in-ui.md](no-nominal-types-in-ui.md)):
UI code declares no `data`, no `newtype`, no `type` synonyms. Anonymous
record rows for all-at-once, anonymous variant rows for one-at-a-time,
`{}` for unit payloads, primitives at the leaves, `Array`/`Maybe` as the
only generic containers. Role names live on **values** (`mvu
plannedTrip`, `with emptyCanvas`) and business function names, never on
types. Nominal types belong below the UI — recursion (an AST) or an
ecosystem API (`Aff`, `Either`, `Milliseconds` inside business actions)
— and enter only as rows projected by business functions.

### A2. Exact footprints; rows are read narrow, payloads are exact.

Every business function states its exact footprint as a closed narrow
row — what it reads ∪ writes, never the whole model. The subsuming
stages (`updates`/`tapped`/`displayed`/`edits`/`acted`/`completed`)
absorb the widening; applications MUST NOT coerce rows at call sites
(`widenRecordInput` is library plumbing, deliberately not re-exported).
A handler that reads nothing is not a transformer but a constant patch.

### A3. Business emissions carry bare data, never UI copy.

Toast and banner text lives in `PUI Web`-returning widget functions
(`welcomeToast = snackbar # forCase @"registered" # lcmap (match …)`);
the business event carries the order, the outcome, the reason — the
data, not the sentence. Validation results are payloads, not strings
destined for a specific widget.

### A4. Visibility is business logic.

Conditional visibility is always a **named `Maybe`-valued projection**
plus `provided` — never an in-UI predicate. The visibility rule thereby
lives in testable business code. `clWhen` stays predicate-driven
because it toggles styling, not existence.

### A5. State lives in the model or in the algebra's loops. Nowhere else.

No FFI stashes, no module-level `Ref`s, no reading the DOM back as
state, no window globals. The model under `mvu` holds the entity; a
widget's private state is a residual threaded by
`Retaining`/`Resolving`/the seeded knots — hidden by the algebra, not
by a side channel.

### A6. The architecture is readable off the types.

An application is a compass walk written as one pipeline — `load → form
(×→×) → live summary → events (×→+) → dispatch (+→+) → statuses (+→×)`
— closed by `mvu seed` to `PUI Web {} model`. If the top-level types do
not tell that story, the structure is wrong, not the types. The
residual input row is the to-do list of unsupplied initial state; a
demo reads `body $ pipeline # mvu seed`, and anything that obscures
that reading (indirection layers, widget registries, config objects
that assemble UIs reflectively) is out.

### A7. Lossy conversions live in the model, not in leaf brackets.

Editors obey the `dimap` round-trip contract: the bracket around a leaf
must round-trip. A lossy normalization belongs in the model via `rmap`
after `completed`, where the loop makes it a transaction — never hidden
inside a component's `dimap`.

### A8. Business literals never hide in UI code.

Numeric bounds and steps (a slider's `min`/`max`/`step`), seed models
(`mvu`/`with`/the trace forms' arguments), tick periods, and default
payload values are business data, and each is a **named top-level
business definition** — never a literal inline in a widget config or at
a seed position. Names speak business language (`smallestLoan`,
`tickPeriod`, `gameStart`, `roomTemperature`), never lifecycle language
(`initial`, `default`, `seed` are the smell's second form). UI code
keeps only presentation: labels, captions, icons, styles, structure —
layout numerics (a textarea's `rows`, a grid's `columns`) stay UI.

---

## Part III — The admission test (how the guardrails are enforced)

Any proposed change — combinator, class, component, demo idiom — passes
these gates in order:

1. **Derivation.** Locate it in the algebra: an instance of a standard
   structure, a row form of an existing strength, a corollary of
   closure. If it needs a new concept, prove irreducibility (the
   `looped` standard). If it can't be derived and can't be proven
   irreducible, it stays out.
2. **Laws.** State its laws in the module header before writing demos
   against it; land value-level tests on the probe carrier, smoke tests
   for carrier-only behaviour.
3. **Subsumption sweep.** Ask what the new form makes derivable — and
   delete that. A feature that only adds is suspect; the best features
   shrink the library.
4. **Honesty audit.** No instance a carrier can't support, no default a
   type can't justify, no metadata on a wire, no name for a concept the
   ecosystem already names (coin — `Resolving`, `Shutter`, `Reel` —
   only where the literature is genuinely silent).
5. **Reachability.** A demo (or law test) reaches it, or it doesn't
   merge. Demos are the executable form of the rules — exactly one type
   declaration survives in ~3,000 demo lines, and that ratio is a
   regression test on this whole document.
6. **Green stack.** The obligatory verification suite of L15 passes in
   full — `spago build`, `spago test`, `npm run bundle-demos`,
   `npm run smoke` — with no demo edited except where the demo is the
   subject. Demos must work correctly no matter what changes in bambik;
   a red smoke is a veto, not a nuisance.
7. **Sync.** The statements of the rules stay in sync: module headers →
   this document → CLAUDE.md → the demo pages' code-style note.
   Changing one changes all.

The historical record is the proof this process works: every rejected
design ([pointedness-entities-vs-events.md](pointedness-entities-vs-events.md)
options A–D, the `Sequencing` class, the `New` wrapper, `synced`/
`latch`, `labeled`) is documented with its reasons, and every survivor
carries its laws. Guardrails are cheap to state and expensive to
recover once breached — this document exists so no future convenience,
contributor, or agent trades a rule for a feature without noticing the
price.
