# Guardrails

*The normative document. [why-bambik.md](why-bambik.md) tells the story;
this states the rules. Every MUST and MUST NOT below is strict: a change
that violates one is wrong even if it works, ships, and demos well. Each
rule cites its grounding — a design note, a law, or a precedent where the
library already paid to learn it.*

The rules exist because bambik is a bet, and a bet pays only if it is not
hedged:

> A UI component is a profunctor. Everything is structure **on** the one core
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
- Every component MUST be a citizen of exactly one direction, and its
  row MUST speak the **business label**, stated once as the leaf's own
  type argument (`text @"Total"`, `filledTextField @"First name" {}`,
  `button @"Submit order" {}`, `select @"Milk" cfg opts`). No canonical
  label (`value`/`clicked`/`event`) appears in application code: adopters
  that need a leaf's label derive it from the closed singleton row via
  `RowToList`'s fundep (`forProperty`/`required`/
  `optional`/`toCases`/`forCase`/`forCases`; the view-side read adopters
  `projection`/`projected` and the singular `forCase` are deleted —
  displays are verbatim under the presentation-model rule,
  doc/research-presentation-model.md). Statuses keep their
  internal payload case private and derived. Components MUST NOT have
  scalar or polymorphic model interfaces; raw scalar leaves stay private
  or in optic positions.
- A **label is the copy it draws**: a captioned leaf's caption defaults
  to its label verbatim — nothing derives copy from an identifier — so
  labels are written as the words they render and are therefore usually
  quoted strings, in the business rows as much as at the leaf
  (`{ "First name" :: String }`). A quoted label MUST NOT appear in a
  record pun (the compiler forbids it); bind explicitly instead. An
  emitter MUST NOT be given a `label:` config: where a trace form's loop
  case would force two buttons to share one case under different words,
  they are two business actions — each takes its own case and `toCases`
  adopts it into the loop case. An editor's caption config is held to the
  same rule: the label carries the copy whole — punctuation, format hints
  and units included (`@"Start date (DD.MM.YYYY)"`, `@"Amount (€)"`) — and
  a caption that merely repeats what the label already says MUST move onto
  the label. Only copy a label genuinely cannot be (localized wording)
  stays in config. A selector's **options** are its ordered case labels
  (`choice @l` per option), never a hand-written `{ value, label }` array;
  the order MUST be the order written, never the variant row's, which the
  compiler sorts alphabetically.
- The closure of the discipline is the **anchor invariant**
  (application-side statement: writing.md's *The anchor invariant*):
  every view line names exactly one semantic anchor — a model **field**
  (`@l` on an editor or selector), a business **case** (`@l` on an
  emitter, pane or status), a **named read function** (a display's
  content, L17), or **nothing** (chrome — statics and oculars write
  nothing and so name nothing). The vocabulary MUST keep every line's
  anchor expressible and singular: an ocular MUST NOT take a label or
  copy config (no model interface, nothing to anchor — a card's heading
  is typography in its content), a display MUST NOT carry a label except
  as an accessible name (L17), and no mechanism may leave a line's
  meaning in an anonymous position. The stronger rule — *every line a
  field*, oculars included — was considered and rejected (2026-09-03):
  a label on an ocular is a parameter that does nothing (gate 4),
  line ↔ field is no bijection (one `settled` spans fields, two lines
  may read one field, `+`-side lines anchor at cases), and making
  display lines fields would move copy back into state, reversing L17.
- The **labelled group** `group @l` (MDC2/MDC3, admitted 2026-09-04) is
  the boundary confirmed, not breached: it is a component, not an
  ocular — its label is the sub-record field it nests (`field @l` fused
  with the card surface), doubling as the heading copy verbatim and the
  accessible group name (`role="group"`), so the label is an anchor
  doing work, never a dead parameter. The criterion it instantiates is
  the **leaf-fusion criterion**: `@l` fuses onto a wrapper exactly where
  the label does work a trailing `# field @l` cannot. Chrome that gains
  a label this way is *renamed into the component sort* — the blind
  `card` stays for surfaces grouping no model, as `subStrong` stays for
  flat sub-row focus. The criterion's closure (2026-09-05): the
  plain-HTML floor's `input @l`/`textArea @l` fused too (the `name`
  stamp was always work the bracket could not do), and with every public
  editor lifted inside its vocabulary and sub-model nesting carried by
  `group @l`, **`field` left the application surface** — it is no longer
  re-exported from `PUI`, living as design-system plumbing beside
  `widenRecordInput`; a nesting no mechanism fits is a
  missing-vocabulary signal (L16), never a reason to reach for the
  lens. Checkable form: `grep "field @" demo/` is empty, always.

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

- A merge MUST NOT carry a unit of its own where a wire fits. Its unit law
  is conditional on the carrier: *if* `p` is also a `Category`, the wire at
  the unit object — `identity @{}` for `×→×`, `identity @(Variant ())` for
  `+→+`, `lcmap case_ identity` for `+→×` — MUST play well with the merge,
  exactly, not up to an echo, because a record gate MUST treat
  a contribution of zero fields as no contribution (L6). The one unit no
  wire reaches is `×→+`'s (`{}` is terminal, `Variant ()` initial — nothing
  maps terminal→initial), and it is the one class-member unit, `silence`
  (parametricity, not policy). Pointing — one emission at registration — is `Seeding`'s
  `announce`, never a unit's: units carry no information and add none.
  No future combinator may invert this. Every starvation bug in the
  library's history was a `+`-behaviour where a `×`-behaviour was
  required; the units and the point are the fixed points that make that a
  diagnosable error.

### L6. Knowledge gates: nothing flows until it is genuinely known.

- A gate MUST withhold until every field has a producer that has spoken,
  and MUST retain last-known values thereafter. The vacuous case is
  decided: an operand owning **zero fields is pre-satisfied** — its only
  possible contribution is the informationless `{}`, which is always
  known, so waiting for it is waiting for nothing and no invented data
  can flow (the silence law in test/Main.purs) — and **inert**: a
  zero-field side's emissions neither open nor re-fire the gate, so a
  wire, a silent display and an announcing one are one operand and
  `identity @{}` is the unit exactly. This is what makes a
  display-side operand unable to starve its siblings, and the
  display-beside-the-wire construction (the gated displays' bodies) a
  derived form rather than a carrier primitive. A gate MUST NOT be
  papered over with invented data. Three designs are permanently
  rejected:
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
  `acted`/`edited` the element's output row excludes the key and the
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
  in place via `text`/`attrWith`); wholesale rebuild (`dynamic`,
  `each`) is admitted only where structure genuinely varies
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
  `Milliseconds` in a UI component signature); unit payloads as `{}` (no
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
  duplicated label via `DisjointLabels`); named runtime watchdogs where they
  cannot (gate starvation names the gate and the missing fields). A
  blank screen with no diagnosis is a library bug, always.
- **No silent information loss.** A combinator that discards the
  emissions of the component it wraps MUST demand, in its type, that
  those emissions carry nothing: the gated displays' content slots and
  `observed` accept only the output `{}` — exactly what a unit display
  emits, and no new class to say so — so an editor or emitter placed
  where a display belongs fails to unify, never swallows an edit.
  Deliberate discarding is its own visible word: `muted`, the counit
  (render and drop), written at the call site (`# muted` on a `foreach`
  that forwards its elements, or a packaged collection display echoing
  its array); an adopted display keeps its `{}` through the input-side
  adopter (`atField @l`, not `field @l`). Information may be lost only
  in writing.

### L14. The library stays small by rule, not by accident.

- **Demo-reachability:** every export is reached by a demo or a law
  test; what nothing reaches gets pruned (the pruned combinators live on
  in the design notes, where the algebra needs their names).
- **Subsumption:** when a new form makes an old one derivable, the old
  one is deleted, not deprecated alongside (`synced`/`latch`,
  `Sequencing`, `labeled`, `constantly`, `forField`/`asCase`,
  `humanizeLabel` — all gone). Convergence is the health metric:
  a design is right when the algebra starts subsuming its own features.
  `humanizeLabel` is the sharpest case: once a label *is* the copy it
  draws, deriving copy from an identifier has nothing left to do.
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

### L16. The import tower: only the algebra layer touches the ecosystem's algebra.

The codebase is three floors, each greppable:

- **Algebra layer** (`PUI`, `Data.Profunctor.Row.*`, `Data.Profunctor.Acting`,
  the optics and `Seeding`) — the **only** importer of the ecosystem's
  `Data.Profunctor` (and `.Strong`/`.Choice`/`.Cochoice`/...). This is
  where instances live, so it is forced. (Bambik's own `Data.Profunctor.Row.*`
  and `.Acting` namespaces are vocabulary, not the ecosystem — the greps
  target `import Data.Profunctor (`/`.Strong`/... exactly.)
- **Vocabulary layer** (the design-system modules, `PUI.Web.HTML`/`PUI.Web.SVG`,
  packaged control modules) — builds from the **carrier** (its license:
  `wrap`/`unwrap`, `PUI.Web`, FFI) plus the same re-exported vocabulary
  applications use (`field`, `recordToCase`, `projected`, `blank`).
  It never imports the ecosystem algebra: a design-system module proves
  the vocabulary complete by being its own first customer. **The floor is
  the namespace**: every web vocabulary is a submodule of its carrier
  (`PUI.Web.*`, under `src/PUI/Web/`), so a module path states which
  carrier a vocabulary specializes and the carrier-independent algebra
  stays visibly apart from it.
- **Application layer** — vocabulary only: no `Data.Profunctor`, no
  carrier internals (Part II, and the rule as applications read it in
  writing.md's *Wiring*).

The consequence is the **mechanism-argument doctrine**: a projection is
an argument of the mechanism that consumes it, never a loose `lcmap`/
`rmap` stage — `provided @l classifierOf`, `foreach @l rowsOf`, `listOf opts
rowsOf`, `dispatched envelopeOf`, `toCase @l payloadOf`, `forCase @l copyOf`,
`forProperty`, `toCases outcomeOf`, `forCases lineOf`, `settled normalize`,
`bracketed stateOf caseOf` (`identity` says verbatim). A shape none of
these fit is a missing-vocabulary signal addressed to the library —
the next `required` waiting to be coined — never a reason to import the
module one floor down. Business optics (`Shutter`/`Reel` in business
code below the UI) are algebra-layer material and exempt by location.

### L17. Copy is a function, not a field.

- What a pipeline operates over is the app's **state**, never its
  rendering. A display leaf whose content *is* copy MUST take the
  **read function** — `text :: ({ | reads } -> String) -> PUI Web
  { | reads } {}` — and MUST carry no label: its content is the copy,
  so there is no field to name and nothing to caption (a caption is
  surrounding chrome). The function MUST be a named function of the
  logic module or a bare accessor section; a formatter bracket, a
  view-side lambda and a `staticText`-plus-leaf text run are all
  forbidden. `projection` (2026-08-31) and `projected` (2026-09-02) are
  deleted; the vocabulary-internal `textOf` serves the statuses' own
  variant payload.
- A display that renders a **number** — `progressBar`,
  `linearProgress`, `progress`, `gauge`, `ratingDisplay` — MUST take a
  read function too: a fraction is *derived* (a ratio of source
  fields), and derivation is the same act as formatting. Its label
  survives as the **accessible name only** (a bar showing 42% must
  announce *what* is 42%), so it is copy like an editor's caption, not
  a field reference: `progressBar @"Elapsed" elapsedFraction`.
  Quantity *editors* are untouched — a slider genuinely edits a field,
  so `sliderLive @l` keeps label-as-field. `forProperty` survives for a
  *labelled* leaf reading one field of a context-pinned wider row —
  selection, never formatting.
- A model field MUST exist because the app's state needs it, never
  because a display wanted a `String` — or a `Number`. `settled`
  therefore maintains invariants among **edited** fields only (two
  writers inherent, type preservation the point); a `present<App>`
  normalization that feeds a display, of either kind, is a violation.
  Checkable form: every `# settled` in `demo/` sits on an editor
  stage. A context-pinned row (collection item, pane payload)
  carries the **source** fields its producing function built, and the
  read function formats them.
- The gain is the point: the screen's copy is a pure function under
  `spago test`, no browser, and the view line names its own writer.
  Checkable form: `npm run check-view-model` rejects
  `projection`/`projected` anywhere in `demo/` and any lambda in a
  `text` read.
- Statuses adopt through `forCases` (a whole classified variant — the
  classifier a record of per-case copy functions, the elimination the
  mechanism's own) and its derived single-case convenience `forCase @l`
  (`forCase @l f = forCases { l: f }` by law); their canonical
  `[ event :: String ]` row stays private to the vocabulary.
- Rationale, census and laws: doc/research-copy-is-a-function.md, which
  partially reverses doc/research-presentation-model.md (keeping its
  testability motivation and its `settled` half); the application-side
  statement is writing.md's *copy is a function, not a field*.

---

## Part II — The approach (applications built on bambik)

The rules for application code are stated once, in the authoring skill's
[writing.md](../.claude/skills/developing-bambik-apps/writing.md) — its
**Code style** section is the strict contract (layout, types and values,
business functions, wiring), and the sections before it are the
vocabulary and shapes that contract is written in. That document is
written from the application developer's perspective and ships to
external users with the skill; this one governs the library. They are
not two statements of the same rules: **writing.md is normative for
application code**, and nothing here restates it.

The library's obligations to it are one-way and concrete:

- The demos in `demo/7guis/` and `demo/nguis/` are the executable form
  of that contract — L15 makes them the compatibility contract, and gate
  5 below makes reachability a merge condition. A demo that breaks a
  rule in writing.md is a broken demo.
- L16 above fixes the import tower whose top floor writing.md states as
  "application code never imports `Data.Profunctor`". The checkable
  form: `grep "import Data.Profunctor (" demo/` is empty, always.
- A shape no mechanism fits is a **missing-vocabulary signal** addressed
  to the library — the next `required` waiting to be coined — and is
  answered here, by admitting vocabulary through the gates below, never
  by relaxing a rule in writing.md.
- When a library change alters what application code should look like,
  writing.md is the file that changes, and the demos change with it.


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
7. **Sync.** Library rules are stated in the module headers and this
   document; application rules are stated **only** in
   [writing.md](../.claude/skills/developing-bambik-apps/writing.md).
   A change that alters how applications are written edits writing.md
   and the demos — not a second copy of the rule here or in CLAUDE.md,
   which carry pointers. The deployed restatements are exactly two,
   both deliberate (deployed HTML cannot read the skill file): the demo
   pages' code-style note, and the demo site's workflow page
   (demo/workflow.html — *Writing order* replayed as a session, its
   compiler responses captured verbatim from the pinned compiler);
   re-read both against writing.md when the contract changes.

The historical record is the proof this process works: every rejected
design (the type-derived seed, the `Sequencing` class, the `New` wrapper,
`synced`/`latch`, `labeled`) is recorded here with its reasons, and every
survivor carries its laws. Guardrails are cheap to state and expensive to
recover once breached — this document exists so no future convenience,
contributor, or agent trades a rule for a feature without noticing the
price.
