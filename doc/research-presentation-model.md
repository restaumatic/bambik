# Research: the presentation model — verbatim displays

Status: **EXECUTED** (2026-08-31) — all 26 demo families migrated,
`projection` deleted, `projected` demoted to vocabulary plumbing,
`forCase` re-admitted as a derived word after a brief dissolution (see
the vocabulary table), `check-view-model` extended, docs synced; full
L15 stack green
(`spago build` 0 warnings, `spago test`, `bundle-demos`, `smoke`).
Companion note: doc/thought-output-leaves-and-plumbing.md (the inventory
this proposal compacts).

Measured effect (baseline → after, `wc -l` over `.purs`):

| Area | Before | After | Net |
| --- | --- | --- | --- |
| Library (`src` + `extras`) | 9280 | 9266 | **−14** |
| Demo view modules | — | — | **−15** |
| Demo logic modules | — | — | **+91** |
| Demo total | 6008 | 6084 | **+76** |

168 view-side read sites (115 `projection`, 34 `projected`, 19 `forCase`)
dissolved into ~30 `present<App>` normalizations and context-row
projection fields. The demos grew slightly on net — the migration is a
**shift, not a shrink**: formatting left the browser-related view
modules (−15) for the unit-testable logic modules (+91, where every row
is spelled structurally by the no-type-synonyms rule). The API surface
is where the compaction landed: display plumbing is now `forProperty` +
`forCases` with its derived `forCase @l`.

Execution notes (deviations the recipe absorbed): a copy line whose
model grew keeps its exact footprint by reading the derived field with
the emitter `# armed` (espresso-bar, product-review, loan-calculator);
a loop-free `# with` app computes derived fields in the seed only
(potluck); order-form's `# settled presentOrder` sits inside `# looped`
(the outer pipeline closes at `{} {}`). The `shownWhen`-from-`provided`
derivation turned out to already hold in the code (see the table above)
— the research item was stale on arrival.

## Principle

**A display shows a model field verbatim.** If the user sees a string,
that string is a field. Formatting — projecting a record or a field into
copy — is business logic: a `settled` invariant deriving presentation
fields from source fields, in the logic module.

The model thereby becomes explicitly a **presentation model**: source
values and their renderings coexist as sibling fields, the derived
field's label being the caption it draws (L3 unchanged — the label *is*
the copy: `text @"Total (€)"`).

This holds for **multi-field lines** exactly as for single-field
formats: a sentence composed from several sources
(`"Hello, [first name] [last name]"`, order-form's summary lines) is a
derived field written by a `settled` stage whose footprint is the
sentence's sources plus the line field. A sentence is a presentation-
model concern by decision, not a special case — it moves out of
browser-related code into the logic module, where it is one pure
function and one unit test.

## Motivation

1. **Testing.** Everything the user reads is a model field, so copy is
   unit-testable in `spago test`: a formatter is a pure
   `{ sources } -> { line }` function, and asserting the presentation
   model asserts the screen — no browser, no CDP. Smoke shrinks toward
   carrier laws and wiring only.
2. **One home for formatting.** Today a formatter can live in a
   `projection` bracket (view) or a `settled` body (model);
   temperature-converter already chose the model. This makes that
   choice the law.
3. **API compaction.** The display-side plumbing drops from five words
   to two.

## Laws

- **Verbatim-display law**: a value display never brackets a formatter;
  its field arrives ready to draw.
- **Presentation invariant**: each formatter runs as
  `# settled format` where `format` writes only derived fields as
  functions of source fields and leaves sources untouched — idempotent
  by construction, satisfying `settled`'s normalization law. Footprints
  stay exact closed rows (`Union small rest big` subsumption
  unchanged).

## Vocabulary changes

| Change | Rationale |
| --- | --- |
| `projection`, `projected` leave the application vocabulary | formatting is no longer a view-side act; `projected` may survive as library plumbing for the statuses' internal reads, or dissolve there too |
| `forProperty` **stays** — the one read adopter | it is *selection*, not formatting: a verbatim field read from a context-pinned wider row (`foreach` item, `provided` pane payload) that `settled` cannot reach because the row is not the model's |
| `forCase @l` dissolves into `forCases` — **revised same day**: restored as a *derived* word (`forCase @l f = forCases { l: f }` by law), and `forCases` now takes the record of per-case copy functions directly (elimination is the mechanism's own) | the dissolution made the dominant single-case sites (17 of 19) longer and broke the `@l` adopter grammar; the `applied = updated (const f)` precedent admits a derived convenience — and the record form removes `match` from the status story entirely |
| the canonical `[ event :: String ]` row goes private | a status only ever appears behind its business classifier (`snackbar # forCases lineOf`); the last fixed display label stops being API |
| `shownWhen` as a derivation of `provided` — **already holds** (since 2026-08-27, "Visibility is case adoption"): `shownWhen f content = recordToRecord (provided @l f content) identity` | one visibility primitive (`provided`), the display rung derived as the pane owned-merged with the wire; `inCase` is the honest primitive remainder — its content emits the *row*, which the owned merge's disjointness rejects, so it keeps a carrier body |

Context-row formatting is already logic-side by construction: it lands
in the collection's **projection argument** (`foreach @l proj`,
`listOf`'s, `shownEach`'s) — a business function — and the element view
only selects via `forProperty`.

## The resulting surface (data → user)

- **Leaves**: `text @l`, the progress family, `ratingDisplay`,
  `imagePane` — all verbatim.
- **Adopters**: `forProperty` (select from a context row), `forCases`
  (status + copy classifier). Nothing else.
- **Rungs & visibility**: `shown` / `shownWhen` / `shownEach` /
  `confirmed` / `observed`, `provided`.
- **In logic**: formatters as `settled` bodies, `caseText` for label
  read-back, collection projections carrying item formatting.

The adopter grammar becomes total and worth stating in writing.md:
`for-` = input-side adoption, `to-` = output-side, `at-` = unwrap,
`shown-` = gated rungs. After the deletions no display word sits
outside it.

## Worked sketch: tip-calculator

Before (view-side read):

    text @"Tip" # projection formatMoney

After: the model carries `"Tip (€)" :: String` beside the numeric
sources; the logic module's stage

    recomputeTip :: { bill :: String, percent :: Number, "Tip (€)" :: String }
                 -> { bill :: String, percent :: Number, "Tip (€)" :: String }

runs as `# settled recomputeTip`, and the view line is

    text @"Tip (€)"

The unit test asserts
`(recomputeTip { bill: "100", percent: 15.0, "Tip (€)": "" })."Tip (€)" == "15.00"` —
the screen's copy, in `spago test`.

A multi-field line is the same shape with a wider footprint:

    greetingLine :: { "First name" :: String, "Last name" :: String, greeting :: String }
                 -> { "First name" :: String, "Last name" :: String, greeting :: String }
    greetingLine r = r { greeting = "Hello, " <> r."First name" <> " " <> r."Last name" }

run as `# settled greetingLine`, shown as `text @"greeting"` — one pure
function, one unit test, no browser.

## Costs and open questions

- Models grow presentation fields; the source value and its rendering
  are siblings. Accepted as the point, not a leak.
- `settled` footprints widen: every formatted line's inputs join its
  sub-row.
- **Sentences in the model** — DECIDED (2026-08-31): multi-field lines
  are derived fields like any other; see Principle. The residual
  question is only layout, not law: if order-form's flat sentence
  fields crowd the row, group them in a presentation sub-record nested
  via `subStrong` — a readability refactor, not a retreat.
- Whether `forCases` fuses into the status leaves
  (`snackbar lineOf`) — rejected for now: adopter uniformity and the
  total grammar outweigh one saved `#`.

## Migration & verification

1. Library: deletions (`projection`, `projected`, `forCase` from the
   app-facing re-exports), the `shownWhen`/`provided` derivation
   research, header/doc updates.
2. Demos: per-demo sweep replacing `# projection f` / `# projected f`
   sites with presentation fields + `settled` stages (this reverses the
   2026-08-31 "single-field reads use projection" direction).
3. Mechanical guard: extend `npm run check-view-model` to reject
   `projection`/`projected` in demo modules, as it rejects stray
   `Maybe`/`Boolean` fields.
4. Docs (L-sync): writing.md gains "The presentation model" +
   the adopter grammar; vocabulary.md re-indexed; CLAUDE.md and
   demo/index.html's `#code-style` note re-read against writing.md.
5. L15 in full: `spago build`, `spago test`, `npm run bundle-demos`,
   `npm run smoke`.

## Admission test

Derivation: pure deletions plus one law — no new combinator. Laws:
verbatim display + the presentation invariant's idempotence. Subsumption:
unchanged (`settled` already subsumes). Honesty: the model *says* it is
a presentation model. Reachability: every surviving word stays
demo-reached; `forCase` would be unreached after migration, which is
the deletion argument restated. Green stack: step 5 above. Sync: step 4.
