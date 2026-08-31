# Thought: output leaves and output data plumbing

*A briefing on the presentation direction — data shown through the UI to
the user. In `PUI m i o` terms this is the **input side**: the `toUser`
channel. The library's word for these components is **displays**.
Editors (`×→×`) are out of scope; so are emitters (`×→+`), which present
nothing — their caption is static and their fed row is replay
ammunition.*

*Updated 2026-08-31 for the presentation-model change (guardrails L17,
doc/research-presentation-model.md); the pre-change inventory this note
used to hold is what that proposal compacted.*

*Every display obeys the same **per-feed display echo** protocol: a
stateless renderer of the row it was last fed. There is no
stateful/stateless axis in the family — where state genuinely lives
(merge gates, the keyed reconciler, a snackbar's dismiss timer) it is
pipeline or carrier machinery, never the display leaf. The honest axes:
what feeds it (`×` value row vs. `+` occurrence), its payload type, and
— for the gated rungs — its release policy.*

## Output leaves — all verbatim (L17)

No display takes a formatter; each shows its field as fed.

| Leaf | Row | Where |
| --- | --- | --- |
| `text @l` | `Cons l String` | `PUI.Web.HTML` — the workhorse, also inside every MDC/`foreach` cell |
| `progress @l` | `Cons l Number` (fraction 0–1) | HTML, Bootstrap |
| `linearProgress @l` / `progressBar @l` | `Cons l Number` | MDC2, MDC3 / Shoelace, Fluent |
| `indeterminateLinearProgress @l`, `indeterminateCircularProgress @l` | `Cons l Boolean` (running/not) | MDC2, MDC3 |
| `ratingDisplay @l` | `Cons l Number` | Fluent — read-only stars |
| `imagePane` | fixed `{ src, label }` | MDC2 — the channel-fed gallery image |

**Statuses** — fed an occurrence (`+→×`): `snackbar` (MDC2/MDC3),
`banner` (MDC2 only — the honest MD3 gap), `toast` (Shoelace,
Bootstrap), `messageBar` (Fluent), HTML's `output`. Their canonical
`[ event :: String ]` row is **private to the vocabulary**: application
code meets a status only behind its `forCases` classifier.

**Presentation decorators**: `attrWith name f` (channel-fed attribute)
and `clWhen pred name` (value-dependent class) — why grids and canvases
update in place instead of rebuilding.

**Statics** — `staticText`, `staticHTML`, `static (ocular)`, void `hr`:
chrome at `{} → {}`, no model data.

## Output data plumbing

- `# forProperty` — the one read adopter: verbatim field **selection**
  from a context-pinned wider row (a collection item, a pane payload),
  arity-0, label derived from the leaf. It survived the compaction
  because it is selection, never formatting.
- `# forCase @l copyOf` — one status per business case, sibling merge
  operands each owning exactly their case. A **derived** word:
  `forCase @l f = forCases (match { l: f })` is its law — kept for the
  `@l` adopter grammar and the `match`-free call site, exactly as
  `applied` keeps `const` off `updated`'s.
- `# forCases classifier` — one status instance for a whole classified
  variant (flight-booker's `bookingLine`).

Deleted (2026-08-31): `projection` (entirely) and `projected` (demoted
to vocabulary-internal plumbing — the statuses'
`text @"line" # projected eventText`; not re-exported from `PUI`).
`npm run check-view-model` rejects both anywhere in demo code.

## Where the formatting went: the presentation model

The rows a pipeline operates over are a **presentation model** — source
fields beside the derived fields they render as. Per app:

- One normalization `present<App> :: row -> row` in the logic module
  writes every derived field (a formatted number, a unit-suffixed
  quantity, a composed sentence line) from its sources.
- It runs as `# settled present<App>` trailing the pipeline (inside the
  loop when the outer pipeline closes at `{} {}` — order-form), and the
  **seed is pre-normalized** (`seed = present<App> { …, tipText: "" }`)
  so the first feed already carries its copy. A loop-free `# with` app
  needs only the normalized seed (potluck).
- **Context rows get it one step earlier**: the business function
  *producing* the row carries the text field — a collection's projection
  argument, a classifier's case payload. The view line only selects.
- `caseText` (Data.Variant.Case) reads a variant's label back as copy
  inside those functions; status copy lines read derived fields so
  emitter footprints stay exact (`# armed` where the model grew).

The payoff: the screen's copy is a pure `row -> row` function under
`spago test` — asserting the presentation model asserts the screen, no
browser.

## Gated rungs and visibility

`shown` (ambient, releases always), `shownWhen @l f` (display pane),
`shownEach @l proj` (keyed collection), `confirmed` (modal witness),
`observed` (the `+`-side sibling), plus `provided @l f` (case-gated
existence). Content slots take only `{}`-output components (no silent
loss); `# muted` is the explicit discard. Recorded fact: **`shownWhen`
is a derivation of `provided`** —
`recordToRecord (provided @l f content) identity` — one visibility
primitive, the display rung as the pane owned-merged with the wire.
`inCase @l` is the honest primitive remainder and is the *editor* pane,
not a display.
