# Thought: output leaves and output data plumbing

*A briefing on the presentation direction — data shown through the UI to the
user. In `PUI m i o` terms this is the **input side**: the `toUser` channel.
The library's word for these components is **displays**. Editors (`×→×`) are
deliberately out of scope; so are emitters (`×→+`), which present nothing —
their caption is static and their fed row is replay ammunition.*

*Every display obeys the same **per-feed display echo** protocol: a stateless
renderer of the row it was last fed. There is no stateful/stateless axis in
the family — where state genuinely lives (merge gates retaining last
contributions, the keyed reconciler retaining element instances, a snackbar's
dismiss timer) it is pipeline or carrier machinery, never the display leaf.
The honest axes are: what feeds it (`×` value row vs. `+` occurrence), its
payload type, and — for the gated rungs — its release policy.*

## Output leaves

**Value displays** — fed a record, label-indexed, closed singleton row,
output `{}`:

| Leaf | Row | Where |
| --- | --- | --- |
| `text @l` | `Cons l String` | `PUI.Web.HTML` — the workhorse, also inside every MDC/`foreach` cell |
| `progress @l` | `Cons l Number` (fraction 0–1) | HTML, Bootstrap |
| `linearProgress @l` / `progressBar @l` | `Cons l Number` | MDC2, MDC3 / Shoelace, Fluent |
| `indeterminateLinearProgress @l`, `indeterminateCircularProgress @l` | `Cons l Boolean` (running/not — a Boolean-family allow-list case) | MDC2, MDC3 |
| `ratingDisplay @l` | `Cons l Number` | Fluent — read-only stars (no star editor in the catalogue, none invented) |
| `imagePane` | fixed `{ src, label }` | MDC2 — the channel-fed gallery image |

**Statuses** — fed an occurrence, canonical row `[ event :: String ] → {}`,
self-presenting copy for a user action's outcome: `snackbar` (MDC2/MDC3),
`banner` (MDC2 only — MD3 dropped it, the honest gap inbox shows), `toast`
(Shoelace, Bootstrap), `messageBar` (Fluent), and plain HTML's `output`
(shown in place, since HTML has nothing self-dismissing).

**Presentation decorators** (not leaves, but channel-fed presentation):
`attrWith name f` — value-computed attribute — and `clWhen pred name` —
value-dependent class. Both are why grids and canvases update in place
instead of rebuilding.

**Statics** — `staticText`, `staticHTML`, `static (ocular)`, void `hr`:
chrome at `{} → {}`, no model data. (Bootstrap's `badge` is an *ocular*, not
a leaf — it decorates the displays inside it.)

## Output data plumbing

**Read adopters** — bind a leaf's canonical row to the business model; each
derives the label from the leaf's closed singleton row, so the label is
stated once:

- `# projection f` — one-field read through a formatter, label preserved
  (the single-field idiom).
- `# projected f` — whole-value read into the leaf's field (how `output`
  itself is built: `text @"line" # projected eventText`).
- `# forProperty` — read one field of a wider, context-pinned row
  (collection item, pane payload): `text @"label" # forProperty`.
- `# forCase @l copyOf` / `# forCases (match {…})` — adopt a status for one
  business case, or one status instance for a whole classified variant
  (flight-booker's single snackbar for booked *and* rejected).
- `caseText` — read a variant case's label back verbatim as the copy it
  already is, never `match`-restated.

**Gated display rungs** — a display placed *in* the pipeline,
`p { o | rest } { o | rest }`: a pass-through whose release is the
fulfillment witness, gate policy baked into the component. `shown` (ambient,
releases always), `shownWhen @l f` (pane, attaches on relevance),
`shownEach @l proj` (keyed collection), `confirmed` (modal — withholds flow
until the user confirms), `observed` (the `+`-side sibling: narrates each
event as it passes). Content slots take only `{}`-output components (the
no-silent-loss law); `# muted` is the explicit discard when content
genuinely emits. `inCase @l` is *not* in this family — it is the editor
pane; `shownWhen` is its display counterpart.

**Conditional presentation** — `provided @l classifier`: case-gated
existence, content attached and fed the payload on case `l`, detached
otherwise. Never a `Maybe`; derived states get a variant-returning
classifier, so mutually exclusive views are exclusive by construction.

**Collection presentation** — retaining `foreach @l proj` (built once,
keyed, re-fed in place, identity following the key) for fixed structure over
changing values; `dynamic` / `each` (rebuild per feed) when structure itself
varies with the data.

**Freshness** comes from the enclosing `looped` ensemble's re-broadcast:
every display sees the row after every change, which is all that keeps
summaries and readouts live — no subscription machinery.
