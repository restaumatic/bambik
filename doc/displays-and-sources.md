# Displays, sources, and the assurance ladder

The boundary of an application is where the flow meets the world. This
note names the citizens that live on that boundary, the laws that follow
from which side of it can hold state, and the business reading of the
types they carry. It states what guardrails L13 (no silent information
loss) makes enforceable, and the construction behind it: a display is a
stage whose **release is the witness**.

## Being read is always the business's concern

A display exists because the business wants the user informed — there is
no "display the business doesn't care about." And what it informs with is
**vouched for**: a display takes its read function at the leaf
(`text balanceLine`), the function living in the view model module, because
copy is a function, not a field (guardrails L17,
doc/research-copy-is-a-function.md) — so the assurance a display gives is
assurance about a value a pure function computes from the model, and that
function is unit-tested in `spago test`. What varies between display
components is not the concern but the **fulfillment policy**: *how* the
component discharges the obligation that its content be read. Each policy
either produces a **witness** — an event establishing that reading
happened — or fulfills by trust. The release records exactly this and
nothing more:

> A display is a stage, `{ | r } → { | r }`, that releases the row it was
> fed. Releasing it **at once** is fulfillment **without a witness** —
> assurance by visibility or salience. **Withholding** the release until
> the user has answered makes the release **the witness itself**, and
> since everything downstream waits on it, a witness is impossible to
> discard silently: choosing a witnessing component is choosing to be
> unable to ignore its answer. `confirmed` withholds the flow until the
> user confirms; a plain display never withholds (the zero-certainty
> case); the pane (`inCase`) renders on relevance and answers always. A
> status, `[ | e ] → { | r }`, releases nothing — its reading is
> unwitnessed by type, so a witness can
> never be dropped there either.

The vocabulary is an **assurance ladder**, and it matches Material's own
interruption ranking (snackbar < banner < dialog):

| Policy | Fulfillment | Witness | Citizens |
| --- | --- | --- | --- |
| ambient | stay visible, always current | none | `text`/`textAt`, the progress bars (stages releasing the row they show); the indeterminate progress, ambient over an **occurrence** input — a status showing between `action`'s `started` and `ended` |
| salient-transient | interrupt attention, briefly | none | `snackbar`, `toast` |
| persistent-until-acted | remain until superseded or dismissed | the dismiss | `banner`, `output` |
| modal-witnessed | block everything until confirmed | the confirm | `confirmed @l` — the witness rung, the `+→×` leaf opened by its case and telling it, the flow withheld until the user confirms, the confirm folding; `dialog` — open on feed, close on emission |

Escalating assurance is a business decision made by **choosing the
component**; routing by assurance is its composite form — cashbox is the
worked example: outgoing money detours through a confirmation dialog (a
witness demanded before money leaves), each such button's case owned by
its `confirmed` in the fold block, while incoming money posts straight to
its fold (ambient). One rung is currently empty: **ambient-with-witness** —
a display that fulfills by visibility but reports (via an
`IntersectionObserver`-style sensor) that it was actually seen. Nothing
occupies it; the ladder predicts it.

## The boundary corners

`{}` is the terminal record (one value, zero information), which fixes
both how displays compose and where the world's writes enter:

| | entity (`×`) | event (`+`) |
| --- | --- | --- |
| **flow → world** (sink) | `p { \| r } { \| r }` — displays, releasing what they show | `p [ \| e ] { \| r }` — statuses, releasing nothing |
| **world → flow** (source) | `p {} { \| r }` — the seed; `p { \| r } { \| r1 }` — a platform source | occurrences — emitters |

Two structural facts follow from terminality, both load-bearing:

- **A map to `1` cannot be a pipeline stage** (everything after it would
  receive the unique `{}`). A display is a stage only because it releases
  its row (`displayWiring`: render, then echo the value); a status, which
  is such a map once instantiated at `{}`, is integrated beside the wire by
  the merge's unit law — `fold` is the status and the wire under the
  `×→×` merge. Dually, a
  map from `1` ignores everything before it, so sources sit at pipeline
  heads — which is what `body :: PUI Web {} o` says about the app itself:
  an application begins at `1` (`with` = source, then `looped`).
- **Why the status-beside-the-wire is derived but the point is primitive**:
  a status's `{}` output is disjoint from every row, so it merges beside
  the wire for free; a source's output row collides with the wire's own,
  so the pass-through-plus-emission wire cannot be a merge and must be
  carrier structure — `announce`, `PUI`'s point, from which the seeded
  echo wire `seeded` is derived through `Choice`.

## Sources: the seed, generalized

The seed is the world's statement at t = 0; the rest of the source family
is the world's statements thereafter, graded by who writes and whether a
current value can be *polled*:

| Source | Who writes | When | Ontology | Form |
| --- | --- | --- | --- | --- |
| seed | the program | t = 0 | entity | `with`/`announce` (`PUI`) |
| sensor | a machine | t > 0 | entity | `source @l now watch` (`PUI.Web`) and the facts over it — `clock`, `online`, `colorScheme`, `geolocated`, HTML's `filePicked`: pointed (the first value announced at registration), every feed answered with the last value known, each platform change an emission; a fact the platform may withhold is a variant with named cases, never a `Maybe` |
| editor's write half | the user | t > 0 | entity draft | fused into editors; the focus guard arbitrates between the display half and the write half |
| emitter | the user | t > 0 | event | `button`/`clicked` — **no seed possible**: events occur, they don't pre-exist (the `×→+` unit is the silent component) |
| `action` result | an effect | t > 0 | either | the Aff adapter |

The dividing law: **a record-channel source must answer "what is your
value now?"** Machines can be polled; a human can only be *asked*, and
asking is an event round-trip (that is what a dialog is). Hence entities
are written directly only by the program and by machines.

## User input is always an occurrence

The user enters the model only as events. An editor is not an
entity-source; it is a **preparation chamber** — a display of the draft
plus a convergence loop (a human cannot write state blind, so the widget
must show the partial result) — whose product reaches the model as an
occurrence. The library encodes the commit in three forms:

- **explicit** — the replay-last-value protocol: content prepares, the
  click delivers (`clicked`, the form-then-buttons compass walk);
- **temporal** — quiescence: `Resolving` derives the branch from time
  (emissions loop while the component is still moving, the last resolves
  at quiescence — `coresolve (resolve g >>> seeded (Right c0)) ≈
  debounced g`, the seeded retraction of observational-semantics.md §5);
- **per-tick** — `sliderLive` against `slider`: the degenerate commit
  where every intermediate value is a legitimate fact.

Bambik deliberately lets the model hold drafts (a text field's keystrokes
flow into the model row; the loop re-broadcasts them): single source of
truth is a design choice, and the draft/fact ontology surfaces as
protocols — arming, debounce, the focus guard — rather than as a typed
boundary. A stricter design could confine drafts to the editor and let
only occurrences touch the model; this library chose otherwise, knowingly.

## The two laws

> **Being read is always the business's concern; a display component is a
> policy for fulfilling it.** The release records the policy's evidence:
> released at once for unwitnessed fulfillment, withheld until the witness
> otherwise — and only the unwitnessed (a status, releasing nothing) may be
> discarded.

> **The user enters the model only as occurrences; editors prepare,
> events commit** — explicitly, by quiescence, or per-tick — **and only
> machines and the program write entities directly.**
