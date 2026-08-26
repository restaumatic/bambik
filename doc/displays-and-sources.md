# Displays, sources, and the assurance ladder

The boundary of an application is where the flow meets the world. This
note names the citizens that live on that boundary, the laws that follow
from which side of it can hold state, and the business reading of the
types they carry. It consolidates what the no-silent-information-loss
work (guardrails L13) made enforceable — and, on this branch, what the
fulfillment-gated displays turned from contract into construction: a
display is a stage whose **release is the witness**.

## Being read is always the business's concern

A display exists because the business wants the user informed — there is
no "display the business doesn't care about." What varies between display
components is not the concern but the **fulfillment policy**: *how* the
component discharges the obligation that its content be read. Each policy
either produces a **witness** — an event establishing that reading
happened — or fulfills by trust. The type records exactly this and
nothing more:

> A `{}` output means **fulfilled without a witness** — assurance by
> visibility or salience. An emission means **the witness itself**, and
> the content-slot law (the gated displays' content slots and `observed`
> accept only `{}`) makes a witness impossible to discard silently:
> choosing a witnessing component is choosing to be unable to ignore its
> answer. In the gated family the witness is the **release**: `confirmed`
> withholds the flow until the user answers; `shown`/`told`/`shownAlways`
> release instantly (the zero-certainty rung); the pane and collection
> rungs release always and render on relevance.

The vocabulary is an **assurance ladder**, and it matches Material's own
interruption ranking (snackbar < banner < dialog):

| Policy | Fulfillment | Witness | Citizens |
| --- | --- | --- | --- |
| ambient | stay visible, always current | none | `text`/`progress` (in merges), `shown`/`told`/`shownAlways` (as stages) |
| salient-transient | interrupt attention, briefly | none | `snackbar`, `toast` |
| persistent-until-acted | remain until superseded or dismissed | the dismiss | `banner`, `output` |
| modal-witnessed | block everything until confirmed | the confirm | `dialog`, `simpleDialog` — open on feed, close on emission |

Escalating assurance is a business decision made by **choosing the
component**; routing by assurance is its composite form — cashbox is the
worked example: outgoing money detours through confirmation dialogs (a
witness demanded before money leaves), incoming money posts straight to
the fold (ambient), and `subChoice` is the router. One rung is currently
empty: **ambient-with-witness** — a display that fulfills by visibility
but reports (via an `IntersectionObserver`-style sensor) that it was
actually seen. Nothing occupies it; the ladder predicts it.

## The boundary corners

`{}` is the terminal record (one value, zero information) and the unit of
the output-row union, which fixes both how displays compose and where the
world's writes enter:

| | entity (`×`) | event (`+`) |
| --- | --- | --- |
| **flow → world** (sink) | `p { \| r } {}` — displays | `p [ \| e ] {}` — statuses |
| **world → flow** (source) | `p {} { \| r }` — seed, sensor | occurrences — emitters |

Two structural facts follow from terminality, both already load-bearing:

- **A map to `1` cannot be a pipeline stage** (everything after it would
  receive the unique `{}`), so a mid-pipeline display is integrated by
  the merge's unit law — the display beside the wire,
  `recordToRecord w identity`, which is what the gated displays' bodies
  are (the standalone `tapped` combinator this construction once named is
  deleted: the rungs carry it). Dually, a map from `1` ignores everything
  before it, so sources sit at pipeline heads — which is what
  `body :: PUI Web {} o` says about the app itself: an application
  begins at `1` (`mvu` = source, then loop).
- **Why the display-beside-the-wire is derived but `seeded` is primitive**: a display's `{}`
  output is disjoint from every row, so it merges beside the wire for
  free; a source's output row collides with the wire's own, so the
  pass-through-plus-emission wire cannot be a merge and must be carrier
  structure (`Seeding`).

## Sources: the seed, generalized

The seed is the world's statement at t = 0; the rest of the source family
is the world's statements thereafter, graded by who writes and whether a
current value can be *polled*:

| Source | Who writes | When | Ontology | Form |
| --- | --- | --- | --- | --- |
| seed | the program | t = 0 | entity | `with`/`mvu`/`announce`, `Seeding` |
| sensor | a machine | t > 0 | entity | `p {} { \| r }` — pollable, so a lawful record channel (clock, `matchMedia`, online/offline; not yet in the vocabulary) |
| editor's write half | the user | t > 0 | entity draft | fused into editors; the focus guard arbitrates between the display half and the write half |
| emitter | the user | t > 0 | event | `button`/`clicked` — **no seed possible**: events occur, they don't pre-exist (the `×→+` unit is silent; `iterate` takes no seed) |
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
  click delivers (`clicked`, `armed`, the form-then-buttons compass walk);
- **temporal** — quiescence: `Resolving` derives the branch from time
  (emissions loop while the component is still moving, the last resolves
  at quiescence — `coresolve (resolve g) = debounced g`);
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
> policy for fulfilling it.** The type records the policy's evidence:
> `{}` for unwitnessed fulfillment, an emission for a witness — and the
> tap law permits discarding only the unwitnessed.

> **The user enters the model only as occurrences; editors prepare,
> events commit** — explicitly, by quiescence, or per-tick — **and only
> machines and the program write entities directly.**
