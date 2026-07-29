# Entities have initial state, events occur: pointedness in the row algebra

Design note. Status: **implemented** — see "What was implemented" at the end
for the deltas between the proposal below and the landed API.

## The fact to model

An **entity** (a record, `{ ... }`) does not come into existence when its
first field is edited — it exists from the very beginning, with a known
initial state. An **event** (a variant, `[ ... ]`) is the opposite: it has
no value at registration; occurrences arrive at some times, or never.

In FRP vocabulary: a record channel is a *behavior* (a total signal —
defined at every instant, including t=0), a variant channel is an *event
stream* (a discrete, partial occurrence relation). Behaviors are pointed
(`pure` exists — the constant signal); events are not (`Event` has no
`pure`, only `never` and merge).

So when we write `p ... { ... }` the record type ought to carry the
knowledge "this has a known initial value, available from the start" —
today it doesn't, and the gap shows up as the starvation problem.

## What the library already says

The asymmetry is **already in the algebra, at the units**:

- `RecordToRecord.pempty` **announces** its informationless `{}` once at
  registration — the record-output unit is pointed, necessarily: `{}`'s
  initial value is derivable from its type (it has no fields to know).
- `RecordToVariant.pempty` (and the variant-output side generally) is
  `silence` — the event unit is *never*, also necessarily: there is no
  canonical first occurrence.

`announce` is exactly the behavior applicative's `pure` generalized to a
payload ("the fold announces its initial state the way `pempty` announces
its informationless `{}`" — its own doc already says this), `constantly`
is the constant behavior, `silence` is the event `never`. The units
witness the FRP pointedness facts precisely.

What the library does **not** say is anything about non-unit records. A
`PUI m { count :: Int } o` channel is fed like any other; if nobody feeds
it, gates starve, and we recover the fact "entities have initial state"
socially: the `with`/`announce`/`seeded` conventions, the `mvu seed`
shape, and — as a confession that the obligation is invisible — the 3s
starvation watchdog, whose warning text literally instructs the developer
to supply the initial state the types never asked for.

## Where can an initial value come from? (the design space)

### A. From the type: `class Initial a where initial :: a` — rejected

Derive record instances field-wise, constrain `body`/`looped`/the gated
combinators with `Initial i`, and every record channel is pointed by
construction.

Rejected because initial state is **business data, not type structure**.
The 7GUIs timer starts at `duration: 15.0`, not `0.0`; an order form
starts with a chosen default payment method, not `""`. A `Default`-style
class silently invents *wrong values with the right types* — the exact
failure the knowledge gates exist to prevent (nothing flows until it is
genuinely known). And it is not even total: `select` is type-changing
`{ value :: Maybe a } → { value :: a }` precisely *because* its output has
no initial — a lawful `Initial { value :: a }` for abstract `a` does not
exist.

### B. From the leaves: widgets announce their rendered initial — rejected

Every ×→× editor does render *something* at registration (empty text
field, unchecked checkbox, slider at min). Let each leaf announce its
rendered state, let merges union announcements, and no ×→× gate ever
starves.

Rejected for three reasons:

1. The union of leaf-initials is a **bogus first model** — the all-empty
   order form — and it would *flow*: into `updates` folds, into `tapped`
   summaries, into action stages. Today's semantics ("nothing flows until
   known") is the honest one; this proposal replaces silence with noise.
2. The counterexample again: `select`/`radioButton`/`segmentedButton`
   outputs are record-typed but **event-natured** — the first occurrence
   is the first user pick. Not every record channel is a behavior.
3. It destroys the knowledge gate as UX. potluck's menu summary being
   *withheld until every guest has chosen* is the demo's whole point;
   auto-announcing leaves would open every gate at registration.

Point 2 is worth keeping as a slogan: **the carrier tells you the shape
axis (all-at-once vs one-at-a-time), not the time axis (defined-at-0 vs
occurring)**. Records are the natural home of pointed channels, but
pointedness itself is knowledge, and knowledge is per-channel, supplied by
terms.

### C. In the core type: an `initial` slot in the wiring — rejected

Change the carrier to `m { initial :: Maybe i, toUser, fromUser }` (or
registration-takes-the-initial, `i -> m {...}`). Record citizens return
`Just`, variant citizens `Nothing`; merges combine initials; `body`
demands one.

Rejected as re-encoding without enforcement: the `Maybe` reintroduces at
the wiring level exactly the optionality we are trying to eliminate at the
type level, while `i -> m {...}` breaks `compose` (registering downstream
needs an initial `o` from upstream, which needs an initial input, …) —
you end up back at `Maybe`. Everything this buys, `announce` already does
as a term, more compositionally.

### D. A polarity index on the hom — redundant

`PUI m (k1 :: Polarity) (k2 :: Polarity) i o`, FRP-style `Behavior`/
`Event`-sorted arrows. Redundant: **Record vs Variant already is the
polarity index** — that is the row layer's founding decision. The four
direction classes are the four hom-sets; adding a kind index would state
the same thing twice and disagree with itself at the type-changing
selectors (record-carried, event-natured).

### E. The input type *is* the obligation; force its discharge — proposed

No new machinery. Read a pipeline's **residual input row as its
initial-state obligation**: `PUI m { count :: Int } o` means "this UI
still needs to be told the count, starting from t=0". Then:

- `{}` is the **unique self-pointed record** — the only row whose initial
  value is derivable from its type. That is the content of the
  `pempty`-announces law.
- Supplying an initial state is **discharging the obligation**:
  `announce seed >>> w` (or `mvu seed w`) turns `PUI m model o` into
  `PUI m {} o`. The seed is a term, as it must be (business data).
- An app is **closed** iff its obligation is discharged — iff its input
  type is `{}`.

The fact "entities have known initial state, available from the very
beginning" becomes a typing judgment: **`p {} o` means everything needed
at t=0 has been supplied**. What is missing today is only that nothing
*demands* this judgment anywhere — `body :: forall i o. PUI Web i o ->
Effect Unit` happily mounts an open pipeline and feeds it nothing, and
the watchdog picks up the pieces 3 seconds later.

## Proposal

Two API changes and one documentation stance; all compile-time, no
semantics change.

### 1. `body` demands a closed app

```purescript
body :: forall o. PUI Web {} o -> Effect Unit
```

Forgetting the seed stops being a blank screen with a console warning and
becomes a type error *at the mount point*, whose shape ("expected `{}`,
got `{ count :: Int }`") names exactly the fields whose initial state was
never supplied — the same information the ×→× starvation watchdog prints
today, three seconds earlier and in the compiler. Events need no change:
no app has a variant at the very top (occurrences originate inside, from
users and timers), so `{}`-input is not a restriction on event flow.

Migration is mechanical: every demo already ends in `mvu seed`/`with
initial`/`with {}` (demo/1's `# with unit` becomes `# with {}` after
retyping its entry, or gets an `lcmap`); helloworld's `staticText` is
already `{} → {}`.

### 2. `mvu` (and friends) discharge, not just feed

```purescript
mvu :: forall m model. Applicative m => model -> PUI m model model -> PUI m {} model
mvu seed w = announce seed >>> looped w
```

Behaviorally equivalent to today's `looped (with seed w)` (the seed still
arrives once at registration, before any occurrence; re-feeds still pass
through the loop), but the result type now *records that the obligation
is gone*. `with` keeps its pass-through type `a -> PUI m a b -> PUI m a b`
— it is a re-feedable stage, useful mid-pipeline; the discharging idiom is
`announce seed >>> w`, which could be named (`primed seed w`) if demos
want it outside `mvu`.

### 3. Knot-tying combinators carry their seed

A cyclic record channel is an entity par excellence — it has state over
time — so **every combinator that ties a knot over a record channel
should take that channel's t=0 value as an argument**, rather than hoping
a `seeded`/`announce` was placed inside the chain:

```purescript
feedback  :: { | state } -> ...   -- today: primed by the widget's first emission, watchdog-guarded
folding   :: @w -> { | state } -> ...  -- today: `announce`-primed by convention (checkout)
unfolding :: @w -> { | state } -> ...  -- today: resumed via its case, seeded by convention
```

This is the same move as `mvu seed` applied to the trace quartet's row
forms: the doc already states "each co-strength is its strength's
retraction *once the state channel is primed*" — priming is a
precondition, so make it a parameter. The three watchdog messages for
`unfirst`/`coresolve`/`retain` ("the state channel was never primed —
seed it with `with`/`announce`/`seeded`") each become an argument the
developer cannot omit. Where a genuinely emission-primed form is wanted
(auction's high-water-mark seeding itself from the slider's first echo is
defensible), it can survive under a longer name — the default spelling
should be the total one.

### What stays unpointed, deliberately

- **Merge sibling gates.** A ×→× merge operand waiting for its sibling is
  not an un-supplied initial state — it is the knowledge semantics doing
  its job, and sometimes the UX itself (potluck's gather gate). Once the
  entry is closed (change 1), the seed reaches every operand at
  registration anyway; the gates that remain closed after that are closed
  for a *reason* (an `acted` element that hasn't spoken, an event that
  hasn't occurred).
- **Variant channels.** Events never need priming; their unit is
  `silence`, and that is the theorem, not the bug.
- **Record-carried event-natured outputs** (`select` before first pick,
  `acted`'s withheld aggregate). Pointedness is not imposed by carrier —
  it is claimed where an entity is closed (the entry, the knots) and
  nowhere else.

## Summary

| | entity (Record, ×) | event (Variant, +) |
|---|---|---|
| time semantics | behavior: total signal, defined at t=0 | occurrences at some times |
| pointed? | yes — but the point is a **term** (business data), not derivable from the type (except `{}`) | no — no canonical first occurrence |
| unit's protocol | `pempty` announces `{}` | `pempty` is `silence` |
| `pure` | `announce` / `constantly` | does not exist (parametricity: `forall o. p {} o` at variant output can only be silent) |
| where the initial is supplied | at the entry (`mvu seed`, `announce seed >>> w`) and at every knot (`feedback`/`folding`/`unfolding` seeds) | never |
| how the obligation is tracked | the residual input row; discharged ⇔ input is `{}` | — |
| enforcement (proposed) | `body :: PUI Web {} o -> Effect Unit`; seeds as arguments to knot-tying combinators | already forced by parametricity |

The model, in one sentence: **the record/variant carrier already
distinguishes entities from events; "entities have known initial state"
is not a new type index but the discipline that a record input row is an
initial-state obligation, `{}` is the only self-discharging one, and the
mount point and every knot must demand discharge — turning the starvation
watchdog's runtime advice into compile-time signatures.**

## What was implemented

All three changes landed, with these deltas from the proposal above:

1. **`body :: forall o. PUI Web {} o -> Effect Unit`** — as proposed.
   Signature-only: `body` still feeds nothing (a lawful `{}`-input widget
   needs nothing — the units announce).
2. **`with` itself became the discharge form** rather than gaining a
   `primed` sibling: `with :: a -> PUI m a b -> PUI m {} b`,
   `with a w = announce a >>> w`. Survey showed every `with` in the tree
   was discharge-intent (app entries); the pass-through seeding stage
   remains spelled `seeded a >>> w`. `mvu seed w = with seed (looped w)
   :: PUI m {} model`.
3. **The seeds are supplied through a new one-method class**,
   `Data.Profunctor.Seeding` (`class Category p <= Seeding p where
   seeded :: a -> p a a` — the pointed wire: identity plus one
   registration emission; no `(->)` instance, since a timeless carrier has
   no registration moment). The knot-tying row forms take their t=0 value
   and prime by *composing a `seeded` into the traced chain* — no carrier
   surgery, no change to `Costrong`/`Coresolving`/`Coretaining`:
   * `folding @w :: {  | fb } -> p { | iw } [ | ow ] -> p { | i } [ | done ]`
     — the seed is emitted once as case `w` on the output side
     (`g >>> seeded (inj @w fb0)`), exactly checkout's dissolved
     `announce cartStep` operand;
   * `unfolding @w :: { | fb } -> p [ | iw ] { | ow } -> p [ | i ] { | o }`
     — the seed is fed once as case `w` on the input side
     (`seeded (inj @w fb0) >>> g`), exactly ticket-dispenser's dissolved
     `seeded firstTicket` stage;
   * `feedback :: { | iw } -> p { | iw } { | ow } -> p { | i } { | o }` —
     the seed is the **whole inner input** (`seeded iw0 >>> g`), not just
     `{ | fb }`: the loop re-enters ×-joined with the input, so the
     chain's t=0 value is the join (state-only injection would need a
     seeded `unfirst` on the carrier — `Costrong` is an ecosystem class);
     this is exactly auction's dissolved `seeded noBids` stage.
   The raw optics (`colens`, `coshutter`, `coreel`) and class methods stay
   emission-primed and gated — they are the longer spelling for the
   exotica; their watchdogs now point at the seeded row forms.
4. **`iterate` deliberately takes no seed** — it is the control trace
   (`Cochoice`): events occur, they don't pre-exist.
