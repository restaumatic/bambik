# Why Bambik? A story of profunctor user interfaces

*A long-form introduction: why the library exists at all, and how its
vocabulary — directions, strengths and co-strengths, the nullary, unary and
binary operators, the `Category` instance — falls out of one idea taken
seriously.*

## Why at all?

Consider what a web application mostly *is*: a form over a data model. Values
flow from the model to the screen; the user edits, clicks, chooses; values
flow back. Everything else — routing, styling, animation — decorates this
round trip.

Now consider how mainstream approaches make you write it.

In the Elm/React family the round trip is cut in half. Rendering is a
function from state to view; user interaction is a separate stream of
messages; an update function reconciles the two. The model-to-screen
direction and the screen-to-model direction never appear as one thing, so
the thing you actually mean — *this text field edits the customer's name* —
is smeared across a view function, a message constructor, an update branch,
and often a lens. Change the model and you chase all four. The framework
then re-derives what changed by diffing virtual DOM trees, reconstructing at
runtime the correspondence you had in your head all along but were given no
way to write down.

Functional programming has a well-known remedy for "navigate a data
structure and update it in place": **optics**. A lens *is* the two halves of
the round trip in one value — get and set, read and write. And functional
programming also has a well-known abstraction for "a processing stage with
an input side and an output side": **arrows**, or more minimally,
**profunctors**. The observation Bambik is built on is that these are not
two remedies but one:

> Optics *are* profunctor transformers. A lens is exactly a thing that turns
> a `p field field` into a `p record record` — for any profunctor `p`.

So if a *UI component* is a profunctor — a value `PUI i o` that consumes model
values of type `i` and produces model values of type `o` — then optics stop
being a way to talk *about* your model and become a way to *assemble your
interface*. The lens that focuses the customer's name is the same value that
mounts a text field into the customer form. Data-structure navigation and
UI composition are one algebra.

That is the bet. The rest of the library is what you discover when you
refuse to add anything that doesn't come from that algebra — and it turns
out you need surprisingly little, and get back surprisingly much: no virtual
DOM, no global state store, no message plumbing, and a type discipline where
an application's architecture is readable off its types.

## The core type

```purescript
newtype PUI m i o = PUI (m
  { toUser   :: i -> Effect Unit
  , fromUser :: (o -> Effect Unit) -> Effect Unit
  })
```

A `PUI m i o` is a **wire with a face**: `toUser` pushes model values `i` at
it, `fromUser` registers a callback for the values `o` it emits. The `m` is
the monad that builds its face (for the DOM, `Web = StateT DOM Effect`).
Everything below is structure *on* this type, not machinery beside it.

## Directions: the compass

The input `i` and output `o` could be anything, but models worth having come
in exactly two shapes: a **record** (`×`, "all of these at once" — a form, a
settings page) and a **variant** (`+`, "one of these at a time" — an event,
a status, a wizard step). Two shapes on each side give **four directions**,
and each is its own module under `Data.Profunctor.Row`:

| direction | reading | typical citizen |
|---|---|---|
| `× → ×` | editor | text field, checkbox, slider |
| `× → +` | event | button, menu item |
| `+ → ×` | status | snackbar, banner |
| `+ → +` | dispatch | backend call routing |

Every Material Design component in `PUI/MDC2.purs` is a citizen of exactly one
direction: `filledTextField @l` is `×→×`, `button @l` is `×→+`,
`snackbar @l` is `+→×`. An application is a journey around the compass:

```
load → form (×→×) → live summary → buttons (×→+) → backend (+→+) → statuses (+→×)
```

## Binary operators: the merges

Each direction has one binary **merge** — how two UI components of that direction
sit side by side and become one:

- `recordToRecord` — two editors merge; inputs may overlap (everyone may
  *read* a field), outputs must be disjoint (every field has exactly one
  *producer*). The merge is **knowledge-gated**: when one operand emits its
  sub-record, the merge completes it with the other operand's last
  contribution — partial emissions become whole records, and nothing
  propagates until every field is known.
- `variantToVariant` — two dispatchers merge; inputs must be disjoint (every
  case has exactly one handler), outputs may overlap.
- `recordToVariant` — ungated broadcast: everyone sees the record, anyone
  may fire an event.
- `variantToRecord` — gated like `×→×`: a status arrives on one side, the
  merged output retains what the other side last said.

Look at the may-overlap/must-be-disjoint pattern for a moment, because it
is a law, not a coincidence: **sharing is inclusive, responsibility is
exclusive**. A record field may be *read* by everyone and a variant case
may be *emitted* by anyone — data can be copied freely, so those rows may
overlap. But a variant case must be *handled* by exactly one operand and a
record field must be *produced* by exactly one operand — responsibility
cannot be split, so those rows must be disjoint. Records are read-shared
but write-owned; variants are emit-shared but handle-owned: the two shapes
swap polarity as you cross from input to output, which is why there are
genuinely four directions and not two with a flip.

The library's constraint vocabulary spells this out. Each merge signature
carries exactly two constraints, one per side:

```
recordToRecord   : SharedRecordInputs + OwnedRecordOutputs
recordToVariant  : SharedRecordInputs + SharedVariantOutputs
variantToVariant : OwnedVariantInputs + SharedVariantOutputs
variantToRecord  : OwnedVariantInputs + OwnedRecordOutputs
```

The `Shared` sides need nothing beyond the type-level overlap bookkeeping —
their runtime actions (broadcast a record, `expand` a variant) are
label-blind. The `Owned` sides each carry **runtime evidence** alongside
disjointness, because their runtime actions are label-*driven*:
`OwnedVariantInputs` bundles `DispatchableVariants` (reified case tags, so
dispatch can route each value to its one handler) and `OwnedRecordOutputs`
bundles `MergeableRecords` (reified field names, so the gate can trim each
emission to exactly its declared row before combining contributions).
Evidence appears precisely where responsibility does — and the constraint
count is an honest price sheet: `recordToVariant`, with no owned side, is
the trivial broadcast; `variantToRecord`, doubly owned, both dispatches
and gates.

These merges are what the qualified-do sugar desugars to. A form is
literally:

```purescript
RecordToRecord.do
  filledTextField @"name"  { floatingLabel: "Name" }
  filledTextField @"email" { floatingLabel: "Email" }
```

each line one more operand merged in — and code order is DOM order. Note
there is no wrapping at the use site: every component carries its own
label (`filledTextField @l` is already the closed-singleton editor at
field `l`), so merge operands drop straight in. The general lifter
`field @l` exists for the other cases — turning a raw scalar leaf into a
singleton, or nesting a whole sub-composite as one field of a larger
record.

## Nullary operators: the units

A binary operator wants a unit, and each merge has one: `pempty`, the UI component
of type `{} → {}` — pure chrome. A divider, a heading, a spacer. It reads no
fields and produces none, so merging it in changes nothing about the model:
the monoid law, and the license to sprinkle decoration freely through a
form.

The subtlety worth savoring: units are not *silent*. `pempty` **announces**
— it emits its informationless `{}` once at registration, so the merge gates
never starve waiting for the side that has nothing to say. Parametricity
makes this unavoidable: anything typed `forall a b. p a b` can never
fabricate an output, so a lawful record-output unit must be per-carrier. The
general forms of this insight are the other nullary leaves: `silence` (truly
mute), `announce` (one registration-time emission of a constant — seeds
fields, primes loops), and `seeded` (an echo wire that first announces a
seed).

## Unary operators: strengths, or the small UI component in the big world

A text field edits a `String`; your model is a whole order. **Strengths**
are the unary operators that embed the small into the large — and this is
where optics re-enter, now as UI combinators:

- `Strong` (the `×` strength) gives `field @l` — the field lens, and the
  lift that makes every label-indexed editor a whole-row citizen;
  `subStrong` focuses a whole sub-record while the background row rides
  along.
- `Choice` (the `+` strength) gives `focusCase @l` — the case prism: handle one
  case, pass the others through.

Those cover the same-shape directions. The mixed directions need strengths
of their own, and here the library coins two:

- **`Resolving`**: `p a b -> p (Tuple a c) (Either b c)` — a UI component that
  sees everything but answers with a *decision*: `Left` done, `Right` keep
  going. One step of a loop. It underlies the `Shutter` optic. On `PUI` the
  decision is derived **from time**: emissions loop while the UI component is
  still moving (mid-typing, mid-drag), and the last emission of a burst
  resolves at quiescence. Note the values on the wire are just values — no
  hidden "transient" flag rides along; transiency is when a value arrives,
  not what it carries. (An earlier design *did* smuggle a continuity
  `Boolean` through every wire in a `New` wrapper; deriving the branch from
  time dissolved it.)
- **`Retaining`**: `p a b -> p (Either a c) (Tuple b c)` — a UI component that
  receives one case at a time but always *remembers* the rest: a
  Mealy/coroutine step. It underlies the `Reel` optic. Tellingly, `(->)` has
  no `Retaining` instance — a stateless function has nowhere to keep the
  memory. Retention is where UI genuinely outgrows pure functions.

## Co-strengths: tying the knot

Every strength opens a channel; every **co-strength is its retraction** — it
ties the channel back into a loop, with the law `co (strength g) ≅ g` once
the channel is primed. This is the **trace quartet**:

| strength | co-strength | ties into | co-optic | row form | example |
|---|---|---|---|---|---|
| `Strong` | `Costrong` / `unfirst` | state feedback | `Colens` | `feedback` | a session-peak readout chasing its own output |
| `Choice` | `Cochoice` / `unleft` | iteration | `Coprism` | `iterate` | retrying a flaky publish with attempt+1 |
| `Resolving` | `Coresolving` / `coresolve` | terminating fold | `Coshutter` | `folding @w` | an accumulating multi-step wizard |
| `Retaining` | `Coretaining` / `coretain` | productive unfold | `Coreel` | `unfolding @w` | an activity meter counting every event |

And just as each strength induces an optic (`Strong` the lens, `Choice`
the prism, the coined pair `Shutter` and `Reel`), each co-strength induces
a **co-optic** — and it is the corresponding optic *run backwards*:
`Colens s t a b ≅ Lens b a t s`, `Coprism ≅ Prism` reversed, and the mixed
pair swap partners under reversal — `Coshutter` is the reversed `Reel`,
`Coreel` the reversed `Shutter`. Where a lens carries its residual visibly
in the type (the background rides along), a co-optic *hides* its residual
and threads it through the loop instead: `Colens` reads each input against
the UI component's own last output; `Coprism` is `tailRec` at the optic level
(every result exits or re-enters as the next focus); `Coshutter`'s fold
state is a *reader* — its collapsed form `b → Either t (s → a)` has no
initial reader, which is exactly why the carrier gates inputs until
primed; `Coreel` is a generator, every emission both leaving and resuming.
Strength optics give compositional *access*; co-optics give compositional
*hiding* — a stage's private state has no footprint in the pipeline's
types.

The retraction law pays an unexpected dividend on the `Resolving` row:
compose the time-driven quiescence step with its co-strength and you get
`coresolve (resolve g) = debounced g` — **debouncing is a theorem**, the
`×→+` strength tied by its own retraction, not a gadget bolted on beside
the algebra. (`debounced` ships as a directly-implemented combinator with
this identity stated in its docs.)

All four loops are knowledge-gated: they will not spin until something
primes the state channel — which is exactly what `announce` and `seeded`
exist for. And one loop resists derivation entirely: `looped`, the
`×`-diagonal **self-trace** that feeds a UI component its own emissions
(re-entrancy-guarded), primitive because a gated `unfirst` cannot self-feed
— no state before the first emission, no emission before the first input.
Wrapped around a record merge, `looped` gives the operands *cross-feed*:
a tab bar and its panes become each other's audience, per-operand retention
falling out of the merge gates. The library once had bespoke `synced` and
`latch` combinators for this; they dissolved into `looped` — the algebra
subsumed them, which is how you know a design is converging.

## The `Category` instance: the spine

Finally the composition everything hangs on: `PUI m` is a `Semigroupoid` and
`Category`. `ui1 >>> ui2` pipes one UI component's output into the next UI component's
input, and `Semigroupoid.do` pipelines read top-to-bottom like the user's
journey through the app — the compass walk from the Directions section is a
single composite wire.

And `identity`? It is not a no-op. It is the **echo wire**: whatever comes
in goes straight back out. Give it a seed and it becomes `seeded`; wrap it
in `retain` and it becomes a counter's heartbeat. Even the unit of
composition is a live UI component — which is perhaps the neatest summary of the
whole design: in Bambik there is no boundary where "the algebra" ends and
"the UI" begins.

## The story in one line

Four **directions** give the map; the binary **merges** lay UI components side by
side; the nullary **units** decorate and prime; the unary **strengths** let
small UI components inhabit big models; the **co-strengths** tie every open
channel into a living loop; and the **`Category`** instance strings the
whole journey into one wire from page load to snackbar.

*See also: the [20-minute presentation](presentation-20min.md), and
[the guardrails](guardrails.md) — this story stated as strict rules.*
