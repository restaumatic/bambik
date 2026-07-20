## Module PUI

The core profunctor UI type and its combinators.

**How to read an app.** An app is `mvu seed pipeline`: the pipeline's
stages are composed with `Semigroupoid.do`, every emission travels
left-to-right through the stages, and `mvu` loops the final emission
back to the top — so a stage placed *before* another is not "above" it
semantically; all stages see every model value on the next loop turn.

A trace of the 7GUIs counter (`display # completed`, then
`button # updates increment`, under `mvu { count: 0 }`):

 1. registration: the seed `{ count: 0 }` is fed to the first stage;
 2. the display shows `0` and echoes; `completed` widens the echo to
    the full model, which flows on and arms the button's replay value
    and `updates`' retained state;
 3. the user clicks: the button emits, `updates` folds `increment`
    into the retained model and emits `{ count: 1 }`;
 4. the loop re-feeds `{ count: 1 }` to the top; the display re-renders;
    the re-feed's own echoes are swallowed by the loop's re-entrancy
    guard, so exactly one turn happens per event.

#### `Action`

``` purescript
type Action s t a b = forall m. Functor m => Optic (PUI m) s t a b
```

#### `PropagationError`

``` purescript
type PropagationError = String
```

#### `PropagationStatus`

``` purescript
type PropagationStatus = Maybe PropagationError
```

#### `PUI`

``` purescript
newtype PUI m i o
  = PUI (m { fromUser :: (o -> Effect PropagationStatus) -> Effect Unit, toUser :: i -> Effect Unit })
```

##### Instances
``` purescript
Newtype (PUI m i o) _
(Functor m) => Profunctor (PUI m)
(Functor m) => Strong (PUI m)
(Functor m) => Choice (PUI m)
(Functor m) => Costrong (PUI m)
(Functor m) => Cochoice (PUI m)
(Functor m) => Coresolving (PUI m)
(Functor m) => Coretaining (PUI m)
(Apply m) => Semigroupoid (PUI m)
(Applicative m) => Category (PUI m)
(Applicative m) => RecordToRecord (PUI m)
(Applicative m) => RecordToVariant (PUI m)
(Functor m) => Resolving (PUI m)
(Functor m) => Retaining (PUI m)
(Applicative m) => VariantToRecord (PUI m)
(Applicative m) => VariantToVariant (PUI m)
```

#### `action`

``` purescript
action :: forall s t. (s -> Aff t) -> Action s t { busy :: Boolean } (Record ())
```

#### `action'`

``` purescript
action' :: forall a b i o m. Functor m => (i -> (a -> Effect Unit) -> (o -> Effect Unit) -> Aff Unit) -> Optic (PUI m) i o a b
```

#### `affAdapter`

``` purescript
affAdapter :: forall m a b s t. Apply m => m { post :: b -> Aff t, pre :: s -> Aff a } -> Optic (PUI m) s t a b
```

#### `announce`

``` purescript
announce :: forall m o. Applicative m => o -> PUI m (Record ()) o
```

The **announcing constant**: silent except for one emission of `o` at
registration — the value-level generalization of the record units'
`{}` announcement (`Web.staticText`'s protocol, with a payload). As a
merge operand it seeds fields or cases; in front of a knowledge-gated
trace (`folding`, `feedback`) it primes the state channel — the fold
announces its initial state the way `pempty` announces its
informationless `{}`.

#### `constant`

``` purescript
constant :: forall a s t m. Functor m => a -> Optic (PUI m) s t a Void
```

#### `constantly`

``` purescript
constantly :: forall m a i o. Functor m => a -> PUI m a o -> PUI m i o
```

Pin a stage's input to a known value: the wrapped widget is fed `a`
for every value flowing through, and the stage's own input type stays
free — so a constant-fed stage (a fixed catalogue driving a collection
component) needs no input-type annotation where `lcmap (const a)`
would.

#### `debounced`

``` purescript
debounced :: forall m. Applicative m => Ocular (PUI m)
```

#### `debounced'`

``` purescript
debounced' :: forall m. Applicative m => Milliseconds -> Ocular (PUI m)
```

Debounce the widget's *input* leg: each incoming value is delayed by
`millis`, and a newer value supersedes (kills) the pending one, so only
the last value of a burst reaches the widget. Rapid sources (keystrokes,
slider drags) emit every value; the stage that doesn't want the burst
opts in here.

Algebraically this is the `× → +` trace at the value level:
`debounced g ≅ coresolve (resolveFor millis g)` once primed — the
quiescence step composed with its retraction. Implemented directly
(ungated, on the input leg) as elsewhere laws are stated and bodies
stay lean.

#### `displayed`

``` purescript
displayed :: forall m s e. Functor m => PUI m s e -> PUI m s s
```

Make any display an **unconditional pass-through stage**: every value
fed is shown and forwarded, no echo required. The honest wrapper for
displays that cannot echo — `foreach`/`foreachWith` collections (silent on
an empty array, so inside a gated merge they starve the gate, and as a
`mvu` pipeline's last stage they kill the loop). `tapped` and `completed`
both rely on the display's echo;
`displayed` does not. (The trivial `updates` fold: any event the
wrapped widget does emit re-emits the retained value.)

#### `effAdapter`

``` purescript
effAdapter :: forall m a b s t. Apply m => m { post :: b -> Effect t, pre :: s -> Effect a } -> Optic (PUI m) s t a b
```

#### `every`

``` purescript
every :: forall m a. Applicative m => Milliseconds -> (a -> Maybe a) -> PUI m a a
```

The **heartbeat wire**: `identity`'s pass-through plus a periodic step.
Retains the last value flowing through; every `interval`, applies
`step` to it — `Just` advances (retained and emitted), `Nothing`
pauses until fresh input arrives. Inside a `looped` chain this is a
tick source: the 7GUIs Timer is `every (Milliseconds 100.0) tick`.
The loop runs for the widget's whole life (no cancellation — a
prototype limitation shared with `action'`).

#### `looped`

``` purescript
looped :: forall m a. Functor m => PUI m a a -> PUI m a a
```

The `×`-diagonal **self-trace**: feed a diagonal widget its own
emissions, re-entrancy-guarded (leaf widgets echo on `toUser`, and the
guard swallows the echoes the re-feed provokes). Wrapped around a record
merge it supplies the sibling cross-feed the gated merge deliberately
omits — every operand sees every emission re-broadcast, and per-operand
*retention* falls out of the merge gates (each gate holds its side's
last contribution). Primitive rather than derived: `Costrong`'s
`unfirst` cannot self-feed (no `c` before the first emission, no
emission before the first input — the gate deadlocks), so the
self-feeding special case ties the knot directly.

#### `muted`

``` purescript
muted :: forall m b i o. Functor m => PUI m (Record ()) b -> PUI m i o
```

Embed `{}`-typed chrome at ANY position: the wrapped widget is fed
`{}` for every value flowing through and its emissions (the statics'
registration announcement) are dropped, so static chrome fits a live
slot — `drawer config (muted staticNav) content` — without touching
the slot's types.

#### `mvu`

``` purescript
mvu :: forall m model. Applicative m => model -> PUI m model model -> PUI m model model
```

The model–view–update shape, named: `mvu seed w = looped (with seed w)`.
`w` is a same-type pipeline over the model — editors (`# completed`
where they don't produce the whole model), displays, wires (`every`),
and event stages folded in with `updates`. The seed primes the loop at
registration; from then on every emission of any stage re-enters at
the top, re-entrancy-guarded. The standalone app reads
`body $ ... $ mvu seed pipeline`.

#### `onCase`

``` purescript
onCase :: forall @l p a b s. IsSymbol l => Cons l a () s => Profunctor p => p a b -> p (Variant s) b
```

The progress slot is row-shaped like every component interface: the
widget is a `{ busy :: Boolean } → {}` display citizen.
Adopt a bare-input widget as the owner of input case `l` inside a
`VariantToVariant.do` merge — `lcmap`-only, the input-side sibling of
`asCase`: `action createPerson # onCase @"create"`.

#### `resolveFor`

``` purescript
resolveFor :: forall m a b c. Functor m => Milliseconds -> PUI m a b -> PUI m (Tuple a c) (Either b c)
```

`resolve` with an explicit quiescence window — see the `Resolving`
instance. `Done` needs no state and fires (after the window) even
unprimed; only the `Loop` branch is gated on a first `c`.

#### `seeded`

``` purescript
seeded :: forall m a. Applicative m => a -> PUI m a a
```

The **seeded echo wire**: `identity`'s pass-through plus one emission
of the seed at registration. As the first stage of a knowledge-gated
trace's inner (`feedback`, `unfolding`), the seed emission flows into
the following stages, they render and emit, and the trace's state
channel is primed before any input arrives — `announce`'s job, at a
pass-through type.

#### `silence`

``` purescript
silence :: forall m i o. Applicative m => PUI m i o
```

The silent widget: shows nothing, captures nothing — at ANY types, and
necessarily so (parametricity: `forall i o. p i o` can neither inspect an
`i` nor fabricate an `o`). The pinned trivial operand of the mixed
introduce laws, the implementation of `pempty` at the variant-output
directions (where silence is forced), and the terminal sink of data-flow
pipelines.

Not primitive — the `dimap`-closure of the `× → +` unit (the one unit
with record input and variant output, so the one that repolarizes):

```
silence = dimap (const {}) case_ RecordToVariant.pempty
```

Implemented directly, as elsewhere laws are stated and bodies stay lean.

#### `spied`

``` purescript
spied :: forall m. Functor m => DebugWarning => String -> Ocular (PUI m)
```

#### `updates`

``` purescript
updates :: forall m s e. Functor m => (e -> s -> s) -> PUI m s e -> PUI m s s
```

The **Mealy update stage** on the `×`-diagonal: a pass-through wire
(every value fed flows on, so ticks and edits upstream keep driving
the loop) that retains the last value and, on each *event* emission of
the wrapped widget, folds it in and emits the updated value. Event
widgets emit **bare payloads** — no smuggling the model through event
cases, no pass-through `state` case in the event merge:

```
looped Semigroupoid.do
  form                                   -- ×→× editors
  updates handle RecordToVariant.do ...  -- ×→+ events, bare payloads
```

is the model–view–update shape as two named stages. Events arriving
before a first value are withheld (the usual knowledge gate).

#### `with`

``` purescript
with :: forall m a b. Applicative m => a -> PUI m a b -> PUI m a b
```

Seed any stage with an initial value: `with a w` feeds `w` the value
`a` once at registration, then behaves as `w` — `seeded`'s composition
closure (`with a w = seeded a >>> w`, so `with a identity = seeded a`).
Insertable at every `PUI m a b` position: in front of a form (the
initial model), around a merge operand (seeding just that operand's
gates), or in front of a knowledge-gated trace as its primer — and at
the app entry: `body` feeds nothing, so `body $ with initial $ ...`
is the standalone-app shape.


### Re-exported from Data.Profunctor.Row:

#### `widenRecordInput`

``` purescript
widenRecordInput :: forall p narrow extra wider o. Profunctor p => Union narrow extra wider => p (Record narrow) o -> p (Record wider) o
```

### Re-exported from Data.Profunctor.Row.RecordToRecord:

#### `tapped`

``` purescript
tapped :: forall p s x. Strong p => p s x -> p s s
```

A display **tap** on the `×`-diagonal: shows the value flowing through
and passes it on — the pipeline-stage form of a live view. Pure `Strong`
plus the leaf-echo protocol: `second` retains the value, and the
display's echo triggers the forwarding. Honest only over *displays*
(elements whose sole emission is the echo) — an editing widget inside
would replay the retained upstream value on every edit.

#### `required`

``` purescript
required :: forall p a b. Profunctor p => p { value :: Maybe a } b -> p { value :: a } b
```

Mark a type-changing selector (`{ value :: Maybe a } → { value :: a }`)
as **always selected**: the `Maybe` input exists for the unselected
display state, so when the model guarantees a selection it is vacuous —
every model value shows as chosen. Dissolves the
`dimap (\v -> { value: Just v }) _.value` bracket into a named stage:
`select config options # required # asField @l`.

#### `projection`

``` purescript
projection :: forall p a b o. Profunctor p => (a -> b) -> p { value :: b } o -> p a o
```

Feed a canonically-labeled component a **function of the whole input**:
`projection f` turns a `{ value :: b }` component into one fed a bare `a`,
with `f a` flowing in as its `value` — so `forValue` is exactly
`projection identity`, and formatted displays read `text # projection
readout` with no trailing `# forValue`. Composes straight into `forField`
(which now reads a field into a *bare*-value display): `text # projection
show # forField @l` formats field `l`. `lcmap`-only.

#### `forValue`

``` purescript
forValue :: forall p a b. Profunctor p => p { value :: a } b -> p a b
```

`property` at the **closed singleton row** — the merge-operand form:
nests a widget (or a whole sub-composite) as exactly one field of the
enclosing record, type-changing like `property` (`f' := f` recovers the
simple `p v v -> p { | r } { | r }` form). The pinned empty background
is what lets merge operands infer with no annotations — raw
`property`'s open background is ambiguous under the merges' `Union`.

With no background to carry it needs no strength — `dimap` suffices —
and its emissions are **runtime-exact** by construction: exactly the one
field, freshly built. (A lens emission (`property`) instead rebuilds the
record from its retained input, which under the merges' widening
coercions runtime-carries stale copies of *sibling* fields. The gated
merges guard against this — their `MergeableRecords` evidence trims every
operand emission to its declared output row before the left-biased
`Record.union` — so this is no longer a correctness obligation on
operands; `field` remains the preferred operand form for its
annotation-free inference.)
Adopt a canonically-labeled component for the **whole input**: what
flows in becomes its `value` — the verbatim display (`text # forValue`),
and `projection identity`. `projection f` is the formatting generalization
(`text # projection readout`); `forField @l` reads one field into such a
bare-value display.

#### `forField`

``` purescript
forField :: forall @l p a o r. IsSymbol l => Profunctor p => Lacks l () => Cons l a () r => p a o -> p (Record r) o
```

Read field `l` into a **bare-value** display — the display expecting a
plain `a`, as produced by `forValue`/`projection`: `text # projection show
# forField @l` formats field `l`, and `text # forValue # forField @l`
shows it verbatim. `lcmap`-only, the input-side member of the adopter
family (`asField` renames both sides of an editor). Closed singleton row:
annotation-free as a merge operand, and a display owns no output fields.

#### `focusRecord`

``` purescript
focusRecord :: forall p f f' b s s'. Strong p => ExclusiveRows f b s => ExclusiveRows f' b s' => p (Record f) (Record f') -> p (Record s) (Record s')
```

Row-typed `Strong`: focus a whole **sub-record** — the row-valued **focus**
`f` — transforming it against the **background** `b`, which is carried
unchanged. The **shot** `s` is refocused to `s'`. Operates on rows on
**both sides** — the argument is itself a `Record → Record` profunctor:

```
focusRecord :: p { | f } { | f' } -> p { | s } { | s' }
             -- where s = f ∪ b,  s' = f' ∪ b   (ExclusiveRows)
```

The labeled analogue of `Strong`'s `first`/`second`: instead of carrying a positional
complement `c`, it carries the background *row* `b`, split off by `ExclusiveRows`.
Plain `Strong` underneath: split `s` into `(f, b)`, run the argument on `f`
via `first`, and re-merge `f'` with `b`.

#### `field`

``` purescript
field :: forall @l p f f' si so. IsSymbol l => Profunctor p => Lacks l () => Cons l f () si => Cons l f' () so => p f f' -> p (Record si) (Record so)
```

#### `completed`

``` purescript
completed :: forall p n nx i o u ol. Strong p => Union n nx i => Union o i u => Nub u i => RowToList o ol => FieldNames ol o o => p (Record n) (Record o) -> p (Record i) (Record i)
```

**Complete** a widget's output to its full input row: fields the
widget doesn't produce are carried from the retained input, so a merge
of editors covering only part of the model needs no `field @l identity`
echo wires to close the loop. The emission is trimmed to its declared
row first (the `FieldNames` evidence), so the left-biased union is
runtime-exact — the same guarantee the merge gates give.

#### `asField`

``` purescript
asField :: forall @l p a b s t. IsSymbol l => Profunctor p => Lacks l () => Cons l a () s => Cons l b () t => p { value :: a } { value :: b } -> p (Record s) (Record t)
```

Adopt a **canonically-labeled** component (`{ value :: a }` in and out,
the citizenship-carrying scalar interface) as business field `l`: a pure
relabeling, `dimap`-only like `field` — merge-gate exactness untouched,
annotation-free as a merge operand (closed singleton rows on both sides).
`field` wraps its argument under `l`; `asField` renames `value` to `l`.

### Re-exported from Data.Profunctor.Row.RecordToVariant:

#### `toCase`

``` purescript
toCase :: forall @l p i a s. IsSymbol l => Cons l a () s => Profunctor p => p i a -> p i (Variant s)
```

Introduce a widget's **bare** output as case `l` — `recordToCase` freed
from the record-input constraint, at the **closed singleton row** (the
`field`/`echoCase` lesson: pinned empty background, so it infers with no
annotations). The output-side dual of `onCase` and the general sibling of
`asCase` (which renames the canonical `clicked` case):
`listOf {} item # rmap _.key # toCase @"picked"`.

#### `asCase`

``` purescript
asCase :: forall @l p i a s. IsSymbol l => Profunctor p => Cons l a () s => p i [ clicked :: a ] -> p i (Variant s)
```

The `× → +` member of the introduce family: the wrapped `p { | r } f` reads
the whole record — `r`, the **reality** the camera is pointed at, which
never enters the shot — and its result, the **focus**
`f`, is emitted as
output case `l`. This is the `introduceCase` that `VariantToVariant`
documents as impossible — there, a fresh output case must coexist with
gated pass-through cases and can never fire; here nothing else emits, the
computed case fires unconditionally, and no strength is needed at all:
plain `rmap (inj l)` on any `Profunctor`. (The **background** `b` of the
output **shot** `s` is simply never produced — the widening is free, as
with `inj` itself.)
Adopt a **canonically-labeled** event component (`[ clicked :: a ]` out,
the citizenship-carrying interface) as business case `l`: renames the
case, input untouched — `rmap`-only, the `asField` twin at `× → +`.

### Re-exported from Data.Profunctor.Row.VariantToRecord:

#### `forCase`

``` purescript
forCase :: forall @l p a o s. IsSymbol l => Profunctor p => Cons l a () s => p [ event :: a ] o -> p (Variant s) o
```

Adopt a **canonically-labeled** status component (`[ event :: a ]` in,
the citizenship-carrying interface) for business case `l`: renames the
incoming case, output untouched — `lcmap`-only, the `asCase` twin at
`+ → ×` (statuses receive; events emit).

