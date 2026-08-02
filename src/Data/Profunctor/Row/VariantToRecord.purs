-- | `Variant → Record` (+ → ×) row profunctors, organized (uniformly across
-- | the four direction modules) as:
-- |
-- |   * **strength** — `Retaining` (defined here; `PUI m` instances only, no
-- |     `(->)`): the unary power, a Mealy/coroutine step.
-- |   * **direction class** — `VariantToRecord`, the binary **merge**: the one
-- |     genuine per-carrier primitive.
-- |   * **free functions over the strength** — everything else: `reelWrap`
-- |     (sub-variant focus), `retainCase` (thread one label), `caseToProperty`
-- |     (single-case focus), `caseToRecord` (introduce/reduce), the `Reel`
-- |     optic with `reel`/`reelE` — and over the co-strength `Coretaining`:
-- |     the `Coreel` optic with `coreel`/`coreelE` (the reversed `Shutter`)
-- |     and its row form `unfolding @w` (the productive unfold).
-- |
-- | Law connecting the two classes: as in `RecordToVariant`, no `identity`
-- | crosses the modes, but a **silent sink** does — `p [ | b ] {}`, consuming
-- | any case and contributing no field (`PUI`'s parametric `silence` at that
-- | type; the unit `pempty` is its `b = ()` special case). The unary
-- | introduce operator is the **sink-pinned merge**,
-- |
-- | ```
-- | caseToRecord @l g = variantToRecord (lcmap unwrap g) silence
-- |   where unwrap :: [ l :: f ] -> f   -- eliminate the singleton variant
-- | ```
-- |
-- | with the cross-operand **retention** the merge machinery performs on
-- | non-`l` events supplied, in the free-function form, by `Retaining`.
-- |
-- | Completing the arity ladder downward, the **nullary** operator is the
-- | class's own unit `pempty :: p (Variant ()) {}` — the empty merge:
-- | `variantToRecord pempty g = g = variantToRecord g pempty`. It is a class
-- | member (not a parametric silent element like `silence`): a lawful
-- | record-output unit must *announce* its informationless `{}` so the merge
-- | knows that side is complete, and parametric silence cannot.
module Data.Profunctor.Row.VariantToRecord
  ( Coreel
  , Reel
  , bind
  , coreel
  , coreelE
  , variantToRecord
  , class Coretaining
  , class VariantToRecord
  , coretain
  , discard
  , pempty
  , caseToProperty
  , forCase
  , forCases
  , caseToRecord
  , class Retaining
  , retain
  , retainCase
  , reel
  , reelE
  , reelWrap
  , unfolding
  )
  where

import Control.Semigroupoid ((>>>))
import Data.Either (Either(..), either)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Row.VariantToVariant (splitVariant)
import Data.Profunctor.Seeding (class Seeding, seeded)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (Unit, unit)
import Data.Variant (class Contractable, Variant, case_, expand, inj, on)
import Prim.Row (class Cons, class Union)
import Record.Unsafe (unsafeSet)
import Type.Proxy (Proxy(..))
import Data.Profunctor.Row (class ExclusiveRows, class OwnedRecordOutputs, class OwnedVariantInputs)
import Unsafe.Coerce (unsafeCoerce)

-- | The **unary** sum→product strength for this direction: a **Mealy /
-- | coroutine step**, the dual of `RecordToVariant`'s `Resolving`. `retain`
-- | turns a transformer `p a b` into a step that consumes either a fresh input
-- | `a` or a resumed state `c`, emitting an output `b` together with the next
-- | state `c`:
-- |
-- | ```
-- | retain :: p a b -> p (Either a c) (Tuple b c)
-- |                        -- Left  a = fresh input
-- |                        -- Right c = resume from state
-- | ```
-- |
-- | State enters optionally (a branch of the sum input) and leaves guaranteed
-- | (product output), so the step *always* produces an output and the next
-- | state — a productive, stateful stream. Its binary, two-profunctor form is
-- | the `variantToRecord` merge below.
-- |
-- | There is deliberately **no `(->)` instance**: a stateless function has no
-- | `c` to place in the product on a fresh `Left a`, and no `b` on a `Right c`
-- | resume — the product output can't be filled without retaining state.
-- |
-- | This is the **bare strength** for the `+ → ×` direction (the analogue of
-- | `Strong`/`Choice`); the row combinator built on it is `reelWrap` below —
-- | exactly as `focusRecord` is built on `Strong`.
class Profunctor p <= Retaining p where
  retain :: forall a b c. p a b -> p (Either a c) (Tuple b c)

-- | The **co-strength** of `Retaining` — its retraction: where `retain`
-- | *adds* the resumable state channel `c`, `coretain` *ties* it. Every
-- | emission `Tuple b c` yields `b` and immediately re-enters the wrapped
-- | profunctor as a `Right c` resume — a **productive unfold**/generator:
-- | control loops back while output flows every step (the dual corner to
-- | `Coresolving`'s terminating fold in the trace quartet).
-- |
-- | Retraction law: `coretain (retain g) ≅ g` — once the state channel is
-- | primed (state must enter somewhere).
-- |
-- | (No `(->)` instance: tying a knot takes state.)
class Profunctor p <= Coretaining p where
  coretain :: forall a b c. p (Either a c) (Tuple b c) -> p a b

-- | `coretain` at row granularity — the **productive unfold** with labeled
-- | channels: the wrapped profunctor consumes either a fresh input case
-- | (any case of `i`) or a resume (case `w`, carrying the unfold state
-- | `{ | fb }`), and every emission's value fields `o` pass while its state
-- | fields `fb` immediately re-enter as case `w` — a generator. The
-- | `+ → ×` co-analogue of `reelWrap`. Like `feedback`, the output is
-- | split by coercion (`ExclusiveRows o fb ow` keeps the typed views
-- | disjoint), so the emitted `{ | o }` runtime-carries the state fields —
-- | an `unfolding` stage belongs in a pipeline, not as a merge operand.
-- |
-- | The unfold state is an **entity** — it exists from the unfold's very
-- | beginning — and `unfolding` takes its t=0 value `{ | fb }` as the
-- | first argument: at registration the seed is fed once as case `w` (a
-- | `seeded` wire composed onto the input), so a gated `retain` inside the
-- | chain is primed before the first fresh input arrives.
unfolding
  :: forall @w p i fb iw wx o ow
   . Seeding p
  => Coretaining p
  => IsSymbol w
  => Cons w { | fb } i iw
  => Union i wx iw
  => ExclusiveRows o fb ow
  => { | fb }
  -> p [ | iw ] { | ow }
  -> p [ | i ] { | o }
unfolding seed g =
  coretain
    (dimap
      (either expand (inj (Proxy @w)))
      (\ow -> Tuple (unsafeCoerce ow) (unsafeCoerce ow))
      (seeded (inj (Proxy @w) seed) >>> g))

-- | The optic `coretain` induces: the **Coreel** — the `Shutter` run
-- | backwards (`Coreel s t a b ≅ Shutter b a t s`). Eliminating the residual
-- | `c` (instantiated to `b`) by co-Yoneda collapses
-- | `∃c. (s + c → a) × (b → t × c)` to
-- | `(embed : s → a) × (out : b → t) × (resume : b → a)`: every emission
-- | both leaves as `t` and **re-enters as the next focus input** — a
-- | generator, producing on every step. `unfolding @w` is this optic at row
-- | granularity.
type Coreel s t a b = forall p. Coretaining p => p a b -> p s t

coreel :: forall s t a b. (s -> a) -> (b -> t) -> (b -> a) -> Coreel s t a b
coreel embed out resume = coreelE (either embed resume) (\b -> Tuple (out b) b)

-- | Construct a `Coreel` straight from its **existential encoding**
-- | `∃c. (s + c → a) × (b → t × c)`: pick the resume channel `c`, then supply
-- | `decon` (read a fresh input or a resumed value) and `recon` (split each
-- | emission into the output and the channel's next value). `coreel` is this
-- | at the co-Yoneda witness `c := b`.
coreelE :: forall s t a b c. (Either s c -> a) -> (b -> Tuple t c) -> Coreel s t a b
coreelE decon recon g = coretain (dimap decon recon g)

class Profunctor p <= VariantToRecord p where
  -- | One constraint per side: `OwnedVariantInputs` (disjoint rows — one
  -- | handler per case — plus `DispatchableVariants`, the runtime tags
  -- | dispatch routes by) and `OwnedRecordOutputs` (disjoint rows — one
  -- | producer per field — plus `MergeableRecords`, the runtime-exactness
  -- | trim; see `RecordToRecord.recordToRecord`). The doubly-owned merge:
  -- | it both dispatches and gates.
  variantToRecord :: forall i1 i1l i2 i2l o1 o2 i o o1l o2l.
    OwnedVariantInputs i1 i2 i i1l i2l =>
    OwnedRecordOutputs o1 o2 o o1l o2l =>
    p [ | i1 ] { | o1 } -> p [ | i2 ] { | o2 } -> p [ | i ] { | o }
  -- | The **nullary** merge — the unit: handles no cases, contributes no
  -- | fields. Genuinely per-carrier: the uninhabited input can never drive it,
  -- | yet a lawful record-output unit must still *announce* its
  -- | informationless `{}` so the merge machinery knows that side is complete
  -- | — which the parametric, necessarily-silent `silence` cannot do.
  pempty :: p (Variant ()) {}

bind :: forall p i1 i1l i2 i2l o1 o2 i o o1l o2l.
  VariantToRecord p =>
  OwnedVariantInputs i1 i2 i i1l i2l =>
  OwnedRecordOutputs o1 o2 o o1l o2l =>
  p [ | i1 ] { | o1 } -> (p [ | i1 ] { | o1 } -> p [ | i2 ] { | o2 }) -> p [ | i ] { | o }
bind first cont = variantToRecord first (cont first)

discard :: forall p i1 i1l i2 i2l o1 o2 i o o1l o2l.
  VariantToRecord p =>
  OwnedVariantInputs i1 i2 i i1l i2l =>
  OwnedRecordOutputs o1 o2 o o1l o2l =>
  p [ | i1 ] { | o1 } -> (Unit -> p [ | i2 ] { | o2 }) -> p [ | i ] { | o }
discard first cont = bind first (\_ -> cont unit)

-- | Adopt a **canonically-labeled** status component (`[ event :: a ]` in,
-- | the citizenship-carrying interface) for business case `l`: renames the
-- | incoming case, output untouched — `lcmap`-only, the `asCase` twin at
-- | `+ → ×` (statuses receive; events emit).
-- | The copy formatter is the mechanism's own argument (import-tower rule
-- | L16): the adopted case carries the bare business payload, and `f`
-- | renders it into the canonical `event` payload at the adoption site —
-- | `snackbar # forCase @"registered" welcomeLine`; `identity` when the
-- | payload already is the copy.
forCase :: forall @l p a b o s. IsSymbol l => Profunctor p => Cons l b () s => (b -> a) -> p [ event :: a ] o -> p [ | s ] o
forCase f = lcmap (on (Proxy @l) (\b -> inj (Proxy @"event") (f b)) case_)

-- | Adopt a status component for a **whole classified variant** — the input
-- | dual of `RecordToVariant`'s `toCases` and `forCase`'s plural: where
-- | `forCase @l` renders one business case into the canonical `event`
-- | payload, `forCases` renders every case through one copy classifier, so
-- | a single status instance serves mutually exclusive outcomes —
-- | `snackbar # forCases (match { booked: …, rejected: … })`. One-at-a-time
-- | input means one classifier is total over the row; per-case copy stays
-- | a `match` branch, never a sibling widget, when the outcomes share one
-- | status area.
forCases :: forall p a o s. Profunctor p => (Variant s -> a) -> p [ event :: a ] o -> p [ | s ] o
forCases f = lcmap (\v -> inj (Proxy @"event") (f v))

-- | Single-case specialization of `retain` — the `edit`-position combinator
-- | for this direction (the dual of `resolveProperty`). Where `case_`
-- | **refocuses** (background fixed, focus transformed), this
-- | **re-backgrounds**: the **focus** `f` at `l` is held fixed and threaded as
-- | **input case ↔ output field**, while the wrapped profunctor transforms the
-- | **background** `b → b'` (turning the input **shot** `s` into the output
-- | shot `s'`). If the input carries case `l :: f`, its value resumes directly
-- | into output field `l` (the `Right` branch); otherwise the wrapped
-- | profunctor runs on the background cases and field `l` is filled from the
-- | carrier's retained state (the `c` that `retain` always emits), not from
-- | the wrapped profunctor.
retainCase
  :: forall @l p f b s b' s'
   . Retaining p
  => IsSymbol l
  => Cons l f b s
  => Cons l f b' s'
  => p [ | b ] { | b' }
  -> p [ | s ] { | s' }
retainCase g =
  dimap
    (on (Proxy @l) Right Left)
    -- no `Lacks`: `unsafeSet` realizes the layout `Cons l f b' s'` pins —
    -- under a shadowed duplicate label the outer entry wins, the same
    -- first-label convention `inj`/`on` follow.
    (\(Tuple b' f) -> unsafeSet (reflectSymbol (Proxy @l)) f b')
    (retain g)

-- | The single-case **focus** for this direction — the `+ → ×` analogue of
-- | `case_` (row-typed `left`), built on `retain` exactly as `case_` is built
-- | on `left`. The **focus** `f` at `l` of the input **shot** `s` is fed to the
-- | wrapped `p f f'` (the `Left`/fresh branch); the **background** `[ | b ]`
-- | cannot stay a variant inside the `Record` output, so — as in `reelWrap` —
-- | it is wrapped as a single output field `w`. Field `l` carries the wrapped
-- | profunctor's output `f'` (drawn from the carrier's retained state when a
-- | background case arrived), field `w` the background-variant. The
-- | single-case form of `reelWrap`; the transpose of `retainCase`, which runs
-- | the wrapped profunctor on the *background* and resumes the focus directly.
caseToProperty
  :: forall @l @w p f f' b s lf s'
   . Retaining p
  => IsSymbol l
  => IsSymbol w
  => Cons l f b s
  => Cons l f' () lf
  => Cons w [ | b ] lf s'
  => p f f'
  -> p [ | s ] { | s' }
caseToProperty g =
  dimap
    (on (Proxy @l) Left Right)
    -- no `Lacks`: `unsafeSet` realizes the layout the `Cons` chain pins —
    -- first-label convention, as `inj`/`on`.
    (\(Tuple f' b) -> unsafeSet (reflectSymbol (Proxy @w)) b (unsafeSet (reflectSymbol (Proxy @l)) f' {}))
    (retain g)

-- | The `+ → ×` member of the introduce family and the dual of `recordToCase`:
-- | the wrapped `p f { | r }` consumes the **focus** — case `l` of the input
-- | **shot** `s` — and produces the whole output record `r`. `r` is the
-- | **reality** the camera is pointed at: it never enters the shot, and here it
-- | must be *produced* without arriving — every **background** case must still
-- | yield a record, and a sum input can't supply one, so it is replayed from
-- | the carrier's retained state. That is why this member alone needs
-- | `Retaining`. A Mealy **reducer**: case `l` updates the record via `g`, the
-- | background cases leave it as it was.
caseToRecord
  :: forall @l p b s f r
   . Retaining p
  => IsSymbol l
  => Cons l f b s
  => p f { | r }
  -> p [ | s ] { | r }
caseToRecord g = dimap (on (Proxy @l) Left Right) fst (retain g)

-- | The optic `retain` induces: the **Reel**. Eliminating the residual `c`
-- | (instantiated to `b → t`) by co-Yoneda collapses `∃c. (s → a + c) × (b × c → t)`
-- | to `s → Either a (b → t)` — a per-input dispatch that either surfaces a focus
-- | `a`, or supplies a *finisher* `b → t` drawn from retained state. Like a film
-- | reel: a wound transport that holds its position and never finishes.
type Reel s t a b = forall p. Retaining p => p a b -> p s t

reel :: forall s t a b. (s -> Either a (b -> t)) -> Reel s t a b
reel dispatch g = reelE dispatch (\(Tuple b f) -> f b) g

-- | Construct a `Reel` straight from its **existential encoding**
-- | `∃c. (s → a + c) × (b × c → t)`: pick the residual `c`, then supply `decon`
-- | (match `s` as a fresh focus `a` or a resumed state `c`) and `recon` (combine
-- | the focus result `b` with the carried state `c` into `t`). The quantified `c`
-- | is exactly the eliminator of that existential; `retain` is the carrier. `reel`
-- | is this at the co-Yoneda witness `c := b → t` (`recon = \(Tuple b f) -> f b`,
-- | i.e. evaluation).
reelE :: forall s t a b c. (s -> Either a c) -> (Tuple b c -> t) -> Reel s t a b
reelE decon recon g = dimap decon recon (retain g)

-- | Row existential `Reel` focusing a whole **sub-Variant** — the row-valued
-- | **focus** `f` — of the input **shot** `s`; the residual is the **background**
-- | `[ | b ]` (`ExclusiveRows f b s`, the split `splitVariant` performs).
-- | Crossing `+ → ×`, the background can't stay a variant in the `Record`
-- | output, so it is **wrapped as a single output field `w`** — a record
-- | holding the variant. The output extension is itself shot-shaped:
-- | `Cons w [ | b ] b' s'` — the wrapped background is the focus of a second
-- | shot at `w`, against the inner output `b'`. The inner
-- | `p [ | f ] { | b' }` runs on the focus; the retained background-variant is
-- | written at field `w`. The sub-variant focus for this direction, and
-- | the dual of `shutterWrap` — same sub-row focus, but the background is
-- | *wrapped* to cross into the record output rather than carried same-kind.
-- | The `+ → ×` row combinator over the bare strength `Retaining`,
-- | just as `focusRecord` is the row combinator over `Strong`.
-- |
-- | ```purescript
-- | -- focus the `cancel` case; wrap the background into output field `pending`
-- | step :: Reel
-- |   [ cancel :: Unit, tick :: Int ]                               -- s   input shot
-- |   { done :: Boolean, pending :: [ tick :: Int ] }              -- s'  output shot
-- |   [ cancel :: Unit ]                                            -- f   sub-Variant focus
-- |   { done :: Boolean }                                          -- b'  inner output
-- | step = reelWrap @"pending"
-- | ```
reelWrap
  :: forall @w p f b s b' s'
   . Retaining p
  => IsSymbol w
  => ExclusiveRows f b s
  => Contractable s f
  => Contractable s b
  => Cons w [ | b ] b' s'
  => p [ | f ] { | b' }
  -> p [ | s ] { | s' }
reelWrap g =
  reelE
    splitVariant
    -- no `Lacks`: `unsafeSet` realizes the layout `Cons w [ | b ] b' s'` pins —
    -- first-label convention, as `inj`/`on`.
    (\(Tuple b' bg) -> unsafeSet (reflectSymbol (Proxy @w)) bg b')
    g
