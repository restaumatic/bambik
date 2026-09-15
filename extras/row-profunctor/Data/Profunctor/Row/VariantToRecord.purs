-- | `Variant → Record` (+ → ×) row profunctors, organized (uniformly across
-- | the four direction modules) as:
-- |
-- |   * **strength** — `Retaining` (`Data.Profunctor.Retaining`; `PUI m`
-- |     instances only, no `(->)`): the unary power, a Mealy/coroutine step.
-- |     Its co-strength `Coretaining` is in `Data.Profunctor.Coretaining`,
-- |     and their optics in `Data.Lens.Reel`/`Data.Lens.Coreel` — neither the
-- |     classes nor the optics mention a row, so none of them lives here.
-- |
-- | The adopters here (`forCases` and its single-case convenience
-- | `forCase @l`) carry **no canonical label**:
-- | a status states its payload case once, in its own row, and the adopters
-- | read it back out via `RowToList`'s fundep — application code writes only
-- | the business cases (`# forCase @"booked" bookedLine`,
-- | `# forCases bookingLine`), and no layer hard-codes a label.
-- |   * **direction class** — `VariantToRecord`, the binary **merge**: the one
-- |     genuine per-carrier primitive.
-- |   * **free functions over the strength** — everything else, named for
-- |     *what the wrapped profunctor runs on*: `subRetaining` (a sub-variant),
-- |     `focusCase` (one case), `backgroundCase` (the background, the focus
-- |     threaded across) — and over the
-- |     co-strength `Coretaining`: `unfolding @w` (the productive unfold,
-- |     the `Coreel` optic's row form).
-- |
-- | ## Laws at `+→×`
-- |
-- | The six laws of Data.Profunctor.Row ("The laws") read at this shape:
-- | a component `w :: p [ | i ] { | o }` is a **fold** of occurrences into
-- | retained state, or at `o = {}` a **status**; the merge is
-- | `m = variantToRecord w1 w2`, inputs owned (`OwnedVariantInputs`),
-- | outputs owned (`OwnedRecordOutputs`).
-- |
-- |   1–2. **Repetition, Answer** — not owed. A variant input is
-- |      dispatched to one owner and `≈` counts every occurrence: a fold
-- |      steps on each, a status renders each, and whether an occurrence
-- |      releases is the fold's; a status owes the channel nothing. What
-- |      is released is a `{ | o }` — whole by type — and before the
-- |      retained state exists a release needing it is withheld, not
-- |      invented, which is why `unfolding`/`accumulated` take a seed.
-- |   3. **Monoid** — unit `lcmap case_ identity :: p (Variant ()) {}`,
-- |      exact: `variantToRecord (lcmap case_ identity) g = g =
-- |      variantToRecord g (lcmap case_ identity)`; symmetric and
-- |      associative up to `≈`. Never fed and owning no field, the unit's
-- |      side is born spoken, so any silent element of that type serves
-- |      equally (`silence` at `b = ()` is one). The merge pinned at it —
-- |      one case folding into the record — is derivable and not exported.
-- |   4. **Projection** — `π_k` is the operand's own cases: an occurrence
-- |      of case `l` reaches the one operand owning `l` and no other
-- |      (`DisjointLabels`), and nothing else reaches an operand. `exact`
-- |      trims: `variantToRecord w1 w2 ≈ variantToRecord (rmap exactRow w1) w2`.
-- |   5. **Preservation** — vacuous at the input: nothing is owed. What
-- |      the shape adds is that no torn row is reachable: dispatch feeds
-- |      one owner per occurrence, so a release is one fresh contribution
-- |      beside retained ones — every ingredient of tearing but a
-- |      broadcast.
-- |   6. **Monotonicity** — `w1 ⊑ w1'` implies
-- |      `variantToRecord w1 w2 ⊑ variantToRecord w1' w2`.
-- |
-- | On `PUI`: dispatch in, gate out — the gate shared with `×→×`
-- | (`PUI.Gate.gateStep`): retain each side's last contribution, release
-- | their union once both owned sides have spoken, withhold before; the
-- | step is kept so a re-entrant echo during an occurrence coalesces into
-- | one release. Releasing on every occurrence once the row is whole is
-- | the carrier's permitted choice (no row needs `Eq`), not a law. A merge
-- | silent once both sides have spoken has an operand that never
-- | releases; one silent before that waits on an owned field's first
-- | occurrence — prime it (`unfolding`'s seed, `seeded`). Probes carry law
-- | and shape in test/Main.purs; 3, 4 and 6 and the gate's conformance to
-- | its pure step run over every script to a bound in test/Exhaustive.purs.
module Data.Profunctor.Row.VariantToRecord
  ( bind
  , variantToRecord
  , class VariantToRecord
  , discard
  , focusCase
  , forCase
  , forCases
  , backgroundCase
  , subRetaining
  , unfolding
  )
  where

import Control.Semigroupoid ((>>>))
import Data.Either (Either(..), either)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Seeding (class Seeding, seeded)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (class Contractable, class VariantMatchCases, case_, expand, inj, match, on)
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record.Unsafe (unsafeSet)
import Type.Proxy (Proxy(..))
import Data.Lens.Reel (reelE)
import Data.Profunctor.Coretaining (class Coretaining, coretain)
import Data.Profunctor.Retaining (class Retaining, retain)
import Data.Profunctor.Row (class ExclusiveRows, class OwnedRecordOutputs, class OwnedVariantInputs, splitVariant)
import Unsafe.Coerce (unsafeCoerce)

-- | `coretain` at row granularity — the **productive unfold** with labeled
-- | channels: the wrapped profunctor consumes either a fresh input case
-- | (any case of `i`) or a resume (case `w`, carrying the unfold state
-- | `{ | fb }`), and every emission's value fields `o` pass while its state
-- | fields `fb` immediately re-enter as case `w` — a generator. The
-- | `+ → ×` co-analogue of `subRetaining`. Like `feedback`, the output is
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
-- | the citizenship-carrying interface) for a **classified variant** — the
-- | input dual of `RecordToVariant`'s `toCases`: one copy classifier renders
-- | the cases the status owns into its own payload case (derived from its
-- | row via `RowToList`'s fundep), so a single status instance serves
-- | mutually exclusive outcomes — `status # forCases { booked: bookedLine,
-- | rejected: rejectionLine }`. The classifier is the **record of per-case
-- | copy functions** and the elimination is this mechanism's own (no
-- | `match` at the call site): the record's totality over its own row is
-- | the exhaustiveness guarantee, and the row it states is exactly what the
-- | operand owns — a one-field record adopts one business case, sibling
-- | merge operands keeping theirs.
-- | The copy formatters are the mechanism's own argument (import-tower rule
-- | L16): the adopted cases carry bare business payloads, rendered at the
-- | adoption site; `identity` branches when the payload already is the copy.
-- | Per-case copy stays a record field, never a sibling operand, when
-- | outcomes share one status area.
forCases :: forall c p a o s1 s rl r cs. RowToList cs (RL.Cons c a RL.Nil) => IsSymbol c => Cons c a () cs => Profunctor p => RowToList r rl => VariantMatchCases rl s1 a => Union s1 () s => { | r } -> p [ | cs ] o -> p [ | s ] o
forCases handlers = lcmap (\v -> inj (Proxy @c) (match handlers v))

-- | `forCases` at one business case — the derived single-case convenience,
-- | keeping the adopter in the `@l` grammar (`atCase @l`/`toCase @l`/
-- | `shownWhen @l`'s family), exactly as `applied` keeps `const` off
-- | `updated`'s. Law (the defining equation, at the singleton variant):
-- |
-- | ```
-- | forCase @l f = forCases { l: f }
-- | ```
-- |
-- | `status # forCase @"registered" welcomeLine`; a status owning several
-- | cases of one variant stays one `forCases` classifier, and sibling
-- | `forCase` operands each own exactly their case.
forCase :: forall @l c p a b o s cs. RowToList cs (RL.Cons c a RL.Nil) => IsSymbol c => IsSymbol l => Cons c a () cs => Cons l b () s => Profunctor p => (b -> a) -> p [ | cs ] o -> p [ | s ] o
forCase f = lcmap (on (Proxy @l) (\b -> inj (Proxy @c) (f b)) case_)

-- | Single-case specialization of `retain` — the `edit`-position combinator
-- | for this direction (the dual of `backgroundProperty`). Where `case_`
-- | **refocuses** (background fixed, focus transformed), this
-- | **re-backgrounds**: the **focus** `f` at `l` is held fixed and threaded as
-- | **input case ↔ output field**, while the wrapped profunctor transforms the
-- | **background** `b → b'` (turning the input **shot** `s` into the output
-- | shot `s'`). If the input carries case `l :: f`, its value resumes directly
-- | into output field `l` (the `Right` branch); otherwise the wrapped
-- | profunctor runs on the background cases and field `l` is filled from the
-- | carrier's retained state (the `c` that `retain` always emits), not from
-- | the wrapped profunctor.
backgroundCase
  :: forall @l p f b s b' s'
   . Retaining p
  => IsSymbol l
  => Cons l f b s
  => Cons l f b' s'
  => p [ | b ] { | b' }
  -> p [ | s ] { | s' }
backgroundCase g =
  dimap
    (on (Proxy @l) Right Left)
    -- no `Lacks`: `unsafeSet` realizes the layout `Cons l f b' s'` pins —
    -- under a shadowed duplicate label the outer entry wins, the same
    -- first-label convention `inj`/`on` follow.
    (\(Tuple b' f) -> unsafeSet (reflectSymbol (Proxy @l)) f b')
    (retain g)

-- | The single-case **focus** for this direction — the `+ → ×` analogue of
-- | `VariantToVariant.focusCase` (row-typed `left`), built on `retain`
-- | exactly as that one is built on `left`. The **focus** `f` at `l` of the input **shot** `s` is fed to the
-- | wrapped `p f f'` (the `Left`/fresh branch); the **background** `[ | b ]`
-- | cannot stay a variant inside the `Record` output, so — as in `subRetaining` —
-- | it is wrapped as a single output field `w`. Field `l` carries the wrapped
-- | profunctor's output `f'` (drawn from the carrier's retained state when a
-- | background case arrived), field `w` the background-variant. The
-- | single-case form of `subRetaining`; the transpose of `backgroundCase`, which runs
-- | the wrapped profunctor on the *background* and resumes the focus directly.
focusCase
  :: forall @l @w p f f' b s lf s'
   . Retaining p
  => IsSymbol l
  => IsSymbol w
  => Cons l f b s
  => Cons l f' () lf
  => Cons w [ | b ] lf s'
  => p f f'
  -> p [ | s ] { | s' }
focusCase g =
  dimap
    (on (Proxy @l) Left Right)
    -- no `Lacks`: `unsafeSet` realizes the layout the `Cons` chain pins —
    -- first-label convention, as `inj`/`on`.
    (\(Tuple f' b) -> unsafeSet (reflectSymbol (Proxy @w)) b (unsafeSet (reflectSymbol (Proxy @l)) f' {}))
    (retain g)

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
-- | the dual of `RecordToVariant.subResolving` — same sub-row focus, but the background is
-- | *wrapped* to cross into the record output rather than carried same-shape.
-- | The `+ → ×` row combinator over the bare strength `Retaining`,
-- | just as `RecordToRecord.subStrong` is the row combinator over `Strong`.
-- |
-- | ```purescript
-- | -- focus the `cancel` case; wrap the background into output field `pending`
-- | step :: Reel
-- |   [ cancel :: Unit, tick :: Int ]                               -- s   input shot
-- |   { done :: Boolean, pending :: [ tick :: Int ] }              -- s'  output shot
-- |   [ cancel :: Unit ]                                            -- f   sub-Variant focus
-- |   { done :: Boolean }                                          -- b'  inner output
-- | step = subRetaining @"pending"
-- | ```
-- |
-- | Law (**background transparency**): `subRetaining @l w` leaves the
-- | background untouched — an occurrence outside `w`'s focus row crosses
-- | into output field `l` verbatim, `w` never seeing it. This is
-- | `Retaining`'s `retain` law read at the row, a law of the strength
-- | rather than of the merge (which is why it is not among the shape's
-- | laws).
subRetaining
  :: forall @w p f b s b' s'
   . Retaining p
  => IsSymbol w
  => ExclusiveRows f b s
  => Contractable s f
  => Contractable s b
  => Cons w [ | b ] b' s'
  => p [ | f ] { | b' }
  -> p [ | s ] { | s' }
subRetaining g =
  reelE
    splitVariant
    -- no `Lacks`: `unsafeSet` realizes the layout `Cons w [ | b ] b' s'` pins —
    -- first-label convention, as `inj`/`on`.
    (\(Tuple b' bg) -> unsafeSet (reflectSymbol (Proxy @w)) bg b')
    g
