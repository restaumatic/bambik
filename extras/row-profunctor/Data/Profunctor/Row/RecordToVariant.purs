-- | `Record → Variant` (× → +) row profunctors, organized (uniformly across
-- | the four direction modules) as:
-- |
-- |   * **strength** — `Resolving` (`Data.Profunctor.Resolving`; `PUI m`
-- |     instances only, no `(->)`): the unary power, a loop/iteration step.
-- |     Its co-strength `Coresolving` is in `Data.Profunctor.Coresolving`,
-- |     and their optics in `Data.Lens.Shutter`/`Data.Lens.Coshutter` —
-- |     neither the classes nor the optics mention a row, so none of them
-- |     lives here.
-- |
-- | The adopter here (`toCases`) carries **no canonical label**: the
-- | emitter states its business case once, as its own type argument, and
-- | `toCases` reads it back out of the closed singleton variant row via
-- | `RowToList`'s fundep — no layer hard-codes a label.
-- |   * **direction class** — `RecordToVariant`, the binary **merge**: the one
-- |     genuine per-carrier primitive.
-- |   * **free functions over the strength** — everything else, named for
-- |     *what the wrapped profunctor runs on*: `subResolving` (a sub-record),
-- |     `focusProperty` (one field), `backgroundProperty` (the background,
-- |     the focus escaping), `recordToCase` (introduce; mere
-- |     `Profunctor`) — and over the co-strength `Coresolving`:
-- |     `folding @w` (the terminating fold at row granularity, the
-- |     `Coshutter` optic's row form).
-- |
-- | Law connecting the two classes: the mixed directions have no `identity` to
-- | pin (nothing inhabits a mode-crossing diagonal), but they have the class's
-- | own **unit** `pempty :: p {} (Variant ())`, the silent source. The unary
-- | introduce operator is the **unit-pinned merge**,
-- |
-- | ```
-- | recordToCase @l g = recordToVariant (rmap (inj (Proxy @l)) g) pempty
-- | ```
-- |
-- | and a pinned unit contributes nothing — which is why `recordToCase`
-- | collapses to plain `rmap (inj l)` on any `Profunctor`.
-- |
-- | As nullary operator, `pempty` is the empty merge:
-- | `recordToVariant pempty g = g`. Silence is forced on the output end (the
-- | empty variant is uninhabited) and sufficient on the input end (the empty
-- | record demands nothing), and parametricity extends both to arbitrary
-- | types — which is `silence` (below), the unit's `dimap`-closure.
module Data.Profunctor.Row.RecordToVariant
  ( bind
  , class RecordToVariant
  , discard
  , folding
  , pempty
  , silence
  , armed
  , recordToVariant
  , focusProperty
  , recordToCase
  , toCase
  , toCases
  , backgroundProperty
  , subResolving
  )
  where

import Data.Either (Either(..), either)
import Control.Semigroupoid ((>>>))
import Data.Function (const)
import Data.Profunctor (class Profunctor, dimap, rmap)
import Data.Profunctor.Seeding (class Seeding, seeded)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (Variant, case_, expand, inj, on)
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record (get)
import Record (union) as Record
import Record.Unsafe (unsafeDelete)
import Type.Proxy (Proxy(..))
import Data.Lens.Shutter (shutterE)
import Data.Profunctor.Coresolving (class Coresolving, coresolve)
import Data.Profunctor.Resolving (class Resolving, resolve)
import Data.Profunctor.Row (class ExclusiveRows, class SharedRecordInputs, class SharedVariantOutputs, widenRecordInput)
import Unsafe.Coerce (unsafeCoerce)

-- | `coresolve` at row granularity — the **terminating fold** with labeled
-- | channels: the wrapped profunctor sees its input joined with the folded
-- | state sub-record `fb`, and answers with a variant that either continues
-- | the fold (case `w`, carrying the next `{ | fb }` — retained silently)
-- | or exits (any `done` case — emitted). The `× → +` co-analogue of
-- | `subResolving`: there the background is wrapped as case `w` to *escape*,
-- | here case `w` is unwrapped to *loop*. No coercions: `on` splits the
-- | output variant exactly.
-- |
-- | The fold state is an **entity** — it exists from the fold's very
-- | beginning — and `folding` takes its t=0 value `{ | fb }` as the first
-- | argument: at registration the seed is emitted once as case `w` (a
-- | `seeded` wire composed onto the output), priming the state channel
-- | before any input arrives — a `folding` stage never starves.
-- | Emission-primed exotica remain expressible with raw
-- | `coresolve`/`coshutter`.
folding
  :: forall @w p i fb iw done ow
   . Seeding p
  => Coresolving p
  => IsSymbol w
  => ExclusiveRows i fb iw
  => Cons w { | fb } done ow
  => { | fb }
  -> p { | iw } [ | ow ]
  -> p { | i } [ | done ]
folding seed g =
  coresolve
    (dimap
      (\(Tuple i fb) -> Record.union i fb)
      (on (Proxy @w) Right Left)
      (g >>> seeded (inj (Proxy @w) seed)))

class Profunctor p <= RecordToVariant p where
  recordToVariant :: forall i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
    SharedRecordInputs i1 i2 i i12 i1x i2x =>
    SharedVariantOutputs o1 o2 o o12 o1x o2x =>
    p { | i1 } [ | o1 ] -> p { | i2 } [ | o2 ] -> p { | i } [ | o ]
  -- | The **nullary** merge — the unit: reads nothing, emits no cases. The
  -- | silent source of the header's law; silence is forced on the uninhabited
  -- | variant output and sufficient on the empty record input, so any silent
  -- | element implements it — this is the one direct silent body a carrier
  -- | writes, and `silence` below generalizes it to any types.
  pempty :: p {} (Variant ())

bind :: forall p i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
  RecordToVariant p =>
  SharedRecordInputs i1 i2 i i12 i1x i2x =>
  SharedVariantOutputs o1 o2 o o12 o1x o2x =>
  p { | i1 } [ | o1 ] -> (p { | i1 } [ | o1 ] -> p { | i2 } [ | o2 ]) -> p { | i } [ | o ]
bind first cont = recordToVariant first (cont first)

discard :: forall p i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
  RecordToVariant p =>
  SharedRecordInputs i1 i2 i i12 i1x i2x =>
  SharedVariantOutputs o1 o2 o o12 o1x o2x =>
  p { | i1 } [ | o1 ] -> (Unit -> p { | i2 } [ | o2 ]) -> p { | i } [ | o ]
discard first cont = bind first (\_ -> cont unit)

-- | The **silent UI component**: shows nothing, captures nothing — at any
-- | rows, and necessarily so (parametricity survives the shaping:
-- | `forall i o. p { | i } [ | o ]` can neither inspect unknown fields nor
-- | fabricate a case of an unknown row). Shaped `× → +` like everything
-- | here — silence's citizenship is this direction's, since it is the
-- | `dimap`-closure of its unit. The pinned trivial operand of the mixed
-- | introduce laws and the terminal sink of event pipelines. The lawful
-- | faceless leaf at *record* output is not silence but `blank` (the
-- | unit's `lcmap`-closure in `RecordToRecord`) — record outputs must
-- | announce.
silence :: forall p i o. RecordToVariant p => p { | i } [ | o ]
silence = dimap (const {}) case_ pempty

-- | The **emit stage** of a record pipeline — the `× → +` member of the
-- | stage-subsumption family (`tapped`/`updated`/`settled` on
-- | the `×`-diagonal, `observed` on `+`): feed an event ensemble the
-- | sub-row its emitters replay, emissions passing on unchanged. Feeding
-- | *arms* the replay values — hence the name. The wrapped stage's row is
-- | already stated exactly by its emitters' consumers (payloads are
-- | exact), so a linear pipeline's polarity flip reads narrow with no
-- | call-site coercion: `(RecordToVariant.do … buttons …) # armed`.
-- | `lcmap`-only — the vocabulary face of `widenRecordInput` at this
-- | direction's citizenship.
armed :: forall p narrow extra wider o. Profunctor p => Union narrow extra wider => p { | narrow } [ | o ] -> p { | wider } [ | o ]
armed = widenRecordInput

-- | Single-field specialization of `resolve` — the `edit`-position combinator
-- | for this direction. Where `RecordToRecord.focusProperty` **refocuses** (background fixed, focus
-- | transformed), this **re-backgrounds**: the **focus** `f` at `l` is held
-- | fixed and threaded across the boundary as **input field ↔ output case**,
-- | while the wrapped profunctor transforms the **background** `b → b'`
-- | (turning the input **shot** `s` into the output shot `s'`). The `Done`
-- | branch emits some case of `b'`; the `Loop`/short-circuit branch lets the
-- | focus escape directly as output case `l`.
backgroundProperty
  :: forall @l p f lf b s b' s'
   . Resolving p
  => IsSymbol l
  => Cons l f b s
  => Cons l f b' s'
  => Cons l f () lf
  => Union b' lf s'
  => p { | b } [ | b' ]
  -> p { | s } [ | s' ]
backgroundProperty g =
  dimap
    -- no `Lacks`: `unsafeDelete` realizes the layout `Cons l f b s` pins —
    -- under a shadowed duplicate label the outer entry wins, the same
    -- first-label convention `inj`/`on` follow.
    (\s -> Tuple (unsafeDelete (reflectSymbol (Proxy @l)) s) (get (Proxy @l) s))
    (either expand (inj (Proxy @l)))
    (resolve g)

-- | The single-field **focus** for this direction — the `× → +` analogue of
-- | `RecordToRecord.focusProperty` (row-typed `first`), built on `resolve`
-- | exactly as that one is built on `first`. The **focus** `f` at `l` of the input **shot** `s` is fed
-- | to the wrapped `p f f'`; the **background** `{ | b }` cannot stay a record
-- | inside the `Variant` output, so — as in `subResolving` — it is wrapped as a
-- | single output case `w`: `Done` emits case `l :: f'`, the `Loop`/escape
-- | branch emits case `w` carrying the untouched background. The single-field
-- | form of `subResolving`; the transpose of `backgroundProperty`, which runs the
-- | wrapped profunctor on the *background* and lets the focus escape.
focusProperty
  :: forall @l @w p f f' b s lx wx s'
   . Resolving p
  => IsSymbol l
  => IsSymbol w
  => Cons l f b s
  => Cons l f' lx s'
  => Cons w { | b } wx s'
  => p f f'
  -> p { | s } [ | s' ]
focusProperty g =
  dimap
    -- no `Lacks`: `unsafeDelete` realizes the layout `Cons l f b s` pins —
    -- under a shadowed duplicate label the outer entry wins, the same
    -- first-label convention `inj`/`on` follow.
    (\s -> Tuple (get (Proxy @l) s) (unsafeDelete (reflectSymbol (Proxy @l)) s))
    (either (inj (Proxy @l)) (inj (Proxy @w)))
    (resolve g)

-- | The `× → +` member of the introduce family: the wrapped `p { | r } f` reads
-- | the whole record — `r`, the **reality** the camera is pointed at, which
-- | never enters the shot — and its result, the **focus**
-- | `f`, is emitted as
-- | output case `l`. This is the `introduceCase` that `VariantToVariant`
-- | documents as impossible — there, a fresh output case must coexist with
-- | gated pass-through cases and can never fire; here nothing else emits, the
-- | computed case fires unconditionally, and no strength is needed at all:
-- | plain `rmap (inj l)` on any `Profunctor`. (The **background** `b` of the
-- | output **shot** `s` is simply never produced — the widening is free, as
-- | with `inj` itself.)
recordToCase
  :: forall @l p r b s f
   . IsSymbol l
  => Cons l f b s
  => Profunctor p
  => p { | r } f
  -> p { | r } [ | s ]
recordToCase = rmap (inj (Proxy @l))

-- | Introduce a UI component's **bare** output as case `l`, projected by the
-- | payload projection — `recordToCase` freed from the record-input
-- | constraint, at the **closed singleton row** (the `field` lesson:
-- | pinned empty background, so it infers with no annotations).
-- | The payload projection is the mechanism's own argument (import-tower
-- | rule L16: projections ride mechanisms, applications never map raw
-- | channels): a collection element emitting its identity,
-- | `… # toCase @"picked" _.key`;
-- | `identity` says verbatim (the case-introduction counterpart of
-- | `projected identity`). The
-- | output-side dual of `atCase`
-- | (which renames the canonical `clicked` case).
toCase :: forall @l p i a b s. IsSymbol l => Cons l b () s => Profunctor p => (a -> b) -> p i a -> p i [ | s ]
toCase f = rmap (\a -> inj (Proxy @l) (f a))

-- | Fire the **business outcome** of what the emitter was shown: adopt the
-- | emitter's case — derived from its closed singleton variant row — by
-- | applying `f` to its payload: `toCases` dissolves the
-- | event into the **variant of business results** `f` computes, so
-- | `button @"register" {…} # toCases register` emits `register`'s cases
-- | directly. The output dual of `VariantToRecord`'s `forCases` (emitters
-- | classify outward, statuses render inward). The outcome row is row-typed
-- | on purpose: this is the `× → +` output side, where a non-variant result
-- | would be out of shape — so `toCases`, like every other placement here,
-- | both takes and returns a row profunctor.
toCases :: forall c p i a o s. RowToList s (RL.Cons c a RL.Nil) => IsSymbol c => Cons c a () s => Profunctor p => (a -> [ | o ]) -> p i [ | s ] -> p i [ | o ]
toCases f = rmap (on (Proxy @c) f case_)

-- | Row existential `Shutter` focusing a whole **sub-Record** — the row-valued
-- | **focus** `f` — of the input **shot** `s`; the residual is the **background**
-- | `{ | b }` (`ExclusiveRows f b s`, the same split `RecordToRecord.subStrong` uses).
-- | Crossing `× → +`, the background can't stay a record in the `Variant`
-- | output, so it is **wrapped as a single output case `w`** — a variant
-- | carrying the record. The output extension is itself shot-shaped:
-- | `Cons w { | b } b' s'` — the wrapped background is the focus of a second
-- | shot at `w`, against the inner output `b'`. The inner
-- | `p { | f } [ | b' ]` runs on the focus: `Done` expands its result into
-- | `s'`, `Loop` injects the retained background-record into case `w`. The
-- | mixed-direction analogue of `RecordToRecord.subStrong` — same sub-record focus, but the
-- | background is *wrapped* to cross into the variant output rather than
-- | carried same-kind. The `× → +` row combinator over the bare strength
-- | `Resolving`, just as `RecordToRecord.subStrong` is the row combinator over
-- | `Strong`.
-- |
-- | ```purescript
-- | -- focus (item, qty); wrap the background { note } into output case `draft`
-- | checkout :: Shutter
-- |   { item :: String, qty :: Int, note :: String }              -- s   input shot
-- |   [ priced :: Int, draft :: { note :: String } ]              -- s'  output shot
-- |   { item :: String, qty :: Int }                              -- f   sub-Record focus
-- |   [ priced :: Int ]                                            -- b'  inner output
-- | checkout = subResolving @"draft"
-- | ```
subResolving
  :: forall @w p f b s b' s' mix
   . Resolving p
  => IsSymbol w
  => ExclusiveRows f b s
  => Cons w { | b } b' s'
  => Union b' mix s'
  => p { | f } [ | b' ]
  -> p { | s } [ | s' ]
subResolving g =
  shutterE
    (\s -> Tuple (unsafeCoerce s) (unsafeCoerce s))
    (either expand (inj (Proxy @w)))
    g