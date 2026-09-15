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
-- |     `backgroundProperty` (the background,
-- |     the focus escaping), `recordToCase` (introduce; mere
-- |     `Profunctor`) — and over the co-strength `Coresolving`:
-- |     `folding @w` (the terminating fold at row granularity, the
-- |     `Coshutter` optic's row form).
-- |
-- | ## Laws at `×→+`
-- |
-- | The six laws of Data.Profunctor.Row ("The laws") read at this shape:
-- | a component `w :: p { | i } [ | o ]` is an **event source** fed a row;
-- | the merge is `m = recordToVariant w1 w2`, inputs shared
-- | (`SharedRecordInputs`), outputs shared (`SharedVariantOutputs`).
-- |
-- |   1. **Repetition** — owed: `feed x ; feed x ≈ feed x`. A feed writes
-- |      the replay slot; a second write of the same row changes nothing.
-- |   2. **Answer** — nothing: a feed is answered by no emission. It
-- |      **arms** — every emission has a cause outside the channel (a
-- |      click, an `Aff` settling, quiescence) — which is what lets
-- |      `updated`/`applied` feed an emitter without firing it. What an
-- |      emission carries is the source's contract, not the shape's:
-- |      `clicked` replays the row last fed.
-- |   3. **Monoid** — unit `silence`, the class's own member, exact:
-- |      `recordToVariant silence g = g = recordToVariant g silence`;
-- |      symmetric and associative up to `≈`. No wire reaches this unit
-- |      (`{}` is terminal, `Variant ()` initial), and parametricity
-- |      extends the silent element to any rows. The merge pinned at it is
-- |      a real word: `recordToCase @l g = rmap (inj (Proxy @l)) g`, which
-- |      is why `recordToCase` and `toCase`/`toCases` over it need only
-- |      `Profunctor`.
-- |   4. **Projection** — `π_k` is the whole row: every feed of `m`
-- |      reaches each operand whole, arming it, and nothing else does.
-- |      `exact` is the identity: a variant carries its one tag, so two
-- |      operands may declare the same case and the merge forwards each,
-- |      unmarked.
-- |   5. **Preservation** — `m` obeys 1 and 2: neither operand answers a
-- |      feed, so neither does their passage.
-- |   6. **Monotonicity** — `w1 ⊑ w1'` implies
-- |      `recordToVariant w1 w2 ⊑ recordToVariant w1' w2`.
-- |
-- | On `PUI`: broadcast in, passage out — each emission exits as it
-- | occurs, nothing retained, no state (`Applicative m` suffices). Law 2
-- | is why this shape has no starvation: a silent source is lawful, and
-- | an absent one is `silence` (below). Probes carry law and shape in
-- | test/Main.purs (`answer ×→+` on the probe carrier's replay source; the
-- | real sources are walked by the smoke suite); 3–6 run over every
-- | script to a bound in test/Exhaustive.purs.
-- |
-- | `silence` is also what an **absent event source is**. A `×→+` component that
-- | exists in one state and not in another
-- | (`button @"Start" {} # provided @"halted" phaseOf`) is, in the state
-- | where it is absent, observationally `silence`: fed nothing, emitting
-- | nothing. That is why the type is variant-output only — silent at
-- | *variant* output is a source with nothing to say, while silent at an
-- | inhabited *record* output would be a starved gate, which is why the
-- | record-side panes (`shownWhen`/`inCase`) release the fed row always
-- | instead. But absence is **not a copairing**: routing the input away
-- | from `w` leaves `w`'s own emission channel connected, so
-- | `lcmap f' (left w >>> right silence)` still emits when the source
-- | fires in the "absent" case (checked on the probe carrier, 2026-09-11:
-- | a Start button clicked while `timing` still emitted). Only removing
-- | the component silences it, and removal is `Hosting`'s — the DOM's —
-- | not the algebra's. So `provided` stays a carrier primitive, and
-- | `silence` is the *value* of what it removes, not an operand it is
-- | built from. (Deleted and restored 2026-09-11 on this argument.)
module Data.Profunctor.Row.RecordToVariant
  ( bind
  , class RecordToVariant
  , discard
  , folding
  , silence
  , armed
  , recordToVariant
  , recordToCase
  , toCase
  , toCases
  , backgroundProperty
  , subResolving
  )
  where

import Data.Either (Either(..), either)
import Control.Semigroupoid ((>>>))
import Data.Profunctor (class Profunctor, dimap, rmap)
import Data.Profunctor.Seeding (class Seeding, seeded)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (case_, expand, inj, on)
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
import Data.Profunctor.Row (class ExclusiveRows, class FieldNames, class SharedRecordInputs, class SharedVariantOutputs, exactRow, widenRecordInput)
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
  :: forall @w p i il fb iw done ow
   . Seeding p
  => Coresolving p
  => IsSymbol w
  => ExclusiveRows i fb iw
  => Cons w { | fb } done ow
  => RowToList i il
  => FieldNames il i i
  => { | fb }
  -> p { | iw } [ | ow ]
  -> p { | i } [ | done ]
folding seed g =
  coresolve
    (dimap
      -- the join is left-biased; `exactRow` trims the fresh input to its
      -- declared row so a fat upstream emission cannot shadow the folded
      -- state fields with stale runtime copies (runtime-exactness)
      (\(Tuple i fb) -> Record.union (exactRow i) fb)
      (on (Proxy @w) Right Left)
      (g >>> seeded (inj (Proxy @w) seed)))

class Profunctor p <= RecordToVariant p where
  recordToVariant :: forall i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
    SharedRecordInputs i1 i2 i i12 i1x i2x =>
    SharedVariantOutputs o1 o2 o o12 o1x o2x =>
    p { | i1 } [ | o1 ] -> p { | i2 } [ | o2 ] -> p { | i } [ | o ]
  -- | The **nullary** merge — the unit: reads nothing, emits no cases, at
  -- | any rows. The one unit no wire reaches (`{}` is terminal, `Variant ()`
  -- | initial — nothing maps terminal → initial), so it is the one unit that
  -- | stays a class member; parametric in both rows because silence is
  -- | forced on any variant output and sufficient on any record input, so
  -- | one silent body serves every type. The pinned trivial operand of the
  -- | mixed introduce laws, the terminal sink of event pipelines, and what
  -- | an emitter removed by `provided` observationally is (header). The
  -- | lawful faceless leaf at *record* output is not silence but `blank`
  -- | (the wire's `lcmap`-closure in `RecordToRecord`).
  silence :: forall i o. p { | i } [ | o ]

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


-- | The **emit stage** of a record pipeline — the `× → +` member of the
-- | stage-subsumption family (`updated`/`settled` on
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
-- | for this direction. Where `RecordToRecord.field` **refocuses** (background fixed, focus
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
-- | channels): an editor's emission adopted as an occurrence — a toggle
-- | inside a collection, `iconToggle @"Favorite" {…} # foreach @"title"
-- | rows # toCase @"favored" favoriteMark` (movie-browser) — or a dialog's
-- | release, `confirmed cfg content # atCase @l # toCase @"refunded"
-- | identity` (cashbox); `identity` says verbatim. Sources need it no
-- | longer: `clicked @l f`, `listOf @l f`, `onClickedXY @l` and the HTML
-- | `button @l` emit their case themselves. The output-side dual of
-- | `atCase`.
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
-- | carried same-shape. The `× → +` row combinator over the bare strength
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
-- |
-- | Law (**background transparency**): `subResolving @l w` leaves the
-- | background untouched — a feed's background fields cross into output
-- | case `l` verbatim, `w` never seeing them. This is `Resolving`'s
-- | `resolve` law read at the row, a law of the strength rather than of
-- | the merge (which is why it is not among the shape's laws).
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