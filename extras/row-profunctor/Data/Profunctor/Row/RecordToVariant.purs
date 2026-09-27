-- | `Record → Variant` (× → +) row profunctors, organized (uniformly across
-- | the four shape modules) as:
-- |
-- |   * **strength** — `Resolving` (`Data.Profunctor.Resolving`; `PUI m`
-- |     instances only, no `(->)`): the unary power, a loop/iteration step.
-- |     Its co-strength `Coresolving` is in `Data.Profunctor.Coresolving`,
-- |     and their optics are `Data.Lens.Shutter` and `Data.Lens.Coshutter`
-- |     — neither the classes nor the optics mention a row, so none of them
-- |     lives here.
-- |   * **shape class** — `RecordToVariant`, the binary **merge** and
-- |     its unit `silence`: the one genuine per-carrier primitive, with its
-- |     qualified-do sugar (`bind`/`discard`).
-- |   * **free functions** — over the strength: `subResolving` (a
-- |     sub-record, the background escaping as a case); over `Strong`:
-- |     `replaying @l` (replay as `Strong`'s retention); over bare
-- |     `Profunctor`: the emit stage `armed`; over the co-strength
-- |     `Coresolving`: `folding @w` (the terminating fold at row
-- |     granularity, the `Coshutter` optic's row form).
-- |
-- | A word lives in the module of the sides it constrains: one polymorphic
-- | on one side sits in the diagonal module of the side it constrains, so
-- | the mixed modules hold only their strength, their trace and the words
-- | that genuinely span both sides.
-- | The variant-output adopter `toCase` is therefore `VariantToVariant`'s. A label `@w` appears exactly where a row is
-- | wrapped as one case to cross the shape change (`subResolving`,
-- | `folding`).
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
-- |      a real word: `toCase @l identity g = rmap (inj (Proxy @l)) g`, which
-- |      is why `toCase` needs only `Profunctor`.
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
-- | real sources are clicked on the leaf-law bench); 3–6 run over every
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
  ( class RecordToVariant
  , recordToVariant
  , silence
  , bind
  , discard
  , subResolving
  , replaying
  , armed
  , folding
  )
  where

import Control.Semigroupoid ((>>>))
import Data.Either (Either(..), either)
import Data.Lens.Shutter (shutterE)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Coresolving (class Coresolving, coresolve)
import Data.Profunctor.Resolving (class Resolving)
import Data.Profunctor.Row (class ExclusiveRows, class FieldNames, class SharedRecordInputs, class SharedVariantOutputs, exactRow, widenRecordInput)
import Data.Profunctor.Seeding (class Seeding, seeded)
import Data.Profunctor.Strong (class Strong, first)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (expand, inj, on)
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList)
import Record (union) as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= RecordToVariant p where
  recordToVariant
    :: forall i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o
     . SharedRecordInputs i1 i2 i i12 i1x i2x
    => SharedVariantOutputs o1 o2 o o12 o1x o2x
    => p { | i1 } [ | o1 ]
    -> p { | i2 } [ | o2 ]
    -> p { | i } [ | o ]
  -- | The silent component, emitting no case at any rows: the merge's unit.
  silence :: forall i o. p { | i } [ | o ]

bind
  :: forall p i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o
   . RecordToVariant p
  => SharedRecordInputs i1 i2 i i12 i1x i2x
  => SharedVariantOutputs o1 o2 o o12 o1x o2x
  => p { | i1 } [ | o1 ]
  -> (p { | i1 } [ | o1 ] -> p { | i2 } [ | o2 ])
  -> p { | i } [ | o ]
bind first cont = recordToVariant first (cont first)

discard
  :: forall p i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o
   . RecordToVariant p
  => SharedRecordInputs i1 i2 i i12 i1x i2x
  => SharedVariantOutputs o1 o2 o o12 o1x o2x
  => p { | i1 } [ | o1 ]
  -> (Unit -> p { | i2 } [ | o2 ])
  -> p { | i } [ | o ]
discard first cont = bind first (\_ -> cont unit)

-- | Focus a sub-record of the input, wrapping the background into output case `w`.
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

-- | Replay the last fed row, mapped by `f`, as case `l` on each occurrence of the source.
replaying
  :: forall @l p narrow extra r o k s
   . Strong p
  => IsSymbol l
  => Cons l k () s
  => Union narrow extra r
  => ({ | r } -> k)
  -> p { | narrow } [ | o ]
  -> p { | r } [ | s ]
replaying f src = dimap (\r -> Tuple (unsafeCoerce r) r) (\(Tuple _ r) -> inj (Proxy @l) (f r)) (first src)

-- | Feed an event ensemble the sub-row its emitters replay, widening its input.
armed
  :: forall p narrow extra wider o
   . Profunctor p
  => Union narrow extra wider
  => p { | narrow } [ | o ]
  -> p { | wider } [ | o ]
armed = widenRecordInput

-- | Fold the state sub-record through case `w` until a `done` case exits, seeded with the fold's initial state.
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
