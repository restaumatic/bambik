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
-- |     `Profunctor`: the emit stage `armed`; over `Strong`: `joined @l` (an
-- |     event joined with the row its emitter was fed); over the co-strength
-- |     `Coresolving`: `folding @w @l` (the terminating fold at row
-- |     granularity, its state one field `l` labelled on the view line —
-- |     the `Coshutter` optic's row form).
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
-- | (`SharedRecordInputs`: both operands fed the merge's one row), outputs
-- | shared (`SharedVariantOutputs`: the inclusive union of their cases).
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
  , armed
  , joined
  , bind
  , discard
  , subResolving
  , replaying
  , folding
  )
  where

import Control.Category (identity)
import Control.Semigroupoid ((>>>))
import Data.Either (Either(..), either)
import Data.Lens.Shutter (shutterE)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Coresolving (class Coresolving, coresolve)
import Data.Profunctor.Resolving (class Resolving)
import Data.Profunctor.Row (class ExclusiveRows, class SharedRecordInputs, class SharedVariantOutputs, widenRecordInput)
import Data.Profunctor.Seeding (class Seeding, isHole, seeded)
import Data.Profunctor.Strong (class Strong, first)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (case_, expand, inj, on)
import Prim.Row (class Cons, class Union)
import Type.Proxy (Proxy(..))
import Record.Unsafe (unsafeSet)
import Record.Unsafe.Union (unsafeUnion)
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
  :: forall p v1 v2 v3 v4 v5 r v
   . RecordToVariant p
  => SharedVariantOutputs v1 v2 v v3 v4 v5
  => p { | r } [ | v1 ]
  -> (p { | r } [ | v1 ] -> p { | r } [ | v2 ])
  -> p { | r } [ | v ]
bind first cont = recordToVariant first (cont first)

discard
  :: forall p v1 v2 v3 v4 v5 r v
   . RecordToVariant p
  => SharedVariantOutputs v1 v2 v v3 v4 v5
  => p { | r } [ | v1 ]
  -> (Unit -> p { | r } [ | v2 ])
  -> p { | r } [ | v ]
discard first cont = bind first (\_ -> cont unit)

-- | Focus a sub-record of the input, wrapping the background into output case `w`.
subResolving
  :: forall @w p r1 b r v1 v2 v3
   . Resolving p
  => IsSymbol w
  => ExclusiveRows r1 b r
  => Cons w { | b } v1 v2
  => Union v1 v3 v2
  => p { | r1 } [ | v1 ]
  -> p { | r } [ | v2 ]
subResolving g =
  shutterE
    (\s -> Tuple (unsafeCoerce s) (unsafeCoerce s))
    (either expand (inj (Proxy @w)))
    g

-- | Replay the last fed row, mapped by `f`, as case `l` on each occurrence of the source.
-- | The source is fed the row it replays, whole.
replaying
  :: forall @l p r v1 k v
   . Strong p
  => IsSymbol l
  => Cons l k () v
  => ({ | r } -> k)
  -> p { | r } [ | v1 ]
  -> p { | r } [ | v ]
replaying f src = dimap (\r -> Tuple (unsafeCoerce r) r) (\(Tuple _ r) -> inj (Proxy @l) (f r)) (first src)

-- | An event joined with the row its emitter was fed: the `×→+` stage's
-- | `Strong` retention, exposed. `first` around the source — the fed row
-- | rides the state channel and leaves beside each occurrence's payload as
-- | one record, `{ event, model }`, under the same case. So a list pick, a
-- | canvas click or a tick reaches the fold with the model in hand, and its
-- | handler is one function of one record (`listOf … # joined @"toggled"`,
-- | `fold (match { toggled: toggleTodo })` with `toggleTodo :: { event ::
-- | Int, model :: { … } } -> { … }`, todo-list). `replaying @l f` is the
-- | degenerate case where the occurrence carries nothing of its own
-- | (2026-10-04).
joined
  :: forall @l p r a v v1
   . Strong p
  => IsSymbol l
  => Cons l a () v
  => Cons l { event :: a, model :: { | r } } () v1
  => p { | r } [ | v ]
  -> p { | r } [ | v1 ]
joined src = dimap (\r -> Tuple r r) (\(Tuple e r) -> inj (Proxy @l) { event: on (Proxy @l) identity case_ e, model: r }) (first src)

-- | Mark an event ensemble as fed the row its emitters replay.
-- | The emitters replay the whole fed row; what a consumer reads of the
-- | payload is its own open-row footprint.
armed
  :: forall p r o
   . Profunctor p
  => p { | r } [ | o ]
  -> p { | r } [ | o ]
armed = widenRecordInput

-- | Fold state field `l` through case `w` until a `done` case exits, seeded with its initial value.
-- | The state is one field labelled on the view line (`folding @"next"
-- | @"step" cartStep`); the loop case carries `{ l :: a }`. A seed that is
-- | a hole is not injected (guardrails L18).
folding
  :: forall @w @l @a p r r1 r2 v v1
   . Seeding p
  => Coresolving p
  => IsSymbol w
  => IsSymbol l
  => Cons l a () r1
  => Cons l a r r2
  => Cons w { | r1 } v v1
  => a
  -> p { | r2 } [ | v1 ]
  -> p { | r } [ | v ]
folding seed g =
  coresolve
    (dimap
      -- the fold state is written over the fresh input, so a fat upstream
      -- emission's stale copy of it never shadows the folded state
      (\(Tuple i fb) -> unsafeUnion fb i :: { | r2 })
      (on (Proxy @w) Right Left)
      (if isHole seed then g else g >>> seeded (inj (Proxy @w) (unsafeSet (reflectSymbol (Proxy @l)) seed {} :: { | r1 }))))
