-- | `Record → Variant` (× → +) row profunctors, organized (uniformly across
-- | the four shape modules) as:
-- |
-- |   * **strength** — `Resolving` (`Data.Profunctor.Resolving`; `PUI m`
-- |     instances only, no `(->)`): the unary power, a loop/iteration step.
-- |     Its co-strength `Coresolving` is in `Data.Profunctor.Coresolving`,
-- |     and their optics are `Data.Lens.Shutter` and `Data.Lens.Coshutter`
-- |     — neither the classes nor the optics mention a row, so none of them
-- |     lives here.
-- |   * **shape class** — `RecordToVariant`, the binary **merge**: the
-- |     one genuine per-carrier primitive, with its qualified-do sugar
-- |     (`bind`/`discard`). Its unit `silence` is not its own but
-- |     `Data.Profunctor.Silencing`'s, as the other shapes' units are
-- |     `Category`'s — no merge class carries a unit.
-- |   * **free functions** — over the strength: `subResolving @w` (a
-- |     sub-record focused, the background escaping as case `w` — the
-- |     `Shutter` at row granularity); over the co-strength `Coresolving`:
-- |     `folding @w @l` (the terminating fold, its state one field `l`
-- |     carried by case `w` — the `Coshutter` at row granularity); over
-- |     `Strong`: `replaying @l` (replay as `Strong`'s retention) and
-- |     `joined @l` (an event joined with the row its emitter was fed).
-- |
-- | A word lives where the sides it constrains do: one constraining both
-- | sides in its shape's module, one constraining a single side on the
-- | floor (`Data.Profunctor.Row`, beside `atAnyRow`/`widenVariantOutput`)
-- | — so no shape module imports another, and every module holds only
-- | its merge, its strength and trace forms, and the words that genuinely
-- | span both of its sides.
-- | `replaying`/`joined` take `toCase` from the floor. A label `@w` appears exactly where a row is wrapped as one case to
-- | cross the shape change (`subResolving`, `folding`).
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
-- |      a loop re-feed an emitter without firing it. What an
-- |      emission carries is the source's contract, not the shape's:
-- |      `clicked` replays the row last fed.
-- |   3. **Monoid** — unit `silence` (`Data.Profunctor.Silencing`'s; the
-- |      law conditional on that class, as the others' are on `Category`),
-- |      exact:
-- |      `recordToVariant silence g = g = recordToVariant g silence`;
-- |      symmetric and associative up to `≈`. No wire reaches this unit
-- |      (`{}` is terminal, `Variant ()` initial), and parametricity
-- |      extends the silent element to any rows. The merge pinned at it is
-- |      a real word: `toCase @l g = rmap (inj (Proxy @l)) g`, which
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
-- | (`button @"Start" # provided @"halted" phaseOf`) is, in the state
-- | where it is absent, observationally `silence`: fed nothing, emitting
-- | nothing. That is why the type is variant-output only — silent at
-- | *variant* output is a source with nothing to say, while silent at an
-- | inhabited *record* output would be a starved gate, which is why the
-- | record-side pane answers every feed instead — `inCase`, with the fed
-- | row. But absence is **not a copairing**: routing the input away
-- | from `w` leaves `w`'s own emission channel connected, so
-- | `lcmap f' (left w >>> right silence)` still emits when the source
-- | fires in the "absent" case (checked on the probe carrier: a Start
-- | button clicked while `timing` still emitted). Only removing
-- | the component silences it, and removal is `Hosting`'s — the DOM's —
-- | not the algebra's. So `provided` stays a carrier primitive, and
-- | `silence` is the *value* of what it removes, not an operand it is
-- | built from. (Deleted and restored on this argument; CHANGELOG, 2026-09-11.)
module Data.Profunctor.Row.RecordToVariant
  ( class RecordToVariant
  , recordToVariant
  , joined
  , bind
  , discard
  , replaying
  , subResolving
  , folding
  )
  where

import Control.Category (identity)
import Control.Semigroupoid ((>>>), (<<<))
import Data.Either (Either(..), either)
import Data.Function ((#))
import Data.Lens.Shutter (shutterE)
import Data.Profunctor (class Profunctor, dimap, rmap)
import Data.Profunctor.Coresolving (class Coresolving, coresolve)
import Data.Profunctor.Resolving (class Resolving)
import Data.Profunctor.Row (class ExclusiveRows, class FieldNames, class RowLabels, class SharedRecordInputs, class SharedVariantOutputs, splitRow, toCase)
import Data.Profunctor.Seeding (class Seeding, isHole, seeded)
import Data.Profunctor.Strong (class Strong, first)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (case_, expand, inj, on)
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList)
import Prim.RowList as RowList
import Record.Unsafe (unsafeSet)
import Record.Unsafe.Union (unsafeUnion)
import Type.Proxy (Proxy(..))

class Profunctor p <= RecordToVariant p where
  recordToVariant
    :: forall i1 o1 i2 o2 o12 o1x o2x i o
     . SharedRecordInputs i1 i2 i
    => SharedVariantOutputs o1 o2 o o12 o1x o2x
    => p { | i1 } [ | o1 ]
    -> p { | i2 } [ | o2 ]
    -> p { | i } [ | o ]

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

-- | Replay the last fed row, mapped by `f`, as case `l` on each occurrence of the source.
-- | The source is fed the row it replays, whole. An **occurrence source**:
-- | one case carrying `{}`, the row pinned by `RowToList` so the one label is
-- | read off it — a source with a payload, or with several cases, is a type
-- | error rather than a silent discard. Derived: `joined` at the empty
-- | payload, the event dropped and the case renamed to `l`, so replay is
-- | `Strong`'s retention and nothing else.
replaying
  :: forall @l p r c v1 v2 k v
   . Strong p
  => IsSymbol l
  => IsSymbol c
  => RowToList v1 (RowList.Cons c {} RowList.Nil)
  => Cons c {} () v1
  => Cons c { event :: {}, model :: { | r } } () v2
  => Cons l k () v
  => ({ | r } -> k)
  -> p { | r } [ | v1 ]
  -> p { | r } [ | v ]
replaying f src = joined @c src # rmap (on (Proxy @c) (inj (Proxy @l) <<< f <<< _.model) case_)

-- | An event joined with the row its emitter was fed: the `×→+` stage's
-- | `Strong` retention, exposed. `first` around the source — the fed row
-- | rides the state channel and leaves beside each occurrence's payload as
-- | one record, `{ event, model }`, under the same case. So a list pick, a
-- | canvas click or a tick reaches the fold with the model in hand, and its
-- | handler is one function of one record (`listOf … # joined @"toggled"`,
-- | `unshown @"Todo toggled" # fold toggleTodo` with `toggleTodo ::
-- | { event :: Int, model :: { … } } -> { … }`, todo-list). `replaying @l f`
-- | is the degenerate case where the occurrence carries nothing of its
-- | own.
joined
  :: forall @l p r a v v1
   . Strong p
  => IsSymbol l
  => Cons l a () v
  => Cons l { event :: a, model :: { | r } } () v1
  => p { | r } [ | v ]
  -> p { | r } [ | v1 ]
joined src = toCase @l (dimap (\r -> Tuple r r) (\(Tuple e r) -> { event: on (Proxy @l) identity case_ e, model: r }) (first src))

-- | Focus a sub-record of the input, wrapping the background into output
-- | case `w`: the `Shutter` optic's row form over the strength `Resolving`.
-- | The focus is fed exactly its labels (`splitRow`); the background, the
-- | rest of the fed row, escapes as case `w` once the focus quiesces.
subResolving
  :: forall @w p r1 rl b r v1 v2 v3
   . Resolving p
  => IsSymbol w
  => ExclusiveRows r1 b r
  => RowToList r1 rl
  => FieldNames rl r r1
  => RowLabels rl
  => Cons w { | b } v1 v2
  => Union v1 v3 v2
  => p { | r1 } [ | v1 ]
  -> p { | r } [ | v2 ]
subResolving g =
  shutterE
    splitRow
    (either expand (inj (Proxy @w)))
    g

-- | Fold state field `l` through case `w` until another case exits, seeded
-- | with its initial value: the terminating fold at one field, the
-- | `Coshutter` optic's row form over the co-strength `Coresolving`. The
-- | state is one field labelled on the view line (`folding @"next" @"step"
-- | firstStep`); the loop case carries `{ l :: a }` and never leaves. A
-- | seed that is a hole is not injected (guardrails L18).
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
      -- the fold state is written over the fresh input, so a stale runtime
      -- copy of it never shadows the folded state
      (\(Tuple i fb) -> unsafeUnion fb i :: { | r2 })
      (on (Proxy @w) Right Left)
      (if isHole seed then g else g >>> seeded (inj (Proxy @w) (unsafeSet (reflectSymbol (Proxy @l)) seed {} :: { | r1 }))))
