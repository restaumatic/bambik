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
-- |   * **free functions** — over `Strong`: `joined @l` (an event joined
-- |     with the row its emitter was fed — replay is this at the empty
-- |     payload, `PUI.Web`'s click source dropping the `{}` event). No strength or co-strength row form: a
-- |     stage is typed at one row, so no word narrows it to a sub-record
-- |     with the rest escaping as a case, and a fold's state is a model
-- |     field, never a hidden one (`Data.Profunctor.Row`, *What is
-- |     deliberately not here*).
-- |
-- | A word lives where the sides it constrains do: one constraining both
-- | sides in its shape's module, one constraining a single side on the
-- | floor (`Data.Profunctor.Row`)
-- | — so no shape module imports another, and every module holds only
-- | its merge, its strength and trace forms, and the words that genuinely
-- | span both of its sides.
-- | `joined` takes `toCase` from the floor.
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
  )
  where

import Control.Category (identity)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Row (class SharedRecordInputs, class SharedVariantOutputs, toCase)
import Data.Profunctor.Strong (class Strong, first)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (case_, on)
import Prim.Row (class Cons)
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

-- | An event joined with the row its emitter was fed: the `×→+` stage's
-- | `Strong` retention, exposed. `first` around the source — the fed row
-- | rides the state channel and leaves beside each occurrence's payload as
-- | one record, `{ event, model }`, under the same case. So a list pick, a
-- | canvas click or a tick reaches the fold with the model in hand, and its
-- | handler is one function of one record (`listOf … # joined @"toggled"`,
-- | `unshown @"Todo toggled" # fold toggleTodo` with `toggleTodo ::
-- | { event :: Int, model :: { … } } -> { … }`, todo-list). A click is the
-- | degenerate case where the occurrence carries nothing of its own:
-- | `PUI.Web.clicked` is this word over its private click source with
-- | the `{}` event dropped and the case renamed — a derivation, so no
-- | word of its own (`Data.Profunctor.Row`, *Retention*).
joined
  :: forall @l p r a v v1
   . Strong p
  => IsSymbol l
  => Cons l a () v
  => Cons l { event :: a, model :: { | r } } () v1
  => p { | r } [ | v ]
  -> p { | r } [ | v1 ]
joined src = toCase @l (dimap (\r -> Tuple r r) (\(Tuple e r) -> { event: on (Proxy @l) identity case_ e, model: r }) (first src))
