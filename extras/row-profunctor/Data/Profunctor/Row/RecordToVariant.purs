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
-- |     (`bind`/`discard`). It carries no unit, like the other merge
-- |     classes; theirs is `Category`'s `identity`, and this one, which no
-- |     wire reaches, is the carrier's silent component (`PUI`, `Cont`),
-- |     named by no class.
-- |   * **free functions** — over `Strong`: `joined`/`joined2` (a source's case joined
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
-- | the merge is `m = recordToVariant w1 w2`, inputs shared (one row
-- | `i`: both operands fed the merge's whole row), outputs shared (one
-- | row `o` every operand is typed at: an emitter open on its output
-- | sits in it by its label, an action by the `joined` that closes it).
-- |
-- |   1. **Repetition** — owed: `feed x ; feed x ≈ feed x`. A feed writes
-- |      the replay slot; a second write of the same row changes nothing.
-- |   2. **Answer** — nothing: a feed is answered by no emission. It
-- |      **arms** — every emission has a cause outside the channel (a
-- |      click, an `Aff` settling, quiescence) — which is what lets
-- |      a loop re-feed an emitter without firing it. What an
-- |      emission carries is the source's contract, not the shape's:
-- |      `clicked` replays the row last fed.
-- |   3. **Monoid** — unit the carrier's silent component (the others'
-- |      laws are conditional on `Category`; this one is stated per
-- |      carrier, `PUI`'s in test/Main.purs and test/Exhaustive.purs),
-- |      exact:
-- |      `recordToVariant silent g = g = recordToVariant g silent`;
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
-- | an absent one is the silent component (below). Probes carry law and shape in
-- | test/Main.purs (`answer ×→+` on the probe carrier's replay source; the
-- | real sources are clicked on the leaf-law bench); 3–6 run over every
-- | script to a bound in test/Exhaustive.purs.
-- |
-- | The silent component is also what an **absent event source is**. A `×→+` component that
-- | exists in one state and not in another
-- | (`button @"Start" # provided @"halted" phaseOf`) is, in the state
-- | where it is absent, observationally silent: fed nothing, emitting
-- | nothing. That is why the type is variant-output only — silent at
-- | *variant* output is a source with nothing to say, while silent at an
-- | inhabited *record* output would be a starved gate, which is why the
-- | record-side pane answers every feed instead — `inCase`, with the fed
-- | row. But absence is **not a copairing**: routing the input away
-- | from `w` leaves `w`'s own emission channel connected, so
-- | `lcmap f' (left w >>> right silent)` still emits when the source
-- | fires in the "absent" case (checked on the probe carrier: a Start
-- | button clicked while `timing` still emitted). Only removing
-- | the component silences it, and removal is `Hosting`'s — the DOM's —
-- | not the algebra's. So `provided` stays a carrier primitive, and
-- | the silent component is the *value* of what it removes, not an operand it is
-- | built from. (Deleted and restored on this argument; CHANGELOG, 2026-09-11.)
module Data.Profunctor.Row.RecordToVariant
  ( class RecordToVariant
  , recordToVariant
  , joined
  , joined2
  , kept
  , bind
  , discard
  )
  where

import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Strong (class Strong, first)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant.Internal (VariantRep(..))
import Unsafe.Coerce (unsafeCoerce)
import Prim.Row (class Cons)

class Profunctor p <= RecordToVariant p where
  recordToVariant
    :: forall i o
     . p { | i } [ | o ]
    -> p { | i } [ | o ]
    -> p { | i } [ | o ]

bind
  :: forall p i o
   . RecordToVariant p
  => p { | i } [ | o ]
  -> (p { | i } [ | o ] -> p { | i } [ | o ])
  -> p { | i } [ | o ]
bind first cont = recordToVariant first (cont first)

discard
  :: forall p i o
   . RecordToVariant p
  => p { | i } [ | o ]
  -> (Unit -> p { | i } [ | o ])
  -> p { | i } [ | o ]
discard first cont = bind first (\_ -> cont unit)

-- | **Case `l` joined with the row its emitter was fed** (`×→+`):
-- | `first` around the source, the fed row riding the state channel and
-- | leaving beside the occurrence's payload as one record, `{ event,
-- | model }`, under the same case. So a
-- | list pick, a canvas click or an action's outcome reaches the fold
-- | with the model in hand, and its handler is one function of one
-- | record (`listOf @"Todo toggled" … # joined @"Todo toggled"`,
-- | `fold @"Todo toggled" toggleTodo` with `toggleTodo ::
-- | { event :: Int, model :: { … } } -> { … }`, todo-list); an outcome
-- | whose fold keeps the model is `kept` (order-form's submit, joined with
-- | its button and action). The label **closes the source** to case `l`
-- | and widens the joined case into the row the block shares: with every
-- | emitter open on its output, nothing but the label says which case is
-- | the source's, and an action, typed at exactly its outcomes, has its
-- | row closed here and nowhere else in an emitter block (a label-free
-- | `joined` over every case of a closed source row was the word from
-- | 2026-10-14 to 2026-10-15; until 2026-10-16 the label left the
-- | source's other cases passing, and the action declared its outcome
-- | row on its line; CHANGELOG). A two-outcome source is `joined2`. A click is the degenerate
-- | case where the occurrence carries nothing of its own: `PUI.Web.clicked`
-- | is this with the `{}` event dropped and the case renamed — a
-- | derivation, so no word of its own (`Data.Profunctor.Row`, *Retention*).
joined
  :: forall @l p r e b v v1
   . Strong p
  => Cons l e () v
  => Cons l { event :: e, model :: { | r } } b v1
  => p { | r } [ | v ]
  -> p { | r } [ | v1 ]
joined = joinedEvery

-- | **Both cases of a two-outcome source joined** with the row it was fed:
-- | `joined` for a source emitting exactly `l1` and `l2`, the action of
-- | two outcomes (`… # action @"Submit order" submitOrder ) # joined2
-- | @"Order submitted" @"Order rejected: no total"`, order-form). Its own
-- | word because one label closes one case: two `joined`s would leave the
-- | source's rest open to the block's other cases, and only both labels
-- | at once say the source has these two and no others.
joined2
  :: forall @l1 @l2 p r e1 e2 b v0 v v1 v2
   . Strong p
  => Cons l2 e2 () v0
  => Cons l1 e1 v0 v
  => Cons l2 { event :: e2, model :: { | r } } b v1
  => Cons l1 { event :: e1, model :: { | r } } v1 v2
  => p { | r } [ | v ]
  -> p { | r } [ | v2 ]
joined2 = joinedEvery

joinedEvery :: forall p r v v1. Strong p => p { | r } [ | v ] -> p { | r } [ | v1 ]
joinedEvery src = dimap (\r -> Tuple r r) (\(Tuple e r) -> withModel r e) (first src)
  where
  withModel :: { | r } -> [ | v ] -> [ | v1 ]
  withModel r e = case (unsafeCoerce e :: VariantRep _) of
    VariantRep rep -> unsafeCoerce (VariantRep { type: rep.type, value: { event: rep.value, model: r } })

-- | The fold that **keeps the model** a joined event arrived with:
-- | `kept = _.model`. For an outcome that is told and changes nothing —
-- | `snackbar @"Order submitted" kept` (order-form), the
-- | action joined with its button so the model is in hand.
kept :: forall a r. { event :: a, model :: { | r } } -> { | r }
kept = _.model
