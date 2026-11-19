-- | `Record → Record` (× → ×) row profunctors:
-- |
-- |   * **strength** — `Strong` (ecosystem, with a `(->)` instance): the
-- |     field lens. Its co-strength is the ecosystem's `Costrong` (gated
-- |     on `PUI`, so the knot at this shape is the whole-row `looped`, the
-- |     carrier's, primed by its first feed); their optics are
-- |     `Data.Lens.Lens` and `Data.Lens.Colens`. None of them mentions a
-- |     row, so none lives here.
-- |   * **shape class** — `RecordToRecord`, the binary **merge**, with its
-- |     qualified-do sugar (`bind`/`discard`).
-- |   * **free functions** — over the strength: `focusField` (the leaf
-- |     lift, making every label-indexed editor a whole-row citizen) and
-- |     `onField` (a field computed from the row and added to it, the
-- |     mirror of `VariantToVariant.onCase`). No co-strength row form: a
-- |     loop's state is a model field the whole-row `looped` carries
-- |     (`Data.Profunctor.Row`, *What is deliberately not here*). The
-- |     one-sided words of this shape — `atField`/`toField` and `settled`
-- |     — live on the floor (`Data.Profunctor.Row`, the single-label grid,
-- |     with `focusField @l = toField @l <<< atField @l` at the closed
-- |     singleton law-tested in test/Main.purs); the knot's closure `with`
-- |     is the carrier's (`PUI`).
-- |
-- | ## What the shape models
-- |
-- | A `×→×` row is the **view model**: the one row the screen and the
-- | app share, the row the loop re-enters whole through `looped` and
-- | the seed's signature states. Everything typed at this shape holds,
-- | shows, edits or normalizes it — editors and selectors (`focusField
-- | @l` over a leaf), displays (fed the row, releasing the row they
-- | show), sources (each owning the field it knows) and `settled` (a
-- | correction of the row by a function of it: a bid raised to the top,
-- | a selection the filter hid dropped). Every field has the type the
-- | screen represents exactly — a text field's is `String`, a slider's a
-- | number, a selector's its option's case, a number field's the two-case
-- | `[ number, invalid ]` that keeps the text that is not one — so what
-- | the row says is what the user sees.
-- |
-- | The **data model** is never a row here. It enters and leaves on the
-- | variant edges, as the payload of an event: in through a fold
-- | (`capture @l`, `fold @l f`, `load f` at mount), out through an action
-- | (`action @l f`, `f :: row -> Aff outcomes`, where the view model is
-- | parsed into what the domain takes and a failed parse is one of the
-- | outcomes). What `×→×` itself transforms is lawful only as long as it
-- | stays total: a `dimap f g` bracket on an editor is an iso lens (a
-- | lossy `g` loses what the user typed on the next broadcast), `settled`
-- | rewrites the row whole, and `introduced @l init` grows it by a field
-- | the user then owns.
-- |
-- | What the shape cannot model is a **decision**. A stage that could say
-- | "no" to a row — a parse that fails, a validation — would withhold,
-- | and in `>>>` withholding reaches nothing: the stages after it keep
-- | the row they last held and an emitter among them replays it, so a
-- | failure shown at the field is a stale value sent from the button
-- | beside it. A decision is a sum, and a sum is a **classifier**, a
-- | function of the row returning a variant, handed to a pane (`inCase
-- | @l f` for displays and editors, `provided @l f` for emitters), which
-- | **detaches** its content on every other case — the only safe form of
-- | withholding, and why a classifier is a pane's argument and never a
-- | stage. A validating stage `p { l :: String } { l' :: Number }` was
-- | considered and rejected on these grounds (2026-11-06).
-- |
-- | ## The two laws
-- |
-- | The shape owes its carrier exactly two things, both obligations of a
-- | **record input** (Data.Profunctor.Row, "The laws", 1 and 2): a record
-- | input is shared, one feed reaching every operand of a merge, so the
-- | boundary can be owed at most one thing back. A component `w :: p { |
-- | i } { | o }` is an **editor** of the row, or at `i = o` a **display**
-- | releasing the row it was fed.
-- |
-- |   1. **Repetition** — `feed x ; feed x ≈ feed x`. Showing the same
-- |      row twice is showing it once: a feed writes the view, it never
-- |      accumulates, so a loop may re-broadcast the model freely.
-- |   2. **Answer** — a row, once: every feed is answered within its step
-- |      by at least one emission, all equal to the step's last; counting
-- |      renderings, exactly one. The row downstream is therefore always
-- |      whole and current, never torn and never stale. A **leaf** law:
-- |      leaves answer on the nose (`identity` is the echo wire,
-- |      `focusField @l` the echo with its background), a display
-- |      answers with the row it shows, and a stage built from them
-- |      without a variant keeps it — by `>>>`, by 5, by the `×→×`
-- |      words — refining the liveness half at most in time (the gather
-- |      gate). A route through a variant, `emitter >>> fold`, is `×→×`
-- |      by type and answers no feed: it releases on occurrences, owing
-- |      only that each release is a whole row. That stage is every
-- |      loop's body and is lawful as such; it is only no answering
-- |      operand, so what must show at mount stands before it, never
-- |      after.
-- |
-- | The type cannot carry either law; they are the protocol a vocabulary
-- | provider discharges at its leaves (`PUI.recordToRecordWiring`), and
-- | the leaf-law bench checks them on the real DOM.
-- |
-- | ## The merge at `×→×`
-- |
-- | The four merge laws of Data.Profunctor.Row read here, for
-- | `m = recordToRecord w1 w2`, inputs shared (one row `i`: both
-- | operands are fed the merge's whole row), outputs owned
-- | (`OwnedRecordOutputs`) — the **pairing** `⟨f, g⟩ : C -> A × B` of the
-- | product:
-- |
-- |   3. **Symmetry and associativity** — up to `≈`.
-- |   4. **Projection** — `π_k` is the whole row: every feed of `m`
-- |      reaches each operand whole, and nothing else does — a sibling's
-- |      emission never reaches it; cross-feed is `looped`'s, and only
-- |      there. `exact` trims:
-- |      `recordToRecord w1 w2 ≈ recordToRecord (rmap exactRow w1) w2`.
-- |   5. **Preservation** — if `w1` and `w2` obey 1 and 2, `m` does: one
-- |      release per feed, the union of both answers, so a feed changing
-- |      several fields never emits a torn row; a one-sided emission is
-- |      completed from the sibling's last contribution. The no-torn
-- |      clause holds of every operand; the one-release clause needs
-- |      both to answer.
-- |   6. **Monotonicity** — `w1 ⊑ w1'` implies
-- |      `recordToRecord w1 w2 ⊑ recordToRecord w1' w2`.
-- |
-- | On `PUI`: broadcast in one step, gate out (`PUI.Gate.gateStep`) — the
-- | participants are the owned field labels, each retaining its last
-- | value; the merge releases once every owned field is known, withholds
-- | and drops before. An operand owning no field enrols nothing, so it is
-- | born satisfied and never starves its sibling. A merge silent after its
-- | first feed has an operand that did not answer — a leaf breaking 2, or
-- | an event-driven stage (a route through a variant) owning fields,
-- | which answers on its occurrence and which the watchdog cannot tell
-- | from a breach; one silent before any feed is unprimed (`with`,
-- | `seeded`, or a source, pointed by construction — the merge's
-- | application use, sources each owning its field and released together
-- | at registration). Probes carry law and shape in test/Main.purs; 3–6,
-- | with 5's two clauses, run over every script to a bound in
-- | test/Exhaustive.purs.
module Data.Profunctor.Row.RecordToRecord
  ( class RecordToRecord
  , recordToRecord
  , bind
  , discard
  , focusField
  , onField
  )
  where

import Control.Category (identity)
import Data.Lens.Lens (lens)
import Data.Lens.Record (prop)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Row (class OwnedRecordOutputs)
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol)
import Data.Unit (Unit, unit)
import Prim.Row (class Cons, class Lacks)
import Record (insert) as Record
import Type.Proxy (Proxy(..))

class Profunctor p <= RecordToRecord p where
  recordToRecord
    :: forall i o1 o2 o o1l o2l
     . OwnedRecordOutputs o1 o2 o o1l o2l
    => p { | i } { | o1 }
    -> p { | i } { | o2 }
    -> p { | i } { | o }

bind
  :: forall p i o1 o2 o o1l o2l
   . RecordToRecord p
  => OwnedRecordOutputs o1 o2 o o1l o2l
  => p { | i } { | o1 }
  -> (p { | i } { | o1 } -> p { | i } { | o2 })
  -> p { | i } { | o }
bind first cont = recordToRecord first (cont first)

discard
  :: forall p i o1 o2 o o1l o2l
   . RecordToRecord p
  => OwnedRecordOutputs o1 o2 o o1l o2l
  => p { | i } { | o1 }
  -> (Unit -> p { | i } { | o2 })
  -> p { | i } { | o }
discard first cont = bind first (\_ -> cont unit)

-- | The field lens: lift a component editing field `l` into a whole-row citizen that retains the rest of the row.
focusField
  :: forall @l p f f' b r r1
   . IsSymbol l
  => Cons l f b r
  => Cons l f' b r1
  => Strong p
  => p f f'
  -> p { | r } { | r1 }
focusField = prop (Proxy @l)

-- | Field `l` computed by a stage that reads the row, every other field
-- | passing through: the row **plus `l`** leaves — the `×→×` stage that
-- | introduces a field, and the mirror of `VariantToVariant.onCase`,
-- | which eliminates a case. As `onCase` is the prism whose builder is
-- | `identity`, this is the lens whose getter is `identity`: `Strong`'s
-- | `first` under the diagonal, the fed row riding beside the stage and
-- | the stage's output inserted into it. Law: at `(->)`, `onField @l f r
-- | = Record.insert l (f r) r`. `PUI.introduced` is this over the wire
-- | read through its init, followed by its editor.
onField
  :: forall @l p a b r
   . IsSymbol l
  => Lacks l b
  => Cons l a b r
  => Strong p
  => p { | b } a
  -> p { | b } { | r }
onField = lens identity (\b a -> Record.insert (Proxy @l) a b)
