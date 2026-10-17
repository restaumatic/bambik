-- | `Record → Record` (× → ×) row profunctors, organized (uniformly across
-- | the four shape modules) as:
-- |
-- |   * **strength** — `Strong` (ecosystem, with a `(->)` instance): the
-- |     unary power, the field lens. Its co-strength is the ecosystem's
-- |     `Costrong` (gated on `PUI`, so the knot at this shape needs a
-- |     starting point: the whole-row `looped`, the carrier's, primed by
-- |     its first feed),
-- |     and their optics are `Data.Lens.Lens` and `Data.Lens.Colens` —
-- |     neither the classes nor the optics mention a row, so none of them
-- |     lives here.
-- |   * **shape class** — `RecordToRecord`, the binary **merge**: the
-- |     one genuine per-carrier primitive, with its qualified-do sugar
-- |     (`bind`/`discard`).
-- |   * **free functions** — over the strength: `focusField` (the field
-- |     lens — the leaf lift, making every label-indexed editor a whole-row
-- |     citizen). No co-strength row form: a loop's state is a model
-- |     field the whole-row `looped` carries, never a hidden one
-- |     (`Data.Profunctor.Row`, *What is deliberately not here*). The
-- |     one-sided words of this shape — `atField`/`toField` (the halves of
-- |     the closed-singleton iso) and `settled` (the normalization) —
-- |     live on the floor; the knot's closure `with`, over the point, is
-- |     the carrier's (`PUI`).
-- |
-- | A word lives where the sides it constrains do: one constraining both
-- | sides in its shape's module, one constraining a single side on the
-- | floor (`Data.Profunctor.Row`)
-- | — so no shape module imports another, and every module holds only
-- | its merge, its strength and trace forms, and the words that genuinely
-- | span both of its sides.
-- |
-- | The three single-label words mirror `VariantToVariant`'s one for one:
-- | `focusField`/`focusCase`, `atField`/`atCase`, `toField`/`toCase`. Where they differ is reach, not form: a record
-- | leaf already emits its labelled row (guardrails L3) and reads its
-- | input through its own read function, so applications meet no bare
-- | record side to adopt — `atField`/`toField` are law-tested
-- | (test/Main.purs, with `focusField @l = toField @l <<< atField @l` at
-- | the closed singleton: the focus is derived from its halves there,
-- | never the halves from it) — while the mixed shapes' words hand bare values
-- | to and from cases all the time. The owned halves close their row to
-- | the one label (`atField`/`atCase`, `toField`: `Cons l a () r`, `Cons
-- | l a () v`), the shared half leaves it open (`toCase`: `Cons l a b v`,
-- | the case sitting in the row the `×→+` block shares).
-- |
-- | ## Laws at `×→×`
-- |
-- | The six laws of Data.Profunctor.Row ("The laws") read at this shape:
-- | a component `w :: p { | i } { | o }` is an **editor** of knowledge, or
-- | at `i = o` a **display** releasing the row it was fed; the merge is
-- | `m = recordToRecord w1 w2`,
-- | inputs shared (one row `i`: both operands are fed the merge's whole
-- | row), outputs owned (`OwnedRecordOutputs`).
-- |
-- |   1. **Repetition** — owed: `feed x ; feed x ≈ feed x`.
-- |   2. **Answer** — a row, once: every feed is answered within its step
-- |      by at least one emission, all equal to the step's last; counting
-- |      renderings, exactly one. A **leaf** law: leaves answer on the nose
-- |      (`identity` is the echo wire, `focusField @l` the echo with its
-- |      background), a display answers with the row it shows, and a stage
-- |      built from them without a variant keeps it — by `>>>`, by 5, by
-- |      the `×→×` words — refining the liveness half at most in time
-- |      (the gather gate). A route through a variant,
-- |      `emitter >>> fold`, is `×→×` by type and answers no feed: it
-- |      releases on occurrences, owing only that each release is a
-- |      whole row. That stage is every loop's body and is lawful as
-- |      such; it is only no answering operand, so what must show at
-- |      mount stands before it, never after.
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
-- | from a breach; one silent before any feed is
-- | unprimed (`with`, `seeded`, or a source, pointed by construction — the
-- | merge's application use, sources each owning its field and released
-- | together at registration). Probes carry law and shape in test/Main.purs;
-- | 3–6, with 5's two clauses, run over every script to a bound in
-- | test/Exhaustive.purs.
module Data.Profunctor.Row.RecordToRecord
  ( class RecordToRecord
  , recordToRecord
  , bind
  , discard
  , focusField
  )
  where

import Data.Lens.Record (prop)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Row (class OwnedRecordOutputs, exactRow)
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol)
import Data.Unit (Unit, unit)
import Prim.Row (class Cons)
import Record (union) as Record
import Type.Proxy (Proxy(..))

class Profunctor p <= RecordToRecord p where
  recordToRecord
    :: forall i o1 o2 o o1l o2l
     . OwnedRecordOutputs o1 o2 o o1l o2l
    => p { | i } { | o1 }
    -> p { | i } { | o2 }
    -> p { | i } { | o }

instance RecordToRecord (->) where
  recordToRecord p1 p2 i = Record.union (exactRow (p1 i)) (exactRow (p2 i))

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
