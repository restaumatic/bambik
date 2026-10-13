-- | `Record → Record` (× → ×) row profunctors, organized (uniformly across
-- | the four shape modules) as:
-- |
-- |   * **strength** — `Strong` (ecosystem, with a `(->)` instance): the
-- |     unary power, the field lens. Its co-strength is the ecosystem's
-- |     `Costrong` (gated on `PUI`, so the knots at this shape need a
-- |     starting point: the whole-row `looped`, `Looping`'s —
-- |     `Data.Profunctor.Looping` — primed by its first feed, and the
-- |     one-field `feedback` over the seeded `PointedCostrong`),
-- |     and their optics are `Data.Lens.Lens` and `Data.Lens.Colens` —
-- |     neither the classes nor the optics mention a row, so none of them
-- |     lives here.
-- |   * **shape class** — `RecordToRecord`, the binary **merge**: the
-- |     one genuine per-carrier primitive, with its qualified-do sugar
-- |     (`bind`/`discard`).
-- |   * **free functions** — over the strength: `focusField` (the field
-- |     lens — the leaf lift, making every label-indexed editor a whole-row
-- |     citizen); over the pointed co-strength: `feedback @l` (the
-- |     `×`-trace at one hidden field, seeded — the `Colens` at row
-- |     granularity); over bare `Profunctor`: the rename `asField`. The
-- |     one-sided words of this shape — `atField`/`toField` (the halves of
-- |     the closed-singleton iso), `with` (the knot's closure, over the
-- |     point) and `settled` (the normalization) — live on the floor.
-- |
-- | A word lives where the sides it constrains do: one constraining both
-- | sides in its shape's module, one constraining a single side on the
-- | floor (`Data.Profunctor.Row`, beside `atAnyRow`/`widenVariantOutput`)
-- | — so no shape module imports another, and every module holds only
-- | its merge, its strength and trace forms, and the words that genuinely
-- | span both of its sides.
-- |
-- | The four single-label words mirror `VariantToVariant`'s one for one:
-- | `focusField`/`focusCase`, `asField`/`asCase`, `atField`/`atCase`,
-- | `toField`/`toCase`. Where they differ is reach, not form: a record
-- | leaf already emits its labelled row (guardrails L3) and reads its
-- | input through its own read function, so applications meet no bare
-- | record side to adopt — `atField`/`toField` are law-tested
-- | (test/Main.purs, with `focusField @l = toField @l <<< atField @l` at
-- | the closed singleton: the focus is derived from its halves there,
-- | never the halves from it) — while the mixed shapes' words hand bare values
-- | to and from cases all the time. `toField` closes its row where
-- | `toCase` leaves it open: a variant output may be widened, a record
-- | output is owned and exact.
-- |
-- | ## Laws at `×→×`
-- |
-- | The six laws of Data.Profunctor.Row ("The laws") read at this shape:
-- | a component `w :: p { | i } { | o }` is an **editor** of knowledge, or
-- | at `i = o` a **display** releasing the row it was fed; the merge is
-- | `m = recordToRecord w1 w2`,
-- | inputs shared (`SharedRecordInputs`: both operands are fed the merge's
-- | one row), outputs owned (`OwnedRecordOutputs`).
-- |
-- |   1. **Repetition** — owed: `feed x ; feed x ≈ feed x`.
-- |   2. **Answer** — a row, once: every feed is answered within its step
-- |      by at least one emission, all equal to the step's last; counting
-- |      renderings, exactly one. Leaves answer on the nose (`identity` is
-- |      the echo wire, `focusField @l` the echo with its background); a stage
-- |      may refine the liveness half in time (`confirmed`, the gather
-- |      gate). A display answers with the row it shows.
-- |   3. **Monoid** — unit the `{}` wire at the merge's row,
-- |      `e = lcmap (const {}) identity`, exact:
-- |      `recordToRecord e g = g = recordToRecord g e`;
-- |      symmetric and associative up to `≈`. The operands share one input
-- |      row, so the unit is the terminal arrow out of it (`identity` at
-- |      `{}` only when that row is `{}`). The merge has no unit of its own,
-- |      and the merge pinned at its unit is the operand itself, so this
-- |      shape names no introducer.
-- |   4. **Projection** — `π_k` is the whole row: every feed of `m`
-- |      reaches each operand whole, and nothing else does — a sibling's
-- |      emission never reaches it; cross-feed is `looped`'s, and only
-- |      there. `exact` trims:
-- |      `recordToRecord w1 w2 ≈ recordToRecord (rmap exactRow w1) w2`.
-- |   5. **Preservation** — `m` obeys 1 and 2: one release per feed, the
-- |      union of both answers, so a feed changing several fields never
-- |      emits a torn row; a one-sided emission is completed from the
-- |      sibling's last contribution.
-- |   6. **Monotonicity** — `w1 ⊑ w1'` implies
-- |      `recordToRecord w1 w2 ⊑ recordToRecord w1' w2`.
-- |
-- | On `PUI`: broadcast in one step, gate out (`PUI.Gate.gateStep`) — the
-- | participants are the owned field labels, each retaining its last
-- | value; the merge releases once every owned field is known, withholds
-- | and drops before. An operand owning no field enrols nothing, so it is
-- | born satisfied and never starves its sibling. A merge silent after its
-- | first feed has an operand breaking 2; one silent before any feed is
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
  , asField
  , feedback
  )
  where

import Data.Lens.Record (prop)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Row (class OwnedRecordOutputs, class SharedRecordInputs, exactRow, fedFirst, fedSecond)
import Data.Profunctor.PointedCostrong (class PointedCostrong, unfirstFrom)
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Prim.Row (class Cons)
import Record (get, insert, union) as Record
import Record.Unsafe (unsafeSet)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= RecordToRecord p where
  recordToRecord
    :: forall i1 o1 i2 o2 i o o1l o2l
     . SharedRecordInputs i1 i2 i
    => OwnedRecordOutputs o1 o2 o o1l o2l
    => p { | i1 } { | o1 }
    -> p { | i2 } { | o2 }
    -> p { | i } { | o }

instance RecordToRecord (->) where
  recordToRecord p1 p2 i = Record.union (exactRow (p1 (fedFirst i))) (exactRow (p2 (fedSecond i)))

bind
  :: forall p r2 r3 r1 r rl1 rl2
   . RecordToRecord p
  => OwnedRecordOutputs r2 r3 r rl1 rl2
  => p { | r1 } { | r2 }
  -> (p { | r1 } { | r2 } -> p { | r1 } { | r3 })
  -> p { | r1 } { | r }
bind first cont = recordToRecord first (cont first)

discard
  :: forall p r2 r3 r1 r rl1 rl2
   . RecordToRecord p
  => OwnedRecordOutputs r2 r3 r rl1 rl2
  => p { | r1 } { | r2 }
  -> (Unit -> p { | r1 } { | r3 })
  -> p { | r1 } { | r }
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

-- | Rename a component's singleton field `c` to business field `l` on both sides.
asField
  :: forall @c @l p a b r r1 r2 r3
   . IsSymbol c
  => IsSymbol l
  => Profunctor p
  => Cons c a () r2
  => Cons c b () r3
  => Cons l a () r
  => Cons l b () r1
  => p { | r2 } { | r3 }
  -> p { | r } { | r1 }
asField = dimap (\r -> Record.insert (Proxy @c) (Record.get (Proxy @l) r) {}) (\r -> Record.insert (Proxy @l) (Record.get (Proxy @c) r) {})

-- | Loop state field `l` of the output back into the input, starting it at
-- | the given value: the `×`-trace at one field, the `Colens` optic's row
-- | form over the pointed trace `PointedCostrong`. The state is one field,
-- | labelled on the view line (`feedback @"top" noBids`), so the split is a
-- | `Cons` at a label the view states. The field is written over the fresh
-- | input, so a stale runtime copy of it never shadows the looped state.
-- | Where the state is the model's, the knot is `looped` and the state a
-- | model field; `feedback` hides it from the rows on both sides.
feedback
  :: forall @l @a p r r1 r2 r3
   . IsSymbol l
  => PointedCostrong p
  => Cons l a r r2
  => Cons l a r1 r3
  => a
  -> p { | r2 } { | r3 }
  -> p { | r } { | r1 }
feedback seed g =
  unfirstFrom seed
    (dimap
      (\(Tuple i a) -> unsafeSet (reflectSymbol (Proxy @l)) a i :: { | r2 })
      (\ow -> Tuple (unsafeCoerce ow) (Record.get (Proxy @l) ow))
      g)
