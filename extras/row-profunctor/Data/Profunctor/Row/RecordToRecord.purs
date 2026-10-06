-- | `Record → Record` (× → ×) row profunctors, organized (uniformly across
-- | the four shape modules) as:
-- |
-- |   * **strength** — `Strong` (ecosystem, with a `(->)` instance): the
-- |     unary power, the field lens. Its co-strength is the ecosystem's
-- |     `Costrong` (gated on `PUI`, so the knot at this shape is
-- |     `Looping`'s `looped`, not a derivation — `Data.Profunctor.Looping`),
-- |     and their optics are `Data.Lens.Lens` and `Data.Lens.Colens` —
-- |     neither the classes nor the optics mention a row, so none of them
-- |     lives here.
-- |   * **shape class** — `RecordToRecord`, the binary **merge**: the
-- |     one genuine per-carrier primitive, with its qualified-do sugar
-- |     (`bind`/`discard`).
-- |   * **free functions** — over the strength: `subStrong` (a sub-record,
-- |     the background carried), `focusField` (the field lens — the leaf lift,
-- |     making every label-indexed editor a whole-row citizen) and, with
-- |     `Looping`, `bracketed @l` (the sum-typed field editor); over the
-- |     wire and the point: `blank` (the faceless leaf), `with`
-- |     (`announce a >>> w` — discharge the initial-state obligation, so
-- |     `with seed (looped w)` is the app shape, the knot closed);
-- |     over bare `Profunctor`: the rename `asField`, the counit `muted`
-- |     and the normalization `settled`.
-- |
-- | A word lives in the module of the sides it constrains: one polymorphic
-- | on one side sits in the diagonal module of the side it constrains, so
-- | the mixed modules hold only their strength, their trace and the words
-- | that genuinely span both sides.
-- |
-- | There is no output-side adopter here — no `toField` beside
-- | `VariantToVariant.toCase`. Turning a value into an occurrence is a
-- | `dimap`, but turning an occurrence into a field value needs the rest of
-- | the row, and that is `focusField @l`'s retained background (or `updated`'s),
-- | never a reshaping; and since every component already emits a row
-- | (guardrails L3), there is no bare value to adopt.
-- |
-- | ## Laws at `×→×`
-- |
-- | The six laws of Data.Profunctor.Row ("The laws") read at this shape:
-- | a component `w :: p { | i } { | o }` is an **editor** of knowledge, or
-- | at `o = {}` a **display**; the merge is `m = recordToRecord w1 w2`,
-- | inputs shared (`SharedRecordInputs`: both operands are fed the merge's
-- | one row), outputs owned (`OwnedRecordOutputs`).
-- |
-- |   1. **Repetition** — owed: `feed x ; feed x ≈ feed x`.
-- |   2. **Answer** — a row, once: every feed is answered within its step
-- |      by at least one emission, all equal to the step's last; counting
-- |      renderings, exactly one. Leaves answer on the nose (`identity` is
-- |      the echo wire, `focusField @l` the echo with its background); a stage
-- |      may refine the liveness half in time (`confirmed`, the gather
-- |      gate). A display's answer is `{}`, which no gate awaits.
-- |   3. **Monoid** — unit the `{}` wire at the merge's row,
-- |      `blank = lcmap (const {}) identity`, exact:
-- |      `recordToRecord blank g = g = recordToRecord g blank`;
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
-- | On `PUI`: broadcast in one step, gate out (`PUI.Gate.gateStep`) —
-- | retain each side's last contribution, release once both owned sides
-- | have spoken, withhold and drop before. A merge silent after its first
-- | feed has an operand breaking 2; one silent before any feed is unprimed
-- | (`with`, `seeded`). Probes carry law and shape in test/Main.purs;
-- | 3–6, with 5's two clauses, run over every script to a bound in
-- | test/Exhaustive.purs.
module Data.Profunctor.Row.RecordToRecord
  ( class RecordToRecord
  , recordToRecord
  , bind
  , discard
  , subStrong
  , focusField
  , bracketed
  , blank
  , with
  , asField
  , muted
  , settled
  )
  where

import Control.Category (class Category, identity)
import Control.Semigroupoid ((>>>))
import Data.Function (const)
import Data.Lens.Record (prop)
import Data.Profunctor (class Profunctor, dimap, lcmap, rmap)
import Data.Profunctor.Looping (class Looping, looped)
import Data.Profunctor.Row (class FieldNames, class OwnedRecordOutputs, class SharedRecordInputs, exactRow)
import Data.Profunctor.Seeding (class Seeding, announce)
import Data.Profunctor.Strong (class Strong, first)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList)
import Record (get, insert, union) as Record
import Record.Unsafe.Union (unsafeUnion)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= RecordToRecord p where
  recordToRecord
    :: forall i1 o1 i2 o2 i12 i1x i2x i o o1l o2l
     . SharedRecordInputs i1 i2 i i12 i1x i2x
    => OwnedRecordOutputs o1 o2 o o1l o2l
    => p { | i1 } { | o1 }
    -> p { | i2 } { | o2 }
    -> p { | i } { | o }

instance RecordToRecord (->) where
  recordToRecord p1 p2 i = Record.union (exactRow (p1 (unsafeCoerce i))) (exactRow (p2 (unsafeCoerce i)))

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

-- | Focus a sub-record of the row, carrying the rest of the row unchanged.
-- | The focus is the component's own (closed) row and the background is
-- | inferred from the fed row by the forward `Union`s alone, so it cannot
-- | overlap the focus — and the fed row may be open, as it is while the
-- | logic that closes it is still a hole (guardrails L18).
subStrong
  :: forall p r1 r2 rl b r r3
   . Strong p
  -- forward only: the focus is the view's (closed), the background is
  -- inferred, so it cannot overlap the focus — and `r` may be open (L18)
  => Union r1 b r
  => Union r2 b r3
  => RowToList r2 rl
  => FieldNames rl r2 r2
  => p { | r1 } { | r2 }
  -> p { | r } { | r3 }
subStrong g =
  dimap (\s -> Tuple (unsafeCoerce s) (unsafeCoerce s))
        -- `exactRow` trims the emission to its declared row first: `g` may
        -- answer a feed *later* than the feed that stocked the retained
        -- background (a debounced inner stage), so a fat echo's runtime
        -- copies of background fields can be genuinely stale — the same
        -- hazard the gated merges trim (runtime-exactness).
        (\(Tuple f' b) -> unsafeUnion (exactRow f') b :: { | r3 })
        (first g)

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

-- | Edit the variant-valued field `l` through a record-shaped, self-looped editor state.
bracketed
  :: forall @l @v @r1 p b r
   . IsSymbol l
  => Cons l [ | v ] b r
  => Looping p
  => Strong p
  => ([ | v ] -> { | r1 })
  -> ({ | r1 } -> [ | v ])
  -> p { | r1 } { | r1 }
  -> p { | r } { | r }
bracketed f g w = focusField @l (dimap f g (looped w))

-- | The faceless leaf that reads nothing and contributes nothing, at any input.
blank :: forall p a. Category p => Profunctor p => p a {}
blank = lcmap (const {}) identity

-- | Discharge a chain's initial obligation by announcing its t=0 value.
-- | The seed is whatever the chain's first stage takes: a model row into
-- | the knot (`with seed (looped @( … ) w)`, the app shape) or into a
-- | knotless flow (potluck). Never `{}`: `body` feeds the terminal record
-- | once itself, so a load action standing before the knot runs on that
-- | feed with no seed written. Its
-- | own input is ignored, so it sits at any row; a seed that is a hole is
-- | never announced (`announce`, guardrails L18).
with :: forall @a p b r. Seeding p => a -> p a b -> p { | r } b
with a w = lcmap (const {}) (announce a >>> w)

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

-- | Render the component and deliberately discard its output.
muted :: forall p a b. Profunctor p => p a b -> p a {}
muted = rmap (const {})

-- | Normalize a stage's emissions with an idempotent function over the row.
-- | The normalizer is typed at the stage's row, its signature the view's
-- | hole hint verbatim (`{ "°C" :: String, "°F" :: String } -> { … }`).
settled
  :: forall p r a
   . Profunctor p
  => ({ | r } -> { | r })
  -> p a { | r }
  -> p a { | r }
settled f = rmap f
