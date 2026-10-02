-- | `Record → Record` (× → ×) row profunctors, organized (uniformly across
-- | the four shape modules) as:
-- |
-- |   * **strength** — `Strong` (ecosystem, with a `(->)` instance): the
-- |     unary power, the field lens. Its trace is the pointed co-strength
-- |     `PointedCostrong` (`Data.Profunctor.PointedCostrong`: the
-- |     ecosystem's `Costrong` with its state channel started at a given
-- |     value), and their optics are `Data.Lens.Lens` and `Data.Lens.Colens`
-- |     — neither the classes nor the optics mention a row, so none of them
-- |     lives here.
-- |   * **shape class** — `RecordToRecord`, the binary **merge**: the
-- |     one genuine per-carrier primitive, with its qualified-do sugar
-- |     (`bind`/`discard`).
-- |   * **free functions** — over the strength: `subStrong` (a sub-record,
-- |     the background carried), `focusField` (the field lens — the leaf lift,
-- |     making every label-indexed editor a whole-row citizen) and, with
-- |     `Looping`, `bracketed @l` (the sum-typed field editor); over the
-- |     wire and the point: `blank` (the faceless leaf), `with`
-- |     (`announce a >>> w` — discharge the initial-state obligation) and
-- |     `mvu` (`with seed (looped w)` — the app shape); over bare
-- |     `Profunctor`: the rename `asField`, the counit `muted` and the
-- |     normalization `settled`; over the pointed co-strength: `feedback`
-- |     (the ×-trace at row granularity: one state field, labelled on the
-- |     view line, seeded with its starting value only).
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
-- | (`with`/`mvu`, `seeded`). Probes carry law and shape in test/Main.purs;
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
  , mvu
  , asField
  , muted
  , settled
  , feedback
  )
  where

import Control.Category (class Category, identity)
import Control.Semigroupoid ((>>>))
import Data.Function (const)
import Data.Lens.Record (prop)
import Data.Profunctor (class Profunctor, dimap, lcmap, rmap)
import Data.Profunctor.Looping (class Looping, looped)
import Data.Profunctor.PointedCostrong (class PointedCostrong, unfirstFrom)
import Data.Profunctor.Row (class FieldNames, class OwnedRecordOutputs, class SharedRecordInputs, exactRow)
import Data.Profunctor.Seeding (class Seeding, announce)
import Data.Profunctor.Strong (class Strong, first)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList)
import Record (get, insert, union) as Record
import Record.Unsafe (unsafeSet)
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
  :: forall p o1 o2 i o o1l o2l
   . RecordToRecord p
  => OwnedRecordOutputs o1 o2 o o1l o2l
  => p { | i } { | o1 }
  -> (p { | i } { | o1 } -> p { | i } { | o2 })
  -> p { | i } { | o }
bind first cont = recordToRecord first (cont first)

discard
  :: forall p o1 o2 i o o1l o2l
   . RecordToRecord p
  => OwnedRecordOutputs o1 o2 o o1l o2l
  => p { | i } { | o1 }
  -> (Unit -> p { | i } { | o2 })
  -> p { | i } { | o }
discard first cont = bind first (\_ -> cont unit)

-- | Focus a sub-record of the row, carrying the rest of the row unchanged.
-- | The focus is the component's own (closed) row and the background is
-- | inferred from the fed row by the forward `Union`s alone, so it cannot
-- | overlap the focus — and the fed row may be open, as it is while the
-- | logic that closes it is still a hole (guardrails L18).
subStrong
  :: forall p f f' f'l b s s'
   . Strong p
  -- forward only: the focus is the view's (closed), the background is
  -- inferred, so it cannot overlap the focus — and `s` may be open (L18)
  => Union f b s
  => Union f' b s'
  => RowToList f' f'l
  => FieldNames f'l f' f'
  => p { | f } { | f' }
  -> p { | s } { | s' }
subStrong g =
  dimap (\s -> Tuple (unsafeCoerce s) (unsafeCoerce s))
        -- `exactRow` trims the emission to its declared row first: `g` may
        -- answer a feed *later* than the feed that stocked the retained
        -- background (a debounced inner stage), so a fat echo's runtime
        -- copies of background fields can be genuinely stale — the same
        -- hazard the gated merges trim (runtime-exactness).
        (\(Tuple f' b) -> unsafeUnion (exactRow f') b :: { | s' })
        (first g)

-- | The field lens: lift a component editing field `l` into a whole-row citizen that retains the rest of the row.
focusField
  :: forall @l p f f' b s s'
   . IsSymbol l
  => Cons l f b s
  => Cons l f' b s'
  => Strong p
  => p f f'
  -> p { | s } { | s' }
focusField = prop (Proxy @l)

-- | Edit the variant-valued field `l` through a record-shaped, self-looped editor state.
bracketed
  :: forall @l @v p s b rs
   . IsSymbol l
  => Cons l [ | v ] b rs
  => Looping p
  => Strong p
  => ([ | v ] -> { | s })
  -> ({ | s } -> [ | v ])
  -> p { | s } { | s }
  -> p { | rs } { | rs }
bracketed f g w = focusField @l (dimap f g (looped w))

-- | The faceless leaf that reads nothing and contributes nothing, at any input.
blank :: forall p a. Category p => Profunctor p => p a {}
blank = lcmap (const {}) identity

-- | Discharge a component's initial-state obligation by announcing its t=0 value.
-- | Its own input is ignored, so it sits at any row; a seed that is a hole is
-- | never announced (`announce`, guardrails L18).
with :: forall @a p o r. Seeding p => { | a } -> p { | a } o -> p { | r } o
with a w = lcmap (const {}) (announce a >>> w)

-- | The model–view–update shape: a self-looped pipeline over the model, seeded with its initial state.
mvu
  :: forall @model p
   . Looping p
  => Seeding p
  => { | model }
  -> p { | model } { | model }
  -> p {} { | model }
mvu seed w = with seed (looped w)

-- | Rename a component's singleton field `c` to business field `l` on both sides.
asField
  :: forall @c @l p a b s t ci co
   . IsSymbol c
  => IsSymbol l
  => Profunctor p
  => Cons c a () ci
  => Cons c b () co
  => Cons l a () s
  => Cons l b () t
  => p { | ci } { | co }
  -> p { | s } { | t }
asField = dimap (\r -> Record.insert (Proxy @c) (Record.get (Proxy @l) r) {}) (\r -> Record.insert (Proxy @l) (Record.get (Proxy @c) r) {})

-- | Render the component and deliberately discard its output.
muted :: forall p i o. Profunctor p => p i o -> p i {}
muted = rmap (const {})

-- | Normalize a stage's emissions with an idempotent function over the row.
-- | The normalizer's footprint is its own signature, an open row
-- | (`forall r. { "°C" :: String, "°F" :: String | r } -> { … | r }`): by
-- | parametricity it touches exactly the fields it names.
settled
  :: forall p big i
   . Profunctor p
  => ({ | big } -> { | big })
  -> p i { | big }
  -> p i { | big }
settled f = rmap f

-- | Loop state field `l` of the output back into the input, starting it at the given value.
-- | The state is one field, labelled on the view line (`feedback @"top"
-- | noBids`), so the split is a `Cons` at a label the view states: checked
-- | whatever the logic has written, and never stuck on an open row. The
-- | field is written over the fresh input, so a stale runtime copy of it
-- | never shadows the looped state.
feedback
  :: forall @l @a p i o iw ow
   . IsSymbol l
  => PointedCostrong p
  => Cons l a i iw
  => Cons l a o ow
  => a
  -> p { | iw } { | ow }
  -> p { | i } { | o }
feedback seed g =
  unfirstFrom seed
    (dimap
      -- the state field is written over the fresh input, so a fat upstream
      -- emission's stale copy of it never shadows the looped state
      (\(Tuple i a) -> unsafeSet (reflectSymbol (Proxy @l)) a i :: { | iw })
      (\ow -> Tuple (unsafeCoerce ow) (Record.get (Proxy @l) ow))
      g)
