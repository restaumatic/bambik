-- | `Variant → Record` (+ → ×) row profunctors, organized (uniformly across
-- | the four shape modules) as:
-- |
-- |   * **strength** — `Retaining` (`Data.Profunctor.Retaining`; `PUI m`
-- |     instances only, no `(->)`): the unary power, a Mealy/coroutine step.
-- |     Its co-strength `Coretaining` is in `Data.Profunctor.Coretaining`,
-- |     and their optics are `Data.Lens.Reel` and `Data.Lens.Coreel` —
-- |     neither the classes nor the optics mention a row, so none of them
-- |     lives here.
-- |   * **shape class** — `VariantToRecord`, the binary **merge**: the
-- |     one genuine per-carrier primitive, with its qualified-do sugar
-- |     (`bind`/`discard`).
-- |   * **free functions** — over the strength: `subRetaining` (a
-- |     sub-variant, the background wrapped as a field); over `Category`:
-- |     `fold @l f` (one case folding into the record, `f` of its payload —
-- |     the merge pinned at its unit, under `atCase @l`); over the co-strength
-- |     `Coretaining`: `unfolding @w @l` (the productive unfold at row
-- |     granularity, the `Coreel` optic's row form).
-- |
-- | A word lives in the module of the sides it constrains: one polymorphic
-- | on one side sits in the diagonal module of the side it constrains, so
-- | the mixed modules hold only their strength, their trace and the words
-- | that genuinely span both sides.
-- | The variant-input words `atCase` and `forCase` are therefore
-- | `VariantToVariant`'s. A label `@w` appears exactly where a row is
-- | wrapped as one field or case to cross the shape change
-- | (`subRetaining`, `unfolding`).
-- |
-- | ## Laws at `+→×`
-- |
-- | The six laws of Data.Profunctor.Row ("The laws") read at this shape:
-- | a component `w :: p [ | i ] { | o }` is a **fold** of occurrences into
-- | retained state, or at `o = {}` a **status**; the merge is
-- | `m = variantToRecord w1 w2`, inputs owned (`OwnedVariantInputs`),
-- | outputs owned (`OwnedRecordOutputs`).
-- |
-- |   1–2. **Repetition, Answer** — not owed. A variant input is
-- |      dispatched to one owner and `≈` counts every occurrence: a fold
-- |      steps on each, a status renders each, and whether an occurrence
-- |      releases is the fold's; a status owes the channel nothing. What
-- |      is released is a `{ | o }` — whole by type — and before the
-- |      retained state exists a release needing it is withheld, not
-- |      invented, which is why `unfolding`/`accumulated` take a seed.
-- |   3. **Monoid** — unit `lcmap case_ identity :: p (Variant ()) {}`,
-- |      exact: `variantToRecord (lcmap case_ identity) g = g =
-- |      variantToRecord g (lcmap case_ identity)`; symmetric and
-- |      associative up to `≈`. Never fed and owning no field, the unit's
-- |      side is born spoken, so any silent element of that type serves
-- |      equally (`silence` at `b = ()` is one). The merge pinned at it —
-- |      one case folding into the record — is `fold @l f` (below): `f` of
-- |      the payload under `atCase @l`, exported 2026-10-02 for the
-- |      counter's loop, whose `+→×` stage folds the click back into the
-- |      model row (`fold @"Count" increment`).
-- |   4. **Projection** — `π_k` is the operand's own cases: an occurrence
-- |      of case `l` reaches the one operand owning `l` and no other
-- |      (`DisjointLabels`), and nothing else reaches an operand. `exact`
-- |      trims: `variantToRecord w1 w2 ≈ variantToRecord (rmap exactRow w1) w2`.
-- |   5. **Preservation** — vacuous at the input: nothing is owed. What
-- |      the shape adds is that no torn row is reachable: dispatch feeds
-- |      one owner per occurrence, so a release is one fresh contribution
-- |      beside retained ones — every ingredient of tearing but a
-- |      broadcast.
-- |   6. **Monotonicity** — `w1 ⊑ w1'` implies
-- |      `variantToRecord w1 w2 ⊑ variantToRecord w1' w2`.
-- |
-- | On `PUI`: dispatch in, gate out — the gate shared with `×→×`
-- | (`PUI.Gate.gateStep`): retain each side's last contribution, release
-- | their union once both owned sides have spoken, withhold before; the
-- | step is kept so a re-entrant echo during an occurrence coalesces into
-- | one release. Releasing on every occurrence once the row is whole is
-- | the carrier's permitted choice (no row needs `Eq`), not a law. A merge
-- | silent once both sides have spoken has an operand that never
-- | releases; one silent before that waits on an owned field's first
-- | occurrence — prime it (`unfolding`'s seed, `seeded`). Probes carry law
-- | and shape in test/Main.purs; 3, 4 and 6 and the gate's conformance to
-- | its pure step run over every script to a bound in test/Exhaustive.purs.
module Data.Profunctor.Row.VariantToRecord
  ( class VariantToRecord
  , variantToRecord
  , bind
  , discard
  , subRetaining
  , fold
  , unfolding
  )
  where

import Control.Category (class Category, identity)
import Control.Semigroupoid ((>>>))
import Data.Either (either)
import Data.Lens.Reel (reelE)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Coretaining (class Coretaining, coretain)
import Data.Profunctor.Retaining (class Retaining)
import Data.Profunctor.Row (class ExclusiveRows, class OwnedRecordOutputs, class OwnedVariantInputs, splitVariant)
import Data.Profunctor.Seeding (class Seeding, isHole, seeded)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (class Contractable, case_, expand, inj, on)
import Prim.Row (class Cons, class Union)
import Record.Unsafe (unsafeSet)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= VariantToRecord p where
  variantToRecord
    :: forall i1 i1l i2 i2l o1 o2 i o o1l o2l
     . OwnedVariantInputs i1 i2 i i1l i2l
    => OwnedRecordOutputs o1 o2 o o1l o2l
    => p [ | i1 ] { | o1 }
    -> p [ | i2 ] { | o2 }
    -> p [ | i ] { | o }

bind
  :: forall p v1 rl1 v2 rl2 r1 r2 v r rl3 rl4
   . VariantToRecord p
  => OwnedVariantInputs v1 v2 v rl1 rl2
  => OwnedRecordOutputs r1 r2 r rl3 rl4
  => p [ | v1 ] { | r1 }
  -> (p [ | v1 ] { | r1 } -> p [ | v2 ] { | r2 })
  -> p [ | v ] { | r }
bind first cont = variantToRecord first (cont first)

discard
  :: forall p v1 rl1 v2 rl2 r1 r2 v r rl3 rl4
   . VariantToRecord p
  => OwnedVariantInputs v1 v2 v rl1 rl2
  => OwnedRecordOutputs r1 r2 r rl3 rl4
  => p [ | v1 ] { | r1 }
  -> (Unit -> p [ | v2 ] { | r2 })
  -> p [ | v ] { | r }
discard first cont = bind first (\_ -> cont unit)

-- | Focus a sub-variant of the input, wrapping the background into output field `w`.
subRetaining
  :: forall @w p v1 b v r1 r
   . Retaining p
  => IsSymbol w
  => ExclusiveRows v1 b v
  => Contractable v v1
  => Contractable v b
  => Cons w [ | b ] r1 r
  => p [ | v1 ] { | r1 }
  -> p [ | v ] { | r }
subRetaining g =
  reelE
    splitVariant
    -- no `Lacks`: `unsafeSet` realizes the layout `Cons w [ | b ] b' s'` pins —
    -- first-label convention, as `inj`/`on`.
    (\(Tuple b' bg) -> unsafeSet (reflectSymbol (Proxy @w)) bg b')
    g

-- | One case folding into the record: the closed singleton `[ l :: a ]`
-- | consumed by `f`, which turns its payload into the row — `atCase @l` of
-- | the function, the `+→×` merge pinned at its unit. The payload of a
-- | replaying emitter *is* the row it was fed, so `f` updates the model it
-- | already holds, and nothing need be retained (counter's
-- | `fold @"Count" increment`; `mvu` around it supplies the loop). Laws on
-- | `(->)`: `fold @l f (inj @l a) = f a`; at `identity` it is the closed
-- | singleton unwrapped to its row, an iso with `toCase @l identity` both
-- | ways — `toCase @l identity identity >>> fold @l identity = identity` on
-- | `{ | r }` and `fold @l identity >>> toCase @l identity identity =
-- | identity` on the singleton. A retaining, seeded `fold` was tried
-- | 2026-10-03/04 and reverted: the payload already carries the model.
fold
  :: forall @l p a r v
   . IsSymbol l
  => Cons l a () v
  => Profunctor p
  => Category p
  => (a -> { | r })
  -> p [ | v ] { | r }
fold f = dimap (on (Proxy @l) identity case_) f identity

-- | Resume state field `l` of every emission as case `w`, seeded with its initial value.
-- | The state is one field labelled on the view line (`unfolding @"resume"
-- | @"next" firstTicket`); case `w` carries `{ l :: a }`. A seed that is a
-- | hole is not injected (guardrails L18).
unfolding
  :: forall @w @l @a p v r1 v1 v2 r r2
   . Seeding p
  => Coretaining p
  => IsSymbol w
  => IsSymbol l
  => Cons l a () r1
  => Cons w { | r1 } v v1
  => Union v v2 v1
  => Cons l a r r2
  => a
  -> p [ | v1 ] { | r2 }
  -> p [ | v ] { | r }
unfolding seed g =
  coretain
    (dimap
      (either expand (inj (Proxy @w)))
      (\ow -> Tuple (unsafeCoerce ow) (unsafeCoerce ow))
      (if isHole seed then g else seeded (inj (Proxy @w) (unsafeSet (reflectSymbol (Proxy @l)) seed {} :: { | r1 })) >>> g))
