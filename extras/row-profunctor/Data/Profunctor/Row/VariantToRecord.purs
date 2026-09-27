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
-- |     sub-variant, the background wrapped as a field); over the
-- |     co-strength `Coretaining`: `unfolding @w` (the productive unfold at
-- |     row granularity, the `Coreel` optic's row form).
-- |
-- | A word lives in the module of the sides it constrains: one polymorphic
-- | on one side sits in the diagonal module of the side it constrains, so
-- | the mixed modules hold only their strength, their trace and the words
-- | that genuinely span both sides.
-- | The variant-input words `atCase` and `forCases` are therefore
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
-- |      one case folding into the record — is derivable and not exported.
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
  , unfolding
  )
  where

import Control.Semigroupoid ((>>>))
import Data.Either (either)
import Data.Lens.Reel (reelE)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Coretaining (class Coretaining, coretain)
import Data.Profunctor.Retaining (class Retaining)
import Data.Profunctor.Row (class ExclusiveRows, class OwnedRecordOutputs, class OwnedVariantInputs, splitVariant)
import Data.Profunctor.Seeding (class Seeding, seeded)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (class Contractable, expand, inj)
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
  :: forall p i1 i1l i2 i2l o1 o2 i o o1l o2l
   . VariantToRecord p
  => OwnedVariantInputs i1 i2 i i1l i2l
  => OwnedRecordOutputs o1 o2 o o1l o2l
  => p [ | i1 ] { | o1 }
  -> (p [ | i1 ] { | o1 } -> p [ | i2 ] { | o2 })
  -> p [ | i ] { | o }
bind first cont = variantToRecord first (cont first)

discard
  :: forall p i1 i1l i2 i2l o1 o2 i o o1l o2l
   . VariantToRecord p
  => OwnedVariantInputs i1 i2 i i1l i2l
  => OwnedRecordOutputs o1 o2 o o1l o2l
  => p [ | i1 ] { | o1 }
  -> (Unit -> p [ | i2 ] { | o2 })
  -> p [ | i ] { | o }
discard first cont = bind first (\_ -> cont unit)

-- | Focus a sub-variant of the input, wrapping the background into output field `w`.
subRetaining
  :: forall @w p f b s b' s'
   . Retaining p
  => IsSymbol w
  => ExclusiveRows f b s
  => Contractable s f
  => Contractable s b
  => Cons w [ | b ] b' s'
  => p [ | f ] { | b' }
  -> p [ | s ] { | s' }
subRetaining g =
  reelE
    splitVariant
    -- no `Lacks`: `unsafeSet` realizes the layout `Cons w [ | b ] b' s'` pins —
    -- first-label convention, as `inj`/`on`.
    (\(Tuple b' bg) -> unsafeSet (reflectSymbol (Proxy @w)) bg b')
    g

-- | Resume the state fields of every emission as case `w`, seeded with the unfold's initial state.
unfolding
  :: forall @w p i fb iw wx o ow
   . Seeding p
  => Coretaining p
  => IsSymbol w
  => Cons w { | fb } i iw
  => Union i wx iw
  => ExclusiveRows o fb ow
  => { | fb }
  -> p [ | iw ] { | ow }
  -> p [ | i ] { | o }
unfolding seed g =
  coretain
    (dimap
      (either expand (inj (Proxy @w)))
      (\ow -> Tuple (unsafeCoerce ow) (unsafeCoerce ow))
      (seeded (inj (Proxy @w) seed) >>> g))
