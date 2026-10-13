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
-- |   * **free functions** — over the strength: `subRetaining @w` (a
-- |     sub-variant focused, the other cases retained as field `w` — the
-- |     `Reel` at row granularity); over the co-strength `Coretaining`:
-- |     `unfolding @w @l` (the productive unfold, its state one field `l`
-- |     resumed as case `w` — the `Coreel` at row granularity); over
-- |     `Category`: `unshown @l` (the faceless leaf owning case `l`, what
-- |     opens a fold whose occurrence nothing needs to tell). The fold
-- |     itself is `PUI`'s, an optic over its status.
-- |
-- | A word lives where the sides it constrains do: one constraining both
-- | sides in its shape's module, one constraining a single side on the
-- | floor (`Data.Profunctor.Row`, beside `atAnyRow`/`widenVariantOutput`)
-- | — so no shape module imports another, and every module holds only
-- | its merge, its strength and trace forms, and the words that genuinely
-- | span both of its sides.
-- | A label `@w` appears exactly where a row is
-- | wrapped as one field to cross the shape change (`subRetaining`,
-- | `unfolding`).
-- |
-- | ## Laws at `+→×`
-- |
-- | The six laws of Data.Profunctor.Row ("The laws") read at this shape:
-- | a component `w :: p [ | v ] { | r }` is a **fold** of occurrences into
-- | the record, or a **status**, which releases nothing and so is typed at
-- | every row like `silence`; the merge is `m = variantToRecord w1 w2`,
-- | inputs owned (`OwnedVariantInputs`), the output row **shared** — the
-- | copairing `[f, g] : A + B -> R` of the coproduct. Each
-- | operand releases the whole row, so the merge forwards releases as they
-- | come and keeps no gate; the `×→×` merge, whose operands own fields, is
-- | the gated one.
-- |
-- |   1–2. **Repetition, Answer** — not owed. A variant input is
-- |      dispatched to one owner and `≈` counts every occurrence: a fold
-- |      steps on each, a status renders each, and whether an occurrence
-- |      releases is the fold's; a status owes the channel nothing. What
-- |      is released is a `{ | r }` — whole by type, the operand's own —
-- |      and where a release needs retained state (`accumulated`) it is
-- |      withheld, not invented, before that state exists.
-- |   3. **Monoid** — unit `lcmap case_ identity :: p (Variant ()) {}`,
-- |      exact: `variantToRecord (lcmap case_ identity) g = g =
-- |      variantToRecord g (lcmap case_ identity)`; symmetric and
-- |      associative up to `≈`. Never fed and owning no field, the unit's
-- |      side is born spoken, so any silent element of that type serves
-- |      equally (`silence` at `b = ()` is one). The merge pinned at it is
-- |      `fold @l f` (below), one case folded into the record
-- |      (`fold @"Count" increment`, counter); a loop's folds merge here one
-- |      per case, each releasing the whole next model, the statuses beside
-- |      them releasing nothing.
-- |   4. **Projection** — `π_k` is the operand's own cases: an occurrence
-- |      of case `l` reaches the one operand owning `l` and no other
-- |      (`DisjointLabels`), and nothing else reaches an operand. `exact`
-- |      trims: `variantToRecord w1 w2 ≈ variantToRecord (rmap exactRow w1) w2`.
-- |   5. **Preservation** — vacuous at the input: nothing is owed. What
-- |      the shape adds is that no torn row is reachable: dispatch feeds
-- |      one owner per occurrence and that owner releases a whole row, so
-- |      nothing is ever assembled from two operands.
-- |   6. **Monotonicity** — `w1 ⊑ w1'` implies
-- |      `variantToRecord w1 w2 ⊑ variantToRecord w1' w2`.
-- |
-- | On `PUI`: dispatch in, passage out — each operand's release exits as
-- | it occurs, nothing retained, no gate, no step (`Applicative m`
-- | suffices, as at `+→+`). The outputs were once owned and the merge
-- | gated like `×→×` (CHANGELOG, 2026-10-04); with a loop's folds one per
-- | case, each releasing the whole next model, the shared row is the
-- | honest type and the copairing the honest mechanism. Releasing on every occurrence is
-- | the fold's choice (no row needs `Eq`), not a law. A merge silent on an
-- | occurrence has an operand that chose not to release — a status. Probes carry law and shape
-- | in test/Main.purs; 3, 4 and 6 run over every script to a bound in
-- | test/Exhaustive.purs.
module Data.Profunctor.Row.VariantToRecord
  ( class VariantToRecord
  , variantToRecord
  , bind
  , discard
  , unshown
  , subRetaining
  , unfolding
  )
  where

import Control.Category (class Category, identity)
import Control.Semigroupoid ((>>>))
import Data.Either (either)
import Data.Lens.Reel (reelE)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Coretaining (class Coretaining, coretain)
import Data.Profunctor.Retaining (class Retaining)
import Data.Profunctor.Row (class ExclusiveRows, class OwnedVariantInputs, splitVariant)
import Data.Profunctor.Seeding (class Seeding, isHole, seeded)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (class Contractable, expand, inj)
import Record.Unsafe (unsafeSet)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Function (const)
import Data.Unit (Unit, unit)
import Prim.Row (class Cons, class Union)

class Profunctor p <= VariantToRecord p where
  variantToRecord
    :: forall v1 rl1 v2 rl2 v r
     . OwnedVariantInputs v1 v2 v rl1 rl2
    => p [ | v1 ] { | r }
    -> p [ | v2 ] { | r }
    -> p [ | v ] { | r }

bind
  :: forall p v1 rl1 v2 rl2 v r
   . VariantToRecord p
  => OwnedVariantInputs v1 v2 v rl1 rl2
  => p [ | v1 ] { | r }
  -> (p [ | v1 ] { | r } -> p [ | v2 ] { | r })
  -> p [ | v ] { | r }
bind first cont = variantToRecord first (cont first)

discard
  :: forall p v1 rl1 v2 rl2 v r
   . VariantToRecord p
  => OwnedVariantInputs v1 v2 v rl1 rl2
  => p [ | v1 ] { | r }
  -> (Unit -> p [ | v2 ] { | r })
  -> p [ | v ] { | r }
discard first cont = bind first (\_ -> cont unit)

-- | Case `l`, **unshown**: the faceless `+→×` leaf owning case `l` and
-- | showing nothing of it — `blank` with a name — wherever the case came
-- | from, an emitter (`unshown @"Count" # fold increment`) or a `+→+`
-- | block. It opens a fold or a tick source whose occurrence nothing needs
-- | to tell (`unshown @"Clock ticked" # ticks tickPeriod`), or declares an
-- | outcome case of an action that is only merged, never folded, where
-- | the merge needs the case named and there is no face to name it.
-- | `lcmap (const {}) identity`, like `blank`.
unshown :: forall @l p a v. Cons l a () v => Profunctor p => Category p => p [ | v ] {}
unshown = lcmap (const {}) identity

-- | Focus a sub-variant of the input, wrapping the background into output
-- | field `w`: the `Reel` optic's row form over the strength `Retaining`.
-- | An occurrence of a focused case steps the focus; any other case is
-- | retained and released as field `w` beside the focus's last row.
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
    -- no `Lacks`: `unsafeSet` realizes the layout `Cons w [ | b ] r1 r` pins —
    -- first-label convention, as `inj`/`on`.
    (\(Tuple b' bg) -> unsafeSet (reflectSymbol (Proxy @w)) bg b')
    g

-- | Resume state field `l` of every emission as case `w`, seeded with its
-- | initial value: the productive unfold at one field, the `Coreel`
-- | optic's row form over the co-strength `Coretaining`. The state is one
-- | field labelled on the view line (`unfolding @"resume" @"next"
-- | firstTicket`); case `w` carries `{ l :: a }`. A seed that is a hole is
-- | not injected (guardrails L18).
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
