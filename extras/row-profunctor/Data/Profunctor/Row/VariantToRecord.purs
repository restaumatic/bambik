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
-- |     `fold @l f` (one case folded into the record, `f` of its payload —
-- |     the merge pinned at its unit, under `atCase @l`; a loop's folds, one
-- |     per case, merge here beside the statuses). The co-strength
-- |     `Coretaining` has no row form here: a chain of this shape is closed
-- |     by a knot at one of its two junctions (`looped` or `cycled`), the
-- |     shape change between them an explicit stage.
-- |
-- | A word lives in the module of the sides it constrains: one polymorphic
-- | on one side sits in the diagonal module of the side it constrains, so
-- | the mixed modules hold only their strength, their trace and the words
-- | that genuinely span both sides.
-- | The variant-input words `atCase` and `forCase` are therefore
-- | `VariantToVariant`'s. A label `@w` appears exactly where a row is
-- | wrapped as one field to cross the shape change (`subRetaining`).
-- |
-- | ## Laws at `+→×`
-- |
-- | The six laws of Data.Profunctor.Row ("The laws") read at this shape:
-- | a component `w :: p [ | v ] { | r }` is a **fold** of occurrences into
-- | the record, or a **status**, which releases nothing and so is typed at
-- | every row like `silence`; the merge is `m = variantToRecord w1 w2`,
-- | inputs owned (`OwnedVariantInputs`), the output row **shared** — the
-- | copairing `[f, g] : A + B -> R` of the coproduct (2026-10-04). Each
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
-- | suffices, as at `+→+`). Until 2026-10-04 the outputs were owned and
-- | the merge gated like `×→×`; with a loop's folds one per case, each
-- | releasing the whole next model, the shared row is the honest type and
-- | the copairing the honest mechanism. Releasing on every occurrence is
-- | the fold's choice (no row needs `Eq`), not a law. A merge silent on an
-- | occurrence has an operand that chose not to release — a status. Probes carry law and shape
-- | in test/Main.purs; 3, 4 and 6 run over every script to a bound in
-- | test/Exhaustive.purs.
module Data.Profunctor.Row.VariantToRecord
  ( class VariantToRecord
  , variantToRecord
  , bind
  , discard
  , subRetaining
  , fold
  )
  where

import Control.Category (class Category, identity)
import Control.Semigroupoid ((>>>))
import Data.Function (const)
import Data.Lens.Reel (reelE)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Retaining (class Retaining)
import Data.Profunctor.Row (class ExclusiveRows, class OwnedVariantInputs, splitVariant)
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant, silence)
import Data.Profunctor.Seeding (isHole)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (class Contractable, case_, on)
import Prim.Row (class Cons)
import Record.Unsafe (unsafeSet)
import Type.Proxy (Proxy(..))

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

-- | One case folded into the record: the closed singleton `[ l :: a ]`
-- | consumed by `f`, which turns its payload into the row — `atCase @l` of
-- | the function, the `+→×` merge pinned at its unit. Label-indexed like
-- | every leaf (`button @l`, `snackbar @l f`): a loop's folds are one
-- | `fold @l f` per event case, merged by `VariantToRecord.do` beside the
-- | statuses, each releasing the whole next model (the merge's output row
-- | is shared, the copairing). Memoryless: a replaying emitter's
-- | payload is the row it was fed, an event with something of its own
-- | arrives `RecordToVariant.joined @l` with that row as `{ event, model }`,
-- | and an effect returns the model (`fold @"created" identity`), so every
-- | handler has the model in hand and the loop's memory stays at the
-- | emitters and in `mvu`. The output row may be declared as a second
-- | visible argument (`fold @"New game" @( board :: … ) newGame`): a loop
-- | cut at a variant junction has no model on its seed line, so the fold
-- | answering the seed event declares the model where it first appears
-- | (guardrails L18, tic-tac-toe). A handler that is a hole is absent
-- | (guardrails L18): the fold then consumes its case and releases nothing
-- | — `silence` at the terminal record, entered from the empty variant —
-- | so a seed event folded by logic not yet written reaches no hole. Laws on `(->)`: `fold @l f (inj @l a) = f a`; at
-- | `identity` it is the closed singleton unwrapped to its row, an iso with
-- | `toCase @l identity` both ways. A seeded, retaining fold (2026-10-03)
-- | and a whole-variant `fold (match …)` (2026-10-04) both gave way to
-- | this form.
fold
  :: forall @l @r p a v
   . IsSymbol l
  => Cons l a () v
  => Profunctor p
  => Category p
  => RecordToVariant p
  => (a -> { | r })
  -> p [ | v ] { | r }
fold f =
  if isHole f then lcmap (const {}) (silence >>> lcmap case_ identity)
  else dimap (on (Proxy @l) identity case_) f identity
