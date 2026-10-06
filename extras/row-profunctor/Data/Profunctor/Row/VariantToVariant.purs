-- | `Variant → Variant` (+ → +) row profunctors, organized (uniformly across
-- | the four shape modules) as:
-- |
-- |   * **strength** — `Choice` (ecosystem, with a `(->)` instance): the
-- |     unary power, the case prism. Its co-strength `Cochoice` is the
-- |     ecosystem's too, and their optics are `Data.Lens.Prism` (existential
-- |     constructor in `Data.Lens.Prism.Existential`) and
-- |     `Data.Lens.Coprism` — neither the classes nor the optics mention a
-- |     row, so none of them lives here, and the focus/background dispatch
-- |     `splitVariant` sits on the floor, in `Data.Profunctor.Row`.
-- |   * **shape class** — `VariantToVariant`, the binary **merge**: the
-- |     one genuine per-carrier primitive, with its qualified-do sugar
-- |     (`bind`/`discard`).
-- |   * **free functions** — over the strength: `subChoice` (a sub-variant,
-- |     the background cases passing) and `focusCase` (one case, the
-- |     value-level prism); over bare `Profunctor`, the structural
-- |     adopters `atCase` (the closed-singleton unwrap of an input case)
-- |     and `toCase` (a bare output introduced as a case), plus
-- |     `forCase @l` (business case `l` rendered into a single-case
-- |     status's own case). The co-strength `Cochoice` has no row form:
-- |     its retraction holds raw here, but a loop through the four shapes
-- |     is cut at its record junction (`looped`), an event's re-entry
-- |     being its fold.
-- |
-- | A word lives in the module of the sides it constrains: one polymorphic
-- | on one side sits in the diagonal module of the side it constrains, so
-- | the mixed modules hold only their strength, their trace and the words
-- | that genuinely span both sides.
-- |
-- | Business functions are arguments of leaves, never adopters: a display
-- | takes its read function, a status its business case and copy function
-- | (`snackbar @"booked" bookedLine`), and an emitter emits its own case,
-- | which the fold consumes. So the adopters left are structural — `atCase`
-- | and `toCase` take their case as a type argument, since there the case
-- | is the caller's to name — and `forCase @l` is **vocabulary plumbing**,
-- | the variant-input twin of `RecordToRecord.focusField`: every status is
-- | its canonical `[ event :: String ]` face under `forCase @l f`, so it is
-- | exported for the vocabularies and not re-exported by `PUI`.
-- |
-- | ## Laws at `+→+`
-- |
-- | The six laws of Data.Profunctor.Row ("The laws") read at this shape:
-- | a component `w :: p [ | i ] [ | o ]` is a **handler** of occurrences;
-- | the merge is `m = variantToVariant w1 w2`, inputs owned
-- | (`OwnedVariantInputs`: exactly one handler per case), outputs shared
-- | (`SharedVariantOutputs`).
-- |
-- |   1–2. **Repetition, Answer** — not owed. A variant input is
-- |      dispatched to one owner and `≈` counts every occurrence: a
-- |      handler may forward once (`identity`), respond later (`action`)
-- |      or never answer, and nothing coalesces two occurrences — no
-- |      `Eq` is ever needed. Emitting inside `toUser`
-- |      here is a response to an occurrence, not the echo law 2 forbids
-- |      at `×→+`.
-- |   3. **Monoid** — unit `identity :: p (Variant ()) (Variant ())`,
-- |      exact: `variantToVariant identity g = g = variantToVariant g identity`;
-- |      symmetric and associative up to `≈`. Both ends uninhabited, the
-- |      wire there is silence, forced. The merge pinned at its unit is
-- |      the operand, so this shape names no introducer (`atCase` is bare
-- |      `Profunctor`).
-- |   4. **Projection** — `π_k` is the operand's own cases: an occurrence
-- |      of case `l` reaches the one handler owning `l` and no other
-- |      (`DisjointLabels` makes a duplicated case a compile error naming
-- |      it), and nothing else reaches a handler. `exact` is the identity:
-- |      a variant carries its one tag, so two handlers may emit the same
-- |      case, forwarded unmarked.
-- |   5. **Preservation** — vacuous: nothing is owed at a variant input.
-- |   6. **Monotonicity** — `w1 ⊑ w1'` implies
-- |      `variantToVariant w1 w2 ⊑ variantToVariant w1' w2`.
-- |
-- | On `PUI`: dispatch in, passage out — no broadcast, no gate, no state
-- | (`Applicative m` suffices; the one merge with neither feed
-- | obligation). `Cochoice`'s retraction holds raw here — `unleft (left
-- | g) = g` — the one shape where it does (doc §5), which is why no knot
-- | is needed at this shape and none is offered. Probes carry law and shape in test/Main.purs; 3, 4 and
-- | 6 run over every script to a bound in test/Exhaustive.purs.
-- |
-- | One transpose of a `RecordToRecord` name is **deliberately absent**
-- | here: `focusField`'s `+ → +` transpose — the closed-singleton case wrap
-- | `p f f' -> p [ l :: f ] [ l' :: f' ]` — fails the admission test's
-- | subsumption step: it is already vocabulary-expressible as
-- | `w # atCase @l # toCase @l' f`, two adopters applications already have.
-- | It was admitted for one day (2026-10-02, as `handler @l f`, the wire at
-- | the wrap) when counter's four-shape loop wanted a `+→+` stage that was
-- | a pure function of the occurrence, and deleted 2026-10-03 when that
-- | function moved onto the fold (`VariantToRecord.fold @l f`): a pure
-- | handling of an occurrence is applied where the occurrence is consumed,
-- | so the `+→+` shape keeps no pure leaf, and its handlers remain the
-- | `action`s. `RecordToRecord.subStrong`'s transpose, `subChoice`, once sat
-- | in this note as failing reachability; an application reached for it —
-- | some cases detouring through an interception stage while the rest pass
-- | straight through — and it is admitted below.
module Data.Profunctor.Row.VariantToVariant
  ( class VariantToVariant
  , variantToVariant
  , bind
  , discard
  , subChoice
  , focusCase
  , atCase
  , toCase
  , forCase
  )
  where
import Control.Category (identity)
import Data.Either (Either(..), either)
import Data.Lens.Prism.Existential (prismE)
import Data.Profunctor (class Profunctor, dimap, lcmap, rmap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Profunctor.Row (class ExclusiveRows, class OwnedVariantInputs, class SharedVariantOutputs, splitVariant)
import Data.Symbol (class IsSymbol)
import Data.Unit (Unit, unit)
import Data.Variant (class Contractable, case_, expand, inj, on)
import Prim.Row (class Cons, class Union)
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))

class Profunctor p <= VariantToVariant p where
  variantToVariant
    :: forall i1 i1l i2 i2l o1 o2 o12 o1x o2x i o
     . OwnedVariantInputs i1 i2 i i1l i2l
    => SharedVariantOutputs o1 o2 o o12 o1x o2x
    => p [ | i1 ] [ | o1 ]
    -> p [ | i2 ] [ | o2 ]
    -> p [ | i ] [ | o ]

instance VariantToVariant (->) where
  variantToVariant p1 p2 v = case splitVariant v of
    Left v1 -> expand (p1 v1)
    Right v2 -> expand (p2 v2)

bind
  :: forall p v1 rl1 v2 rl2 v4 v5 v6 v7 v8 v3 v
   . VariantToVariant p
  => OwnedVariantInputs v1 v2 v3 rl1 rl2
  => SharedVariantOutputs v4 v5 v v6 v7 v8
  => p [ | v1 ] [ | v4 ]
  -> (p [ | v1 ] [ | v4 ] -> p [ | v2 ] [ | v5 ])
  -> p [ | v3 ] [ | v ]
bind first cont = variantToVariant first (cont first)

discard
  :: forall p v1 rl1 v2 rl2 v4 v5 v6 v7 v8 v3 v
   . VariantToVariant p
  => OwnedVariantInputs v1 v2 v3 rl1 rl2
  => SharedVariantOutputs v4 v5 v v6 v7 v8
  => p [ | v1 ] [ | v4 ]
  -> (Unit -> p [ | v2 ] [ | v5 ])
  -> p [ | v3 ] [ | v ]
discard first cont = bind first (\_ -> cont unit)

-- | Focus a sub-variant, passing the background cases through untouched.
-- | Routing needs only the focus's labels, which the view names; the
-- | background is whatever else arrives (guardrails L18: the whole row is
-- | the logic's, so it is left free).
subChoice
  :: forall p v1 v2 b v v3
   . Choice p
  => ExclusiveRows v1 b v
  => ExclusiveRows v2 b v3
  => Contractable v v1
  => Contractable v b
  => p [ | v1 ] [ | v2 ]
  -> p [ | v ] [ | v3 ]
subChoice g = dimap splitVariant (either expand expand) (left g)

-- | The case prism: transform the payload of case `l`, passing the other cases through.
focusCase
  :: forall @l p f f' b v v1 v2
   . IsSymbol l
  => Cons l f b v
  => Cons l f' b v1
  => Union b v2 v1
  => Choice p
  => p f f'
  -> p [ | v ] [ | v1 ]
focusCase =
  prismE
    (on (Proxy @l) Left Right)
    (either (inj (Proxy @l)) expand)

-- | Adopt a bare-input component as the owner of input case `l`.
atCase :: forall @l p a b v. IsSymbol l => Cons l a () v => Profunctor p => p a b -> p [ | v ] b
atCase = lcmap (on (Proxy @l) identity case_)

-- | Emit a component's bare output, mapped by the projection, as case `l`.
toCase
  :: forall @l @b p c a v
   . IsSymbol l
  => Cons l b () v
  => Profunctor p
  => (a -> b)
  -> p c a
  -> p c [ | v ]
toCase f = rmap (\a -> inj (Proxy @l) (f a))

-- | Render business case `l` into a single-case status's own case with `f`.
forCase
  :: forall @l c p a b d v v1
   . RowToList v1 (RL.Cons c a RL.Nil)
  => IsSymbol c
  => IsSymbol l
  => Cons c a () v1
  => Cons l b () v
  => Profunctor p
  => (b -> a)
  -> p [ | v1 ] d
  -> p [ | v ] d
forCase f = lcmap (on (Proxy @l) (\b -> inj (Proxy @c) (f b)) case_)
