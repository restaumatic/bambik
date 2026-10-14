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
-- |   * **free functions** — over the strength: `focusCase` (one case, the
-- |     value-level prism; the one-sided halves `atCase`/`toCase` live
-- |     on the floor). No co-strength row form: an app's loop is cut at
-- |     its record junction, where the model can be declared
-- |     (`Data.Profunctor.Row`, *What is deliberately not here*).
-- |
-- | A word lives where the sides it constrains do: one constraining both
-- | sides in its shape's module, one constraining a single side on the
-- | floor (`Data.Profunctor.Row`)
-- | — so no shape module imports another, and every module holds only
-- | its merge, its strength and trace forms, and the words that genuinely
-- | span both of its sides.
-- |
-- | Business functions are arguments of leaves, never adopters: a display
-- | takes its read function, a status its business case and fold
-- | (`snackbar @"Flight booked" kept`), and an emitter emits its own case,
-- | which the fold consumes. So the adopters are structural and take no
-- | function: `atCase` and `toCase` take only their cases, since there
-- | the case is the caller's to name. A status is its face over its
-- | label, adopted as its case: `snackbar f = snackbarFace (reflectSymbol
-- | (Proxy @l)) # atCase @l # folding @l f`.
-- |
-- | The three single-label words mirror `RecordToRecord`'s one for one —
-- | `focusCase`/`focusField` (the background threaded), `atCase`/`atField`
-- | and `toCase`/`toField` (the input and output halves of the iso between
-- | a closed singleton and its payload). Laws: `atCase @l >>> toCase @l`
-- | is `identity` at the closed singleton, and there the focus is its two
-- | halves, `focusCase @l = toCase @l <<< atCase @l` (test/Main.purs). The
-- | derivation runs that way only: a two-sided wrap cannot be split into
-- | one side's, so the two halves are generators, not `focusCase`
-- | specialised. There is no rename (a closed singleton relabelled on
-- | both sides): its one use, re-labelling a component built at a fixed
-- | label, is what guardrails L3 forbids (`Data.Profunctor.Row`, the
-- | single-label grid).
-- |
-- | ## Laws at `+→+`
-- |
-- | The six laws of Data.Profunctor.Row ("The laws") read at this shape:
-- | a component `w :: p [ | i ] [ | o ]` is a **handler** of occurrences;
-- | the merge is `m = variantToVariant w1 w2`, inputs owned
-- | (`OwnedVariantInputs`: exactly one handler per case), outputs **one
-- | row** every operand is typed at — an equality, as the `×→×` merge's
-- | inputs are (an inclusive union could not be split back out into an
-- | operand's row under holes, so two actions sharing an outcome had to
-- | name it on each line; CHANGELOG, 2026-10-07).
-- |
-- |   1–2. **Repetition, Answer** — not owed. A variant input is
-- |      dispatched to one owner and `≈` counts every occurrence: a
-- |      handler may forward once (`identity`), respond later (`action`)
-- |      or never answer, and nothing coalesces two occurrences — no
-- |      `Eq` is ever needed. Emitting inside `toUser`
-- |      here is a response to an occurrence, not the echo law 2 forbids
-- |      at `×→+`.
-- |   3. **Monoid** — unit `lcmap case_ identity`, the wire entered from
-- |      the empty variant at any output row,
-- |      exact: `variantToVariant unit g = g = variantToVariant g unit`;
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
-- | g) = g` — the one shape where it does (doc §5), which is why a knot
-- | at this shape would need no class of its own. Probes carry law and shape in test/Main.purs; 3, 4 and
-- | 6 run over every script to a bound in test/Exhaustive.purs.
-- |
-- | One transpose of a `RecordToRecord` name is **deliberately absent**
-- | here: `focusField`'s `+ → +` transpose — the closed-singleton case wrap
-- | `p f f' -> p [ l :: f ] [ l' :: f' ]` — fails the admission test's
-- | subsumption step: it is already vocabulary-expressible as
-- | `lcmap`/`rmap` around `w # atCase @l # toCase @l'`.
-- | It was admitted briefly (as `handler @l f`, the wire at the wrap)
-- | when counter's four-shape loop wanted a `+→+` stage that was a pure
-- | function of the occurrence, and deleted when that function moved onto
-- | the fold (CHANGELOG, 2026-10-03): a pure
-- | handling of an occurrence is applied where the occurrence is consumed,
-- | so the `+→+` shape keeps no pure leaf, and its handlers remain the
-- | `action`s. Nor does any word pass background cases through a `+→+`
-- | stage: an action is folded by its own outcome statuses, and that pair
-- | owns the button's case in the fold block, so every event reaches the
-- | fold block on its own (CHANGELOG, 2026-10-13).
module Data.Profunctor.Row.VariantToVariant
  ( class VariantToVariant
  , variantToVariant
  , bind
  , discard
  , focusCase
  )
  where
import Data.Either (Either(..), either)
import Data.Lens.Prism.Existential (prismE)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Row (class OwnedVariantInputs, splitVariant)
import Data.Symbol (class IsSymbol)
import Data.Unit (Unit, unit)
import Data.Variant (expand, inj, on)
import Prim.Row (class Cons, class Union)
import Type.Proxy (Proxy(..))

class Profunctor p <= VariantToVariant p where
  variantToVariant
    :: forall v1 rl1 v2 rl2 v o
     . OwnedVariantInputs v1 v2 v rl1 rl2
    => p [ | v1 ] [ | o ]
    -> p [ | v2 ] [ | o ]
    -> p [ | v ] [ | o ]

instance VariantToVariant (->) where
  variantToVariant p1 p2 v = case splitVariant v of
    Left v1 -> p1 v1
    Right v2 -> p2 v2

bind
  :: forall p v1 rl1 v2 rl2 v3 o
   . VariantToVariant p
  => OwnedVariantInputs v1 v2 v3 rl1 rl2
  => p [ | v1 ] [ | o ]
  -> (p [ | v1 ] [ | o ] -> p [ | v2 ] [ | o ])
  -> p [ | v3 ] [ | o ]
bind first cont = variantToVariant first (cont first)

discard
  :: forall p v1 rl1 v2 rl2 v3 o
   . VariantToVariant p
  => OwnedVariantInputs v1 v2 v3 rl1 rl2
  => p [ | v1 ] [ | o ]
  -> (Unit -> p [ | v2 ] [ | o ])
  -> p [ | v3 ] [ | o ]
discard first cont = bind first (\_ -> cont unit)

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
