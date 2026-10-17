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
-- |     value-level prism, the background passing — the lift every
-- |     vocabulary dialog is built with, as every editor is `focusField
-- |     @l`-lifted) and `onCase` (one case handed to a handler that
-- |     answers nothing on it, the background passing: the row less that
-- |     case); the one-sided halves `atCase`/`toCase` live
-- |     on the floor. No co-strength row form: an app's loop is cut at
-- |     its record junction, where the model row holds the state
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
-- | (`snackbar @"Flight booked" identity`), and an emitter emits its own case,
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
-- | so the `+→+` shape keeps no pure leaf. Its leaves are the ones that
-- | **wait**: the `action`s, and the dialogs, which wait for the user —
-- | `confirmed @l` (`focusCase @l` over a modal whose content is a stage
-- | over the payload: confirm re-emits the case with the content's row,
-- | cancel consumes it) and `acknowledged @l` (`onCase @l` over a modal
-- | that dismisses: the case consumed, the row less it leaving). These are
-- | the two words that pass background cases through a `+→+` stage, each
-- | over its strength form and never over the merge, so a dialog among
-- | events names only its own case and the fold block stays typed at
-- | exactly the model-changing cases. Until 2026-10-17 `confirmed` was a
-- | `+→×` leaf folding on confirm, and an informational dialog folded
-- | `identity` (CHANGELOG).
module Data.Profunctor.Row.VariantToVariant
  ( class VariantToVariant
  , variantToVariant
  , bind
  , discard
  , focusCase
  , onCase
  )
  where
import Data.Either (Either(..), either)
import Control.Category (identity)
import Control.Semigroupoid ((<<<))
import Data.Lens.Prism.Existential (prismE)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Profunctor.Row (class OwnedVariantInputs, splitVariant)
import Data.Symbol (class IsSymbol)
import Data.Unit (Unit, unit)
import Data.Variant (Variant, inj, on)
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= VariantToVariant p where
  variantToVariant
    :: forall i1 i2 i o i1l i2l
     . OwnedVariantInputs i1 i2 i i1l i2l
    => p [ | i1 ] [ | o ]
    -> p [ | i2 ] [ | o ]
    -> p [ | i ] [ | o ]

instance VariantToVariant (->) where
  variantToVariant p1 p2 v = case splitVariant v of
    Left v1 -> p1 v1
    Right v2 -> p2 v2

bind
  :: forall p i1 i2 i o i1l i2l
   . VariantToVariant p
  => OwnedVariantInputs i1 i2 i i1l i2l
  => p [ | i1 ] [ | o ]
  -> (p [ | i1 ] [ | o ] -> p [ | i2 ] [ | o ])
  -> p [ | i ] [ | o ]
bind first cont = variantToVariant first (cont first)

discard
  :: forall p i1 i2 i o i1l i2l
   . VariantToVariant p
  => OwnedVariantInputs i1 i2 i i1l i2l
  => p [ | i1 ] [ | o ]
  -> (Unit -> p [ | i2 ] [ | o ])
  -> p [ | i ] [ | o ]
discard first cont = bind first (\_ -> cont unit)

-- | The case prism: the payload of case `l` handed to the leaf, whose
-- | emissions leave as case `l` again, every other case passing through
-- | untouched — `focusField`'s `+→+` transpose, constrained like it by
-- | the two `Cons` alone (the background `b` is what both rows share).
-- | At `f = f'` it is the dialog lift: a modal whose content is a stage
-- | over the payload is `focusCase @l`-lifted into the event row
-- | (`confirmed @l`, MDC2/MDC3), as an editor is `focusField @l`-lifted
-- | into the model row. Laws: `focusCase @l identity ≈ identity`; a leaf
-- | that answers no feed (a cancelled dialog) is a `⊑`-refinement of it.
focusCase
  :: forall @l p f f' b v v1
   . IsSymbol l
  => Cons l f b v
  => Cons l f' b v1
  => Choice p
  => p f f'
  -> p [ | v ] [ | v1 ]
focusCase =
  prismE
    (on (Proxy @l) Left Right)
    (either (inj (Proxy @l)) background)
  where
  -- a background value is one of `v1`'s cases other than `l`, which
  -- `Cons l f' b v1` states; `expand` would say the same under a
  -- `Union b _ v1` the two `Cons` already imply
  background :: Variant b -> Variant v1
  background = unsafeCoerce

-- | Case `l` handed to a handler, every other case passing through: the
-- | row **less `l`** leaves, so what the handler does not re-emit is
-- | consumed — the `+→+` stage that eliminates a case. The handler's
-- | output is the background row, where a status face, releasing nothing,
-- | is typed at any row (`acknowledged @l`, MDC2/MDC3: a modal that
-- | dismisses, the event dying with its copy read). `focusCase @l` keeps
-- | the case in the row; `onCase @l` is for the stage that may not re-emit
-- | it, so the fold block after it is typed at exactly what folds.
-- | Law: `onCase @l identity ≈ identity` at `b = v`'s background, and the
-- | silent handler's `onCase` is `⊑` the wire.
onCase
  :: forall @l p a b v
   . IsSymbol l
  => Cons l a b v
  => Choice p
  => p a [ | b ]
  -> p [ | v ] [ | b ]
onCase = dimap (on (Proxy @l) Left Right) (either identity identity) <<< left
