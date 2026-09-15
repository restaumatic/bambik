-- | `Variant → Variant` row profunctors: the direction class
-- | `VariantToVariant` — the binary **merge**, the one genuine per-carrier
-- | primitive — with its qualified-do sugar. One-at-a-time events dispatch to
-- | the operand handling their case (`ExclusiveRows` on input: exactly one
-- | handler per case); outputs may overlap (`InclusiveRows`). Over ecosystem
-- | `Choice`: `focusCase` (the value-level case prism, via `prismE`) and
-- | `subChoice` (sub-variant focus); over bare `Profunctor`: `atCase`
-- | (the closed-singleton unwrap, `RecordToRecord.atField`'s transpose);
-- | over `Cochoice`: `iterate` (the `Coprism` optic's row form). The
-- | `Coprism` optic itself is in `Data.Lens.Coprism`, and the
-- | focus/background dispatch `splitVariant` on the floor in
-- | `Data.Profunctor.Row` — neither mentions a row profunctor.
-- |
-- | ## Laws of the `+→+` shape
-- |
-- | These are the three laws of Data.Profunctor.Row ("The laws, stated
-- | once") read at `+→+`, on the **nine axes** every shape's law set
-- | shares — each line's title is the axis, its subtitle this shape's
-- | reading (the floor's grid has all four; the two laws that read the
-- | same at every shape, symmetry-and-associativity and monotonicity, are
-- | stated once on the floor) — kept spelled out here because each line
-- | is what a test or a starvation message names.
-- |
-- | For a citizen `w :: p [ | i ] [ | o ]` — a **handler** of occurrences
-- | — with `occur e` an input occurrence (a feed at `+`), `emit e'` an
-- | emission, `≈` observational equivalence and `⊑` refinement
-- | (doc/observational-semantics.md; §3.1 is this shape's modality,
-- | **may** respond). The first three are protocol obligations; the rest
-- | are carrier guarantees given them.
-- |
-- | **Citizen laws** — repetition, emission, answer:
-- |
-- |   1. **Repetition — twice is two.** `occur e ; occur e` is two
-- |      occurrences, not one, and nothing in the shape may coalesce them:
-- |      an event has no value between occurrences, so there is nothing to
-- |      compare and no `Eq` is ever needed. (The contrast with `×→×`'s
-- |      line 1 is the shape distinction itself.)
-- |   2. **Emission — a response to an occurrence.** Every `emit e'`
-- |      descends from an input occurrence. Nothing at registration,
-- |      nothing spontaneous: a handler never *originates*, so a response
-- |      emitted inside `toUser` is not the echo doc law 3 forbids (that
-- |      law is about sources).
-- |   3. **Answer — any number.** An occurrence is answered by any number
-- |      of emissions — during it or after it, once, several times or
-- |      never (forward, transform, split, end). `identity` forwards each
-- |      occurrence exactly once; `action` responds when its `Aff`
-- |      settles. This is `iterate`'s well-foundedness — re-entry is an
-- |      event loop, not a busy loop.
-- |
-- | **Merge laws** — for `m = variantToVariant w1 w2`, inputs owned
-- | (`OwnedVariantInputs`: exactly one handler per case), outputs shared
-- | (`SharedVariantOutputs`):
-- |
-- |   4. **Unit — the wire at `Variant ()`.** Conditional on the carrier —
-- |      *if* `p` is a `Category`, `identity :: p (Variant ()) (Variant ())`
-- |      is the unit exactly:
-- |
-- |      ```
-- |      variantToVariant identity g = g = variantToVariant g identity
-- |      ```
-- |
-- |      Both empty-variant ends are uninhabited, so the wire there can
-- |      neither receive nor emit: it is silence, forced, and any silent
-- |      element of that type is equal to it. The merge has no unit of its
-- |      own, and the unary form — the merge pinned at its unit — is the
-- |      operand itself, so this shape names no introducer (its
-- |      closed-singleton adopter `atCase` is bare `Profunctor`).
-- |   5. **Input side — dispatch.** An occurrence of case `l` is delivered
-- |      to the one handler owning `l` and to no other (`DisjointLabels`
-- |      makes a duplicated case a compile error naming it). Exactly one
-- |      operand answers each occurrence — the exclusive-input side's whole
-- |      content.
-- |   6. **Output side — passage.** Each emission exits **as it occurs**,
-- |      ungated: a variant output has no value between occurrences, so
-- |      there is nothing to retain, nothing to gate and nothing to tear.
-- |   7. **Exactness — free.** An operand counts only at its declared
-- |      cases, and nothing need enforce it: a variant carries its one tag,
-- |      so `widenVariantOutput` is `rmap expand` and there is no trim
-- |      (`SharedVariantOutputs` carries no evidence). Two handlers may
-- |      declare the same case; the merge forwards each, unmarked.
-- |   8. **Closure — stateless.** If `w1`, `w2` satisfy 1–3, so does `m`:
-- |      dispatched on input, each emission exiting as it occurs. No
-- |      broadcast and no gate, so the merge keeps no state — the one merge
-- |      carrying neither feed obligation, needing only `Applicative m` on
-- |      `PUI`.
-- |   9. **Independence — the loop is `iterate`, raw.** An emission goes
-- |      downstream, never to the sibling handler. The loop at this shape is
-- |      `Cochoice`'s `iterate` (`again` cases re-enter, `done` cases exit),
-- |      and its retraction holds **raw** — `unleft (left g) = g` — the one
-- |      direction where it does, which is the trace asymmetry theorem
-- |      (doc §5).
-- |
-- | Every cell has a probe in test/Main.purs: 1–3 (`repetition +→+`,
-- | `emission +→+`, `answer +→+` on the forward wire and its merge; 3's
-- | other readings are `unleft: looped branch withheld`/`re-enters` and
-- | `iterate`), 4 (`unit law +→+`), 5 (`+→+ dispatch: case routed to its
-- | one handler`), 6 (`+→+ dispatch: outputs may overlap, both exit`),
-- | 7 (the same probe — `ok` declared by both handlers, both exits
-- | unmarked), 8 (`+→+ dispatch`), 9 (`independence +→+`; the raw
-- | retraction `unleft (left g) = g`); the floor's two shared laws at this
-- | shape are `+→+ symmetry`/`+→+ associativity` and `enrichment at +→+`.
-- | Beyond the probes, laws 4–5 and the shared laws are checked over
-- | **every script** to length 6 (two operands) or 8 (three) in
-- | test/Exhaustive.purs (doc/observational-semantics.md §9). The
-- | background transparency of `subChoice` — `identity` on every case
-- | outside its focus — is a law of the strength, not of the merge: stated
-- | at `subChoice` below, as at its three siblings, and pinned by the
-- | cashbox demo.
-- |
-- | One transpose of a `RecordToRecord` name is **deliberately absent**
-- | here: `field`'s
-- | `+ → +` transpose — the closed-singleton case wrap
-- | `p f f' -> p [ l :: f ] [ l' :: f' ]` — fails the admission test's
-- | subsumption step: it is already vocabulary-expressible as
-- | `w # atCase @l # toCase @l' f`, two adopters applications already have.
-- | `RecordToRecord.subStrong`'s transpose, `subChoice`, once sat in this
-- | note as failing reachability; an application reached for it — some cases
-- | detouring through an interception stage while the rest pass straight
-- | through — and it is admitted below.
module Data.Profunctor.Row.VariantToVariant
  ( bind
  , variantToVariant
  , focusCase
  , class VariantToVariant
  , discard
  , subChoice
  , iterate
  , atCase
  )
  where

import Control.Category (identity)
import Data.Either (Either(..), either)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Profunctor.Cochoice (class Cochoice, unleft)
import Data.Symbol (class IsSymbol)
import Data.Unit (Unit, unit)
import Data.Variant (class Contractable, case_, expand, inj, on)
import Prim.Row (class Cons, class Union)
import Type.Proxy (Proxy(..))
import Data.Lens.Prism.Existential (prismE)
import Data.Profunctor.Row (class ExclusiveRows, class OwnedVariantInputs, class SharedVariantOutputs, splitVariant)

class Profunctor p <= VariantToVariant p where
  variantToVariant :: forall i1 i1l i2 i2l o1 o2 o12 o1x o2x i o.
    OwnedVariantInputs i1 i2 i i1l i2l =>
    SharedVariantOutputs o1 o2 o o12 o1x o2x =>
    p [ | i1 ] [ | o1 ] -> p [ | i2 ] [ | o2 ] -> p [ | i ] [ | o ]

-- | The timeless carrier: dispatch to the one handler owning the case,
-- | expand its answer — the (+,+)-monoid on plain functions, which makes
-- | the merge's unit and associativity laws pure equalities (test/Main.purs).
instance VariantToVariant (->) where
  variantToVariant p1 p2 v = case splitVariant v of
    Left v1 -> expand (p1 v1)
    Right v2 -> expand (p2 v2)

bind :: forall p i1 i1l i2 i2l o1 o2 o12 o1x o2x i o.
  VariantToVariant p =>
  OwnedVariantInputs i1 i2 i i1l i2l =>
  SharedVariantOutputs o1 o2 o o12 o1x o2x =>
  p [ | i1 ] [ | o1 ] -> (p [ | i1 ] [ | o1 ] -> p [ | i2 ] [ | o2 ]) -> p [ | i ] [ | o ]
bind first cont = variantToVariant first (cont first)

discard :: forall p i1 i1l i2 i2l o1 o2 o12 o1x o2x i o.
  VariantToVariant p =>
  OwnedVariantInputs i1 i2 i i1l i2l =>
  SharedVariantOutputs o1 o2 o o12 o1x o2x =>
  p [ | i1 ] [ | o1 ] -> (Unit -> p [ | i2 ] [ | o2 ]) -> p [ | i ] [ | o ]
discard first cont = bind first (\_ -> cont unit)

-- | Focus a **sub-variant**: the wrapped profunctor handles the focus cases
-- | `f → f'`, the **background** cases `b` pass through untouched — the shot
-- | `s` is refocused to `s'`. `RecordToRecord.subStrong`'s transpose, completing the wrap
-- | family's `+ → +` corner:
-- |
-- | ```
-- | subChoice :: p [ | f ] [ | f' ] -> p [ | s ] [ | s' ]
-- |               -- where s = f ∪ b,  s' = f' ∪ b   (ExclusiveRows)
-- | ```
-- |
-- | The labeled analogue of `Choice`'s `left`: instead of a positional
-- | complement `c`, the background *row* `b`, split off by `splitVariant`.
-- | Where `RecordToRecord.subStrong` says "this sub-form edits these fields, the rest of
-- | the model rides along", `subChoice` says "these cases are
-- | intercepted, the rest pass" — the focus cases detour through whatever
-- | the wrapped profunctor does with them, the rest flow straight on.
-- |
-- | Law (**background transparency**): `subChoice w` acts as `identity` on
-- | every case outside `w`'s focus row — a background occurrence passes
-- | untouched, exactly once. This is `Choice`'s `left` law read at the
-- | row, a law of the strength rather than of the merge (which is why it
-- | is not among the header's nine); the cashbox demo is its contract.
subChoice
  :: forall p f f' b s s'
   . Choice p
  => ExclusiveRows f b s
  => ExclusiveRows f' b s'
  => Contractable s f
  => Contractable s b
  => p [ | f ] [ | f' ]
  -> p [ | s ] [ | s' ]
subChoice g = dimap splitVariant (either expand expand) (left g)

-- | Adopt a bare-input UI component as the owner of input case `l` — `lcmap`-only,
-- | the **closed-singleton unwrap** at `+`, and so `RecordToRecord.atField`'s
-- | exact transpose (`Cons l a () s` on both): `action createPerson #
-- | atCase @"create"` inside a `VariantToVariant.do` merge, and the input-side
-- | transpose of `RecordToRecord.asField` at `+`.
-- | No subsumption here, deliberately: a case *payload* is pinned by the
-- | action that consumes it as often as by the UI component that emits it, so
-- | widening this position would leave both unknown (the payload-boundary
-- | rule).
atCase :: forall @l p a b s. IsSymbol l => Cons l a () s => Profunctor p => p a b -> p [ | s ] b
atCase = lcmap (on (Proxy @l) identity case_)

-- | Focus an existing case in place — the standard `Choice` case prism, read
-- | photographically as **refocusing**: the **focus** `f → f'` changes, the
-- | **background** `b` stays, so the **shot** `s` becomes `s'` (`Union b mix s'`
-- | lets the untouched background `expand` into the new row). `f' := f`
-- | recovers the simple `p f f -> p [ | s ] [ | s ]` form. Built via `prismE`
-- | at `c := [ | b ]`. (The *diagonal* re-backgrounder — pass case `l`
-- | untouched, handle everything else — needs no combinator of its own: it is
-- | `subChoice` at the singleton complement `[ l :: f ]`.)
focusCase
  :: forall @l p f f' b s s' mix
   . IsSymbol l
  => Cons l f b s
  => Cons l f' b s'
  => Union b mix s'
  => Choice p
  => p f f' -> p [ | s ] [ | s' ]
focusCase =
  prismE
    (on (Proxy @l) Left Right)
    (either (inj (Proxy @l)) expand)

-- | The `+`-diagonal **trace** at row granularity, over ecosystem `Cochoice`:
-- | loop the `again` cases of the output back into the input, emit only the
-- | `done` cases — **iteration** (retry/wizard flows). `splitVariant` is the
-- | done/again dispatch. Unit law: at `again = ()` (no loop-back cases) the
-- | UI component is unchanged. On `PUI` the re-entry is a `toUser`, so the loop
-- | advances on the UI component's next emission — an event loop, not a busy loop.
iterate
  :: forall p done again out
   . Cochoice p
  => ExclusiveRows done again out
  => Contractable out done
  => Contractable out again
  => p [ | again ] [ | out ]
  -> p [ | again ] [ | done ]
iterate g = unleft (dimap (either identity identity) splitVariant g)
