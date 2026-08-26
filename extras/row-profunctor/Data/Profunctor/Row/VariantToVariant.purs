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
-- | The **nullary** operator is the merge **unit** — the class's own
-- | `pempty :: p (Variant ()) (Variant ())`: `variantToVariant pempty g = g`.
-- | Here silence is not merely lawful but forced — both empty-variant ends
-- | are uninhabited, so the unit can neither receive nor emit — and any
-- | silent element implements it (`PUI` writes a direct silent body: the
-- | variant *input* is outside `silence`'s `{ | i }` shape).
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
  , bracketed
  , pempty
  )
  where

import Control.Category (identity)
import Data.Either (Either(..), either)
import Data.Profunctor.Looping (class Looping, looped)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Profunctor.Cochoice (class Cochoice, unleft)
import Data.Symbol (class IsSymbol)
import Data.Unit (Unit, unit)
import Data.Variant (class Contractable, Variant, case_, expand, inj, on)
import Prim.Row (class Cons, class Union)
import Type.Proxy (Proxy(..))
import Data.Lens.Prism.Existential (prismE)
import Data.Profunctor.Row (class ExclusiveRows, class OwnedVariantInputs, class SharedVariantOutputs, splitVariant)

-- | The **variant-editor bracket**: adopt a record-shaped editor ensemble
-- | (every case's payload retained) as an editor of one-at-a-time variant
-- | state — `stateOf` brackets the variant in (seeding absent payloads
-- | from the retained editor state), `caseOf` projects the selection back
-- | out, and the self-trace in between (`Looping`) keeps the ensemble
-- | consistent. An adopter with a `+ → +` *result* — which is why it
-- | lives here, like a label-indexed emitter lives at its `× → +` result. The demos'
-- | variant editors read
-- | `(Category.do …) # bracketed fulfillmentState fulfillmentCase # field @l`.
bracketed :: forall p v s v'. Looping p => ([ | v ] -> { | s }) -> ({ | s } -> [ | v' ]) -> p { | s } { | s } -> p [ | v ] [ | v' ]
bracketed f g w = dimap f g (looped w)

class Profunctor p <= VariantToVariant p where
  variantToVariant :: forall i1 i1l i2 i2l o1 o2 o12 o1x o2x i o.
    OwnedVariantInputs i1 i2 i i1l i2l =>
    SharedVariantOutputs o1 o2 o o12 o1x o2x =>
    p [ | i1 ] [ | o1 ] -> p [ | i2 ] [ | o2 ] -> p [ | i ] [ | o ]
  -- | The **nullary** merge — the unit: handles no cases, emits no cases.
  -- | Both empty-variant ends are uninhabited, so silence is forced — any
  -- | silent element implements it (`PUI` writes a direct silent body: the
  -- | variant *input* is outside `silence`'s `{ | i }` shape).
  pempty :: p (Variant ()) (Variant ())

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
