-- | `Record → Record` row profunctors, organized as:
-- |
-- |   * **strength** — `Strong` (ecosystem class, imported): the unary power,
-- |     minimal and interop-friendly.
-- |   * **direction class** — `RecordToRecord`, the binary **merge**: the one
-- |     genuine per-carrier primitive.
-- |   * **free functions** — over the strength: `focusRecord` (sub-record
-- |     focus), `property` (the field lens), `tapped` (the display tap);
-- |     over bare `Profunctor`: `field` (`property`'s closed-singleton
-- |     form — the merge-operand shape, `dimap`-only and runtime-exact
-- |     by construction; the merges themselves enforce exactness via
-- |     `ExactRows`, so `property` operands are safe too);
-- |     over the co-strength `Costrong`: `feedback` (the ×-trace at row
-- |     granularity — a state sub-record loops from output to input).
-- |
-- | The **nullary** operator is the class's own unit `pempty :: p {} {}` —
-- | the empty merge:
-- |
-- | ```
-- | recordToRecord pempty g = g = recordToRecord g pempty
-- | ```
-- |
-- | It is a class member (not a parametric silent element like `UI`'s
-- | `silence`) because a lawful record-output unit must *announce* its
-- | contribution — the informationless `{}` — to the merge machinery, and
-- | anything typed `forall a b. p a b` is silent by parametricity (it can
-- | never fabricate a `b`). For `Category` carriers, `pempty = identity @{}`.
module Data.Profunctor.Row.RecordToRecord
  ( bind
  , recordToRecord
  , class RecordToRecord
  , discard
  , feedback
  , field
  , pempty
  , focusRecord
  , property
  , tapped
  )
  where

import Data.Lens.Record (prop)
import Data.Profunctor (dimap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Costrong (class Costrong, unfirst)
import Data.Profunctor.Strong (class Strong, first, second)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (Unit, unit)
import Prim.Row (class Cons, class Lacks)
import Record (get, insert, union) as Record
import Type.Proxy (Proxy(..))
import Data.Profunctor.Row (class ExactRows, class ExclusiveRows, class InclusiveRows)
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= RecordToRecord p where
  -- | The `ExactRows` constraint is the merge's **runtime-exactness
  -- | guarantee**: gated carriers trim each operand's emission to its
  -- | declared output row before the left-biased union, so an operand whose
  -- | runtime object carries stale copies of sibling fields (an echo wire or
  -- | lens rebuild over the widening-coerced input) cannot shadow the
  -- | siblings' genuine contributions.
  recordToRecord :: forall i1 o1 i2 o2 i12 i1x i2x i o o1l o2l.
    InclusiveRows i1 i2 i i12 i1x i2x =>
    ExclusiveRows o1 o2 o =>
    ExactRows o1 o2 o1l o2l =>
    p { | i1 } { | o1 } -> p { | i2 } { | o2 } -> p { | i } { | o }
  -- | The **nullary** merge — the unit: reads nothing, contributes no fields.
  -- | Genuinely per-carrier: a parametric silent element cannot serve, because
  -- | a record-output unit must *announce* its informationless `{}` so the
  -- | merge machinery knows that side is complete. For `Category` carriers,
  -- | `pempty = identity @{}`.
  pempty :: p {} {}

bind :: forall p i1 o1 i2 o2 i12 i1x i2x i o o1l o2l.
  RecordToRecord p =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  ExclusiveRows o1 o2 o =>
  ExactRows o1 o2 o1l o2l =>
  p { | i1 } { | o1 } -> (p { | i1 } { | o1 } -> p { | i2 } { | o2 }) -> p { | i } { | o }
bind first cont = recordToRecord first (cont first)

discard :: forall p i1 o1 i2 o2 i12 i1x i2x i o o1l o2l.
  RecordToRecord p =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  ExclusiveRows o1 o2 o =>
  ExactRows o1 o2 o1l o2l =>
  p { | i1 } { | o1 } -> (Unit -> p { | i2 } { | o2 }) -> p { | i } { | o }
discard first cont = bind first (\_ -> cont unit)

-- | Row-typed `Strong`: focus a whole **sub-record** — the row-valued **focus**
-- | `f` — transforming it against the **background** `b`, which is carried
-- | unchanged. The **shot** `s` is refocused to `s'`. Operates on rows on
-- | **both sides** — the argument is itself a `Record → Record` profunctor:
-- |
-- | ```
-- | focusRecord :: p { | f } { | f' } -> p { | s } { | s' }
-- |              -- where s = f ∪ b,  s' = f' ∪ b   (ExclusiveRows)
-- | ```
-- |
-- | The labeled analogue of `Strong`'s `first`/`second`: instead of carrying a positional
-- | complement `c`, it carries the background *row* `b`, split off by `ExclusiveRows`.
-- | Plain `Strong` underneath: split `s` into `(f, b)`, run the argument on `f`
-- | via `first`, and re-merge `f'` with `b`.
focusRecord
  :: forall p f f' b s s'
   . Strong p
  => ExclusiveRows f b s
  => ExclusiveRows f' b s'
  => p { | f } { | f' }
  -> p { | s } { | s' }
focusRecord g =
  dimap (\s -> Tuple (unsafeCoerce s) (unsafeCoerce s))
        -- `Record.union` is left-biased and does not nub; safe here only because
        -- `ExclusiveRows f' b s'` guarantees `f'` and `b` are disjoint.
        (\(Tuple f' b) -> Record.union f' b)
        (first g)

-- | Edit an existing field in place — the standard `Strong` field lens, read
-- | photographically as **refocusing**: the **focus** `f → f'` changes, the
-- | **background** `b` stays, so the **shot** `s` becomes `s'` (the shared `b`
-- | witnesses "same rows except at `l`"). `f' := f` recovers the simple
-- | `p f f -> p { | s } { | s }` form. (The *diagonal* re-backgrounder — hold
-- | field `l`, transform everything else — needs no combinator of its own: it
-- | is `focusRecord` at the singleton complement `(l :: f)`.)
property
  :: forall @l p f f' b s s'
   . IsSymbol l
  => Cons l f b s
  => Cons l f' b s'
  => Strong p
  => p f f' -> p { | s } { | s' }
property = prop (Proxy @l)

-- | `property` at the **closed singleton row** — the merge-operand form:
-- | nests a widget (or a whole sub-composite) as exactly one field of the
-- | enclosing record, type-changing like `property` (`f' := f` recovers the
-- | simple `p v v -> p { | r } { | r }` form). The pinned empty background
-- | is what lets merge operands infer with no annotations — raw
-- | `property`'s open background is ambiguous under the merges' `Union`.
-- |
-- | With no background to carry it needs no strength — `dimap` suffices —
-- | and its emissions are **runtime-exact** by construction: exactly the one
-- | field, freshly built. (A lens emission (`property`) instead rebuilds the
-- | record from its retained input, which under the merges' widening
-- | coercions runtime-carries stale copies of *sibling* fields. The gated
-- | merges guard against this — their `ExactRows` evidence trims every
-- | operand emission to its declared output row before the left-biased
-- | `Record.union` — so this is no longer a correctness obligation on
-- | operands; `field` remains the preferred operand form for its
-- | annotation-free inference.)
field :: forall @l p f f' si so. IsSymbol l => Profunctor p => Lacks l () => Cons l f () si => Cons l f' () so => p f f' -> p { | si } { | so }
field = dimap (Record.get (Proxy @l)) (\v -> Record.insert (Proxy @l) v {})

-- | A display **tap** on the `×`-diagonal: shows the value flowing through
-- | and passes it on — the pipeline-stage form of a live view. Pure `Strong`
-- | plus the leaf-echo protocol: `second` retains the value, and the
-- | display's echo triggers the forwarding. Honest only over *displays*
-- | (elements whose sole emission is the echo) — an editing widget inside
-- | would replay the retained upstream value on every edit.
tapped :: forall p s x. Strong p => p s x -> p s s
tapped display = dimap (\s -> Tuple s s) fst (second display)

-- | The `×`-diagonal **trace at row granularity**, over ecosystem
-- | `Costrong`: the **state** sub-record `fb` of the output loops back into
-- | the input, so the wrapped profunctor sees `i ∪ fb` and its `fb`
-- | contribution comes around again — state threading across a pipeline
-- | stage. Like `focusRecord`, the output is split by coercion, so the
-- | emitted `{ | o }` runtime-carries the looped fields — a `feedback`
-- | stage belongs in a pipeline, not as a record-merge operand.
-- |
-- | On a knowledge-gated carrier (`UI`) the state channel must be
-- | **primed by the widget's first emission** — inputs are withheld until
-- | then, so the inner widget must be able to emit unfed (editors that emit
-- | on user input qualify; load-first ensembles use `looped`, the
-- | self-feeding special case, instead).
feedback
  :: forall p i o fb iw ow
   . Costrong p
  => ExclusiveRows i fb iw
  => ExclusiveRows o fb ow
  => p { | iw } { | ow }
  -> p { | i } { | o }
feedback g =
  unfirst
    (dimap
      (\(Tuple i fb) -> Record.union i fb)
      -- coerce-split, as in `focusRecord`: safe because `ExclusiveRows o fb ow`
      -- guarantees the two typed views are disjoint
      (\ow -> Tuple (unsafeCoerce ow) (unsafeCoerce ow))
      g)
