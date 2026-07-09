module Data.Profunctor.Row.VariantToRecord
  ( Reel
  , bind
  , variantToRecord
  , class VariantToRecord
  , discard
  , caseToProperty
  , caseToRecord
  , class Retaining
  , retain
  , retainCase
  , reel
  , reelE
  , reelWrap
  )
  where

import Data.Either (Either(..))
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Row.VariantToVariant (splitVariant)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (Unit, unit)
import Data.Variant (class Contractable, on)
import Prim.Row (class Cons, class Lacks)
import Record (insert)
import Type.Proxy (Proxy(..))
import Type.Row.Constraints (class DispatchableVariants, class ExclusiveRows)

class Profunctor p <= VariantToRecord p where
  variantToRecord :: forall i1 i1l i2 i2l o1 o2 i o.
    ExclusiveRows i1 i2 i =>
    ExclusiveRows o1 o2 o =>
    DispatchableVariants i1 i2 i1l i2l =>
    p [ | i1 ] { | o1 } -> p [ | i2 ] { | o2 } -> p [ | i ] { | o }

bind :: forall f i1 i1l i2 i2l o1 o2 i o.
  VariantToRecord f =>
  ExclusiveRows i1 i2 i =>
  ExclusiveRows o1 o2 o =>
  DispatchableVariants i1 i2 i1l i2l =>
  f [ | i1 ] { | o1 } -> (f [ | i1 ] { | o1 } -> f [ | i2 ] { | o2 }) -> f [ | i ] { | o }
bind first cont = variantToRecord first (cont first)

discard :: forall f i1 i1l i2 i2l o1 o2 i o.
  VariantToRecord f =>
  ExclusiveRows i1 i2 i =>
  ExclusiveRows o1 o2 o =>
  DispatchableVariants i1 i2 i1l i2l =>
  f [ | i1 ] { | o1 } -> (Unit -> f [ | i2 ] { | o2 }) -> f [ | i ] { | o }
discard first cont = bind first (\_ -> cont unit)

-- | The **unary** sum→product strength for this direction: a **Mealy /
-- | coroutine step**, the dual of `RecordToVariant`'s `Resolving`. `retain`
-- | turns a transformer `p a b` into a step that consumes either a fresh input
-- | `a` or a resumed state `c`, emitting an output `b` together with the next
-- | state `c`:
-- |
-- | ```
-- | retain :: p a b -> p (Either a c) (Tuple b c)
-- |                        -- Left  a = fresh input
-- |                        -- Right c = resume from state
-- | ```
-- |
-- | State enters optionally (a branch of the sum input) and leaves guaranteed
-- | (product output), so the step *always* produces an output and the next
-- | state — a productive, stateful stream. Its binary, two-profunctor form is
-- | the `variantToRecord` merge above.
-- |
-- | There is deliberately **no `(->)` instance**: a stateless function has no
-- | `c` to place in the product on a fresh `Left a`, and no `b` on a `Right c`
-- | resume — the product output can't be filled without retaining state.
-- |
-- | This is the **bare strength** for the `+ → ×` direction (the analogue of
-- | `Strong`/`Choice`); the row combinator built on it is `reelWrap` below —
-- | exactly as `focusVariant` is built on `Choice`.
class Profunctor p <= Retaining p where
  retain :: forall a b c. p a b -> p (Either a c) (Tuple b c)

-- | Single-case specialization of `retain` — the `edit`-position combinator
-- | for this direction (the dual of `resolveProperty`). It threads one label
-- | `l` as **input case ↔ output field**: if the input variant carries case
-- | `l :: x`, its value resumes directly into output field `l` (the `Right`
-- | branch); otherwise the wrapped profunctor runs on the remaining cases and
-- | field `l` is filled from the carrier's retained state (the `c` that `retain`
-- | always emits), not from the wrapped profunctor.
retainCase
  :: forall @l p x i i' o o'
   . Retaining p
  => IsSymbol l
  => Cons l x i i'
  => Cons l x o o'
  => Lacks l o
  => p [ | i ] { | o }
  -> p [ | i' ] { | o' }
retainCase g =
  dimap
    (on (Proxy @l) Right Left)
    (\(Tuple r x) -> insert (Proxy @l) x r)
    (retain g)

-- | The single-case **focus** for this direction — the `+ → ×` analogue of
-- | `case_` (row-typed `left`), built on `retain` exactly as `case_` is built
-- | on `left`. Case `l` of the input variant is the focus fed to the wrapped
-- | `p a b` (the `Left`/fresh branch); the leftover `[ | r ]` cannot stay a
-- | variant inside the `Record` output, so — as in `reelWrap` — it is wrapped
-- | as a single output field `w`. Field `l` carries `p`'s output (drawn from
-- | the carrier's retained state when some other case arrived), field `w` the
-- | rest-variant. The single-case form of `reelWrap`; the transpose of
-- | `retainCase`, which runs the wrapped profunctor on the *other* cases and
-- | resumes the focused case directly.
caseToProperty
  :: forall @l @w p s r a b lo t
   . Retaining p
  => IsSymbol l
  => IsSymbol w
  => Cons l a r s
  => Cons l b () lo
  => Cons w [ | r ] lo t
  => Lacks w lo
  => p a b
  -> p [ | s ] { | t }
caseToProperty g =
  dimap
    (on (Proxy @l) Left Right)
    (\(Tuple b rest) -> insert (Proxy @w) rest (insert (Proxy @l) b {}))
    (retain g)

-- | The `+ → ×` member of the introduce family and the dual of `recordToCase`:
-- | the wrapped `p a { | o }` consumes case `l`'s value and produces the whole
-- | output record (as `caseToVariant`'s wrapped profunctor produces the whole output
-- | variant). Every *other* case must still yield a record, and a sum input
-- | can't supply one — it is replayed from the carrier's retained state, which
-- | is why this member alone needs `Retaining`. A Mealy **reducer**: case `l`
-- | updates the record via `g`, the remaining cases leave it as it was.
caseToRecord
  :: forall @l p s r a o
   . Retaining p
  => IsSymbol l
  => Cons l a r s
  => p a { | o }
  -> p [ | s ] { | o }
caseToRecord g = dimap (on (Proxy @l) Left Right) fst (retain g)

-- | The optic `retain` induces: the **Reel**. Eliminating the residual `c`
-- | (instantiated to `b → t`) by co-Yoneda collapses `∃c. (s → a + c) × (b × c → t)`
-- | to `s → Either a (b → t)` — a per-input dispatch that either surfaces a focus
-- | `a`, or supplies a *finisher* `b → t` drawn from retained state. Like a film
-- | reel: a wound transport that holds its position and never finishes.
type Reel s t a b = forall p. Retaining p => p a b -> p s t

reel :: forall s t a b. (s -> Either a (b -> t)) -> Reel s t a b
reel dispatch g = reelE dispatch (\(Tuple b f) -> f b) g

-- | Construct a `Reel` straight from its **existential encoding**
-- | `∃c. (s → a + c) × (b × c → t)`: pick the residual `c`, then supply `decon`
-- | (match `s` as a fresh focus `a` or a resumed state `c`) and `recon` (combine
-- | the focus result `b` with the carried state `c` into `t`). The quantified `c`
-- | is exactly the eliminator of that existential; `retain` is the carrier. `reel`
-- | is this at the co-Yoneda witness `c := b → t` (`recon = \(Tuple b f) -> f b`,
-- | i.e. evaluation).
reelE :: forall s t a b c. (s -> Either a c) -> (Tuple b c -> t) -> Reel s t a b
reelE decon recon g = dimap decon recon (retain g)

-- | Row existential `Reel` focusing a whole **sub-Variant `i`** of the full
-- | input `i'`; the residual is the **rest** `[ | rest ]` (`ExclusiveRows i
-- | rest i'`, the same split `focusVariant` uses). Crossing `+ → ×`, the rest
-- | can't stay a variant in the `Record` output, so it is **wrapped as a single
-- | output field `w`** — a record holding the variant (`o' = o` plus field `w`).
-- | The inner `p [ | i ] { | o }` runs on the focus; the retained
-- | rest-variant is inserted at field `w`. The mixed-direction analogue of
-- | `focusVariant`, and the dual of `shutterWrap` — same sub-row focus, but the
-- | complement is *wrapped* to cross into the record output rather than carried
-- | same-kind. The `+ → ×` row combinator over the bare strength `Retaining`,
-- | just as `focusVariant` is the row combinator over `Choice`.
-- |
-- | ```purescript
-- | -- focus the `cancel` case; wrap the rest into output field `pending`
-- | step :: Reel
-- |   [ cancel :: Unit, tick :: Int ]                               -- i'  full input
-- |   { done :: Boolean, pending :: [ tick :: Int ] }              -- o'  full output
-- |   [ cancel :: Unit ]                                            -- i   sub-Variant focus
-- |   { done :: Boolean }                                          -- o   inner output
-- | step = reelWrap @"pending"
-- | ```
reelWrap
  :: forall @w p i i' rest o o'
   . Retaining p
  => IsSymbol w
  => ExclusiveRows i rest i'
  => Contractable i' i
  => Contractable i' rest
  => Cons w [ | rest ] o o'
  => Lacks w o
  => p [ | i ] { | o }
  -> p [ | i' ] { | o' }
reelWrap g =
  reelE
    splitVariant
    (\(Tuple o v) -> insert (Proxy @w) v o)
    g
