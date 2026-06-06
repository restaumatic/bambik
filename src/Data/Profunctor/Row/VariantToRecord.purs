module Data.Profunctor.Row.VariantToRecord
  ( Reel
  , bind
  , variantToRecord
  , class VariantToRecord
  , discard
  , class Retaining
  , class RetainingVariantToRecord
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
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (class Contractable, Variant, on)
import Prim.Row (class Cons, class Lacks)
import Record (insert)
import Type.Proxy (Proxy(..))
import Type.Row.Constraints (class DispatchableVariants, class ExclusiveRows)

class Profunctor p <= VariantToRecord p where
  variantToRecord :: forall i1 i1l i2 i2l o1 o2 i o.
    ExclusiveRows i1 i2 i =>
    ExclusiveRows o1 o2 o =>
    DispatchableVariants i1 i2 i1l i2l =>
    p (Variant i1) (Record o1) -> p (Variant i2) (Record o2) -> p (Variant i) (Record o)

bind :: forall f i1 i1l i2 i2l o1 o2 i o.
  VariantToRecord f =>
  ExclusiveRows i1 i2 i =>
  ExclusiveRows o1 o2 o =>
  DispatchableVariants i1 i2 i1l i2l =>
  f (Variant i1) (Record o1) -> (f (Variant i1) (Record o1) -> f (Variant i2) (Record o2)) -> f (Variant i) (Record o)
bind first cont = variantToRecord first (cont first)

discard :: forall f i1 i1l i2 i2l o1 o2 i o.
  VariantToRecord f =>
  ExclusiveRows i1 i2 i =>
  ExclusiveRows o1 o2 o =>
  DispatchableVariants i1 i2 i1l i2l =>
  f (Variant i1) (Record o1) -> (Unit -> f (Variant i2) (Record o2)) -> f (Variant i) (Record o)
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
-- | `Strong`/`Choice`); the row combinator built on it lives in the row class
-- | `RetainingVariantToRecord` below — exactly as `focusVariant` lives in
-- | `ChoiceVariantToVariant` (built on `Choice`).
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
  => p (Variant i) (Record o)
  -> p (Variant i') (Record o')
retainCase g =
  dimap
    (on (Proxy @l) Right Left)
    (\(Tuple r x) -> insert (Proxy @l) x r)
    (retain g)

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

-- | The **row-typed** class for this direction — the `+ → ×` analogue of
-- | `ChoiceVariantToVariant` (row-typed `Choice`). `Retaining` above is the bare
-- | strength; `RetainingVariantToRecord` adds the row combinator `reelWrap`, and
-- | the generic `instance Retaining p => RetainingVariantToRecord p` gives it to
-- | every `Retaining` profunctor for free (just as every `Choice` is a
-- | `ChoiceVariantToVariant`).
class Retaining p <= RetainingVariantToRecord p where
  -- | Row existential `Reel` focusing a whole **sub-Variant `i`** of the full
  -- | input `i'`; the residual is the **rest** `Variant rest` (`ExclusiveRows i
  -- | rest i'`, the same split `focusVariant` uses). Crossing `+ → ×`, the rest
  -- | can't stay a variant in the `Record` output, so it is **wrapped as a single
  -- | output field `w`** — a record holding the variant (`o' = o` plus field `w`).
  -- | The inner `p (Variant i) (Record o)` runs on the focus; the retained
  -- | rest-variant is inserted at field `w`. The mixed-direction analogue of
  -- | `focusVariant`, and the dual of `shutterWrap` — same sub-row focus, but the
  -- | complement is *wrapped* to cross into the record output rather than carried
  -- | same-kind. The wrapper label is a `Proxy w` (instance methods can't bind a
  -- | visible `@w` for the body):
  -- |
  -- | ```purescript
  -- | -- focus the `cancel` case; wrap the rest into output field `pending`
  -- | step :: Reel
  -- |   (Variant (cancel :: Unit, tick :: Int))                       -- i'  full input
  -- |   (Record (done :: Boolean, pending :: Variant (tick :: Int)))  -- o'  full output
  -- |   (Variant (cancel :: Unit))                                    -- i   sub-Variant focus
  -- |   (Record (done :: Boolean))                                    -- o   inner output
  -- | step = reelWrap (Proxy @"pending")
  -- | ```
  reelWrap
    :: forall w i i' rest o o'
     . IsSymbol w
    => ExclusiveRows i rest i'
    => Contractable i' i
    => Contractable i' rest
    => Cons w (Variant rest) o o'
    => Lacks w o
    => Proxy w
    -> p (Variant i) (Record o)
    -> p (Variant i') (Record o')

instance Retaining p => RetainingVariantToRecord p where
  reelWrap pw g =
    reelE
      splitVariant
      (\(Tuple o v) -> insert pw v o)
      g
