module Data.Profunctor.Row.VariantToRecord
  ( Reel
  , bind
  , variantToRecord
  , class VariantToRecord
  , discard
  , class RetainingVariantToRecord
  , retain
  , retainCase
  , reel
  )
  where

import Data.Either (Either(..))
import Data.Profunctor (class Profunctor, dimap)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (Variant, on)
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
-- | coroutine step**, the dual of `RecordToVariant`'s `ResolvingRecordToVariant`. `retain`
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
class Profunctor p <= RetainingVariantToRecord p where
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
   . RetainingVariantToRecord p
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
-- | (Polish: *Szpula*.)
type Reel s t a b = forall p. RetainingVariantToRecord p => p a b -> p s t

reel :: forall s t a b. (s -> Either a (b -> t)) -> Reel s t a b
reel dispatch g = dimap dispatch (\(Tuple b f) -> f b) (retain g)
