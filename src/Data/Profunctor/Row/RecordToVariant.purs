module Data.Profunctor.Row.RecordToVariant
  ( bind
  , class RecordToVariant
  , discard
  , recordToVariant
  , class ResolvingRecordToVariant
  , resolve
  , resolveProperty
  )
  where

import Data.Either (Either, either)
import Data.Profunctor (class Profunctor, dimap)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (Variant, expand, inj)
import Prim.Row (class Cons, class Lacks, class Union)
import Record (delete, get)
import Type.Proxy (Proxy(..))
import Type.Row.Constraints (class InclusiveRows)

class Profunctor p <= RecordToVariant p where
  recordToVariant :: forall i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
    InclusiveRows i1 i2 i i12 i1x i2x =>
    InclusiveRows o1 o2 o o12 o1x o2x =>
    p (Record i1) (Variant o1) -> p (Record i2) (Variant o2) -> p (Record i) (Variant o)

bind :: forall f i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
  RecordToVariant f =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  InclusiveRows o1 o2 o o12 o1x o2x =>
  f (Record i1) (Variant o1) -> (f (Record i1) (Variant o1) -> f (Record i2) (Variant o2)) -> f (Record i) (Variant o)
bind first cont = recordToVariant first (cont first)

discard :: forall f i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
  RecordToVariant f =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  InclusiveRows o1 o2 o o12 o1x o2x =>
  f (Record i1) (Variant o1) -> (Unit -> f (Record i2) (Variant o2)) -> f (Record i) (Variant o)
discard first cont = bind first (\_ -> cont unit)

-- | The **unary** product→sum strength for this direction: a single **loop /
-- | iteration step**. `resolve` runs a transformer `p a b` on an input `a`
-- | alongside a carried state `c`, returning a `Step`:
-- |
-- | ```
-- | resolve :: p a b -> p (Tuple a c) (Either b c)
-- |                                      -- Left  b = Done b  (finish)
-- |                                      -- Right c = Loop c  (continue)
-- | ```
-- |
-- | State enters guaranteed (product input) and leaves optionally (a branch of
-- | the sum output), so the step may *halt*; closing the `c` channel gives `p`
-- | a terminating iteration (`tailRec`-style). It is the `identity`-pinned form
-- | of the binary base merge `Data.Profunctor.ProductToSum.prosum` (its second
-- | operand fixed to `identity`) — the product→sum analogue of how
-- | `StrongRecordToRecord`/`focusRecord` is the unary form of `recordToRecord`.
-- |
-- | (No `(->)` instance: the only one would be the trivial always-`Done` step,
-- | which carries no iteration — this class is for profunctors that actually loop.)
class Profunctor p <= ResolvingRecordToVariant p where
  resolve :: forall a b c. p a b -> p (Tuple a c) (Either b c)

-- | Single-field specialization of `resolve` — the `edit`-position combinator
-- | for this direction (the analogue of `editProperty`/`editCase` when input and
-- | output kinds differ). It threads one label `l` as **input field ↔ output
-- | case**: field `l :: x` is split off the input record, and the wrapped
-- | profunctor either runs on the rest (emitting some case of `o`, the `Done`
-- | branch) or the field's value escapes directly as output case `l` (the
-- | `Loop`/short-circuit branch).
resolveProperty
  :: forall @l p x lo i i' o o'
   . ResolvingRecordToVariant p
  => IsSymbol l
  => Cons l x i i'
  => Lacks l i
  => Cons l x o o'
  => Cons l x () lo
  => Union o lo o'
  => p (Record i) (Variant o)
  -> p (Record i') (Variant o')
resolveProperty g =
  dimap
    (\s -> Tuple (delete (Proxy @l) s) (get (Proxy @l) s))
    (either expand (inj (Proxy @l)))
    (resolve g)
