-- | The CPS profunctor — **parked**: nothing in the library, the demos or the
-- | tests reaches it (L14), so it lives outside the build glob.
-- |
-- | `Cont r a b = (b -> r) -> (a -> r) ≅ a -> ((b -> r) -> r)`: the `Star` of
-- | the continuation monad `K r`, which is where most of the instances below
-- | come from. It is the repo's only *pure* carrier of the row algebra — a
-- | timeless model in which the merge gate is continuation nesting rather
-- | than a pair of `Ref`s — so it is the natural home for value-level laws
-- | that today have to be stated on `PUI Effect` probes.
-- |
-- | What it validly inhabits, and why:
-- |
-- |   * `Strong`/`Choice` — Tambara over × and +
-- |   * `Semigroupoid`/`Category` — CPS composition
-- |   * `Wander` — instantiate a traversal's `Applicative` at `K r`
-- |   * `Acting` — `Wander` at `traverse`; the key is ignored, exactly as in
-- |     `actedBy _ = map` for `(->)`
-- |   * `Cochoice` — the continuation sits in tail position, so the `Right c`
-- |     branch re-enters: `tailRec` at the optic level, and the honest
-- |     semantics of `iterate` (it may diverge; that is what iteration is)
-- |   * `RecordToRecord` — the ×→× gate as continuation nesting: `p1` runs
-- |     under a continuation that runs `p2`, the union at the innermost point
-- |   * `VariantToVariant` — one input case reaches exactly one operand, so
-- |     no combining is needed
-- |   * `RecordToVariant` — needs `Monoid r`: both operands are fed and both
-- |     may emit, so two answers must combine, and `pempty` must be silence
-- |
-- | `Resolving`/`Coretaining` typecheck but are degenerate and stated here
-- | only to record that: `resolve` can only ever take `Left` (without time
-- | there is no "still moving"), and `coretain` must drop the state.
-- |
-- | What it cannot inhabit, and why — the same reasons that shape the
-- | library's seeded trace forms:
-- |
-- |   * `Costrong`/`Coresolving` — `unfirst`/`coresolve` need a `c` on the
-- |     *input* side before any output exists. Contrast `Cochoice`, where
-- |     `Left a` needs none: that asymmetry is why `looped` is a primitive
-- |     and `feedback`/`folding` take a seed
-- |   * `Retaining` — must produce a `b` for a `Right c` input; a stateless
-- |     carrier has none (same reason there is no `(->)` instance)
-- |   * `VariantToRecord` — an input case reaches one operand only, so the
-- |     other never contributes; retention is what `PUI`'s `Ref`s supply
-- |   * `Seeding` — needs a registration moment; a timeless carrier has none
-- |   * `Closed` — would have to extract a `b` per `x`, and CPS only ever
-- |     hands `b` to a continuation
module Data.Profunctor.Cont where

import Prelude

import Data.Either (Either(..))
import Data.Lens.Internal.Wander (class Wander)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Acting (class Acting)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Cochoice (class Cochoice)
import Data.Profunctor.Row (exactRow, widenRecordInput, widenVariantOutput)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord)
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant, class Resolving)
import Data.Profunctor.Row.VariantToRecord (class Coretaining)
import Data.Profunctor.Row.VariantToVariant (class VariantToVariant, splitVariant)
import Data.Profunctor.Strong (class Strong)
import Data.Traversable (traverse)
import Record as Record
import Data.Tuple (Tuple(..))

newtype Cont r a b = Cont ((b -> r) -> (a -> r))

derive instance Newtype (Cont r a b) _

-- the continuation functor `Cont r` is a Star of: `Cont r a b ≅ a -> K r b`
newtype K r b = K ((b -> r) -> r)

instance Functor (K r) where
  map f (K g) = K \k -> g (k <<< f)

instance Apply (K r) where
  apply (K f) (K a) = K \k -> f \g -> a (k <<< g)

instance Applicative (K r) where
  pure b = K \k -> k b

star :: forall r a b. Cont r a b -> a -> K r b
star p a = K \k -> unwrap p k a

unstar :: forall r a b. (a -> K r b) -> Cont r a b
unstar f = wrap \k a -> case f a of K g -> g k

instance Profunctor (Cont r) where
  dimap f g r = wrap \br -> (unwrap r) (br <<< g) <<< f

instance Strong (Cont r) where
  first r = wrap \bd2r (Tuple a d) -> unwrap r (\b -> bd2r (Tuple b d)) a
  second r = wrap \db2r (Tuple d a) -> unwrap r (\b -> db2r (Tuple d b)) a

instance Choice (Cont r) where
  left r = wrap \bd2r -> case _ of
    Left a -> unwrap r (\b -> bd2r (Left b)) a
    Right d -> bd2r (Right d)
  right r = wrap \db2r -> case _ of
    Right a -> unwrap r (\b -> db2r (Right b)) a
    Left d -> db2r (Left d)

instance Semigroupoid (Cont r) where
  compose f g = wrap \cr -> unwrap g (unwrap f cr)

instance Category (Cont r) where
  identity = wrap identity

instance Wander (Cont r) where
  wander trav p = unstar (trav (star p))

instance Acting (Cont r) where
  actedBy _ p = unstar (traverse (star p))

instance Cochoice (Cont r) where
  unleft p = wrap \k ->
    let go e = unwrap p (case _ of
          Left b -> k b
          Right c -> go (Right c)) e
    in go <<< Left
  unright p = wrap \k ->
    let go e = unwrap p (case _ of
          Right b -> k b
          Left c -> go (Left c)) e
    in go <<< Right

instance RecordToRecord (Cont r) where
  pempty = identity
  recordToRecord p1 p2 = wrap \k i ->
    unwrap (widenRecordInput p1)
      (\o1 -> unwrap (widenRecordInput p2)
        (\o2 -> k (Record.union (exactRow o1) (exactRow o2))) i) i

instance VariantToVariant (Cont r) where
  pempty = identity
  variantToVariant p1 p2 = wrap \k v -> case splitVariant v of
    Left v1 -> unwrap (widenVariantOutput p1) k v1
    Right v2 -> unwrap (widenVariantOutput p2) k v2

instance Monoid r => RecordToVariant (Cont r) where
  pempty = wrap \_ _ -> mempty
  recordToVariant p1 p2 = wrap \k i ->
    unwrap (widenVariantOutput (widenRecordInput p1)) k i
      <> unwrap (widenVariantOutput (widenRecordInput p2)) k i

instance Resolving (Cont r) where
  resolve p = wrap \k (Tuple a _) -> unwrap p (k <<< Left) a

instance Coretaining (Cont r) where
  coretain p = wrap \k a -> unwrap p (\(Tuple b _) -> k b) (Left a)

run :: forall a b. Cont b a b -> a -> b
run cont a = unwrap cont identity a

introduce :: forall r a b. (a -> b) -> Cont r a b
introduce a2b = wrap (\b2r -> a2b >>> b2r)

introduce' :: forall r a b. b -> Cont r a b
introduce' b = introduce (const b)

eliminate :: forall a r. (a -> r) -> Cont r a Void
eliminate a2r = wrap (\_ a -> a2r a)

eliminate' :: forall a . Cont a a Void
eliminate' = eliminate identity
