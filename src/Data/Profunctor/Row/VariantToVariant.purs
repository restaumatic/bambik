-- | `Variant → Variant` row profunctors, in three layers:
-- |
-- |   * `variantToVariant` — the n-ary **merge** class: combine two complete variant-shaped
-- |     sub-profunctors (dispatch inputs, merge outputs).
-- |   * `ChoiceVariantToVariant`/`focusVariant` — the row-typed **`Choice`**: focus a whole
-- |     sub-variant, carrying the complement (`left`/`right`, relabeled to rows).
-- |   * `eliminateCase`/`editCase` — the single-case **combinators** built on
-- |     `ChoiceVariantToVariant`. (Introducing a *fresh* case is the one operation outside
-- |     `Choice`; it's built via the `Sum`/`variantToVariant` path, not a focus combinator.)
module Data.Profunctor.Row.VariantToVariant
  ( bind
  , variantToVariant
  , class VariantToVariant
  , discard
  , class ChoiceVariantToVariant
  , focusVariant
  , eliminateCase
  , editCase
  )
  where

import Control.Category (identity)
import Data.Either (Either(..), either)
import Data.Lens (Optic, Prism)
import Data.Lens.Extra.Commons (variant) as Commons
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Symbol (class IsSymbol)
import Data.Unit (Unit, unit)
import Data.Variant (class Contractable, Variant, contract, expand, on)
import Data.Void (Void, absurd)
import Effect.Exception.Unsafe (unsafeThrow)
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))
import Type.Row.Constraints (class DispatchableVariants, class ExclusiveRows, class InclusiveRows)

class Profunctor p <= VariantToVariant p where
  variantToVariant :: forall i1 i1l i2 i2l o1 o2 o12 o1x o2x i o.
    ExclusiveRows i1 i2 i =>
    InclusiveRows o1 o2 o o12 o1x o2x =>
    DispatchableVariants i1 i2 i1l i2l =>
    p (Variant i1) (Variant o1) -> p (Variant i2) (Variant o2) -> p (Variant i) (Variant o)

bind :: forall f i1 i1l i2 i2l o1 o2 o12 o1x o2x i o.
  VariantToVariant f =>
  ExclusiveRows i1 i2 i =>
  InclusiveRows o1 o2 o o12 o1x o2x =>
  DispatchableVariants i1 i2 i1l i2l =>
  f (Variant i1) (Variant o1) -> (f (Variant i1) (Variant o1) -> f (Variant i2) (Variant o2)) -> f (Variant i) (Variant o)
bind first cont = variantToVariant first (cont first)

discard :: forall f i1 i1l i2 i2l o1 o2 o12 o1x o2x i o.
  VariantToVariant f =>
  ExclusiveRows i1 i2 i =>
  InclusiveRows o1 o2 o o12 o1x o2x =>
  DispatchableVariants i1 i2 i1l i2l =>
  f (Variant i1) (Variant o1) -> (Unit -> f (Variant i2) (Variant o2)) -> f (Variant i) (Variant o)
discard first cont = bind first (\_ -> cont unit)

-- | Row-typed `Choice`: focus a **sub-variant** `sub`, transforming it while carrying the
-- | complement `rest` of the cases unchanged. The coproduct dual of `StrongRecordToRecord`
-- | — operates on rows on **both sides**:
-- |
-- | ```
-- | focusVariant :: p (Variant sub) (Variant sub') -> p (Variant s) (Variant t)
-- |               -- where s = sub ∪ rest,  t = sub' ∪ rest   (ExclusiveRows)
-- | ```
-- |
-- | The labeled analogue of `Choice`'s `left`/`right`, carrying the complement *row* `rest`.
-- | Equivalent to `Choice` (generic instance below): dispatch `s` into `sub | rest` (via
-- | `Data.Variant.contract`), run the argument on the `sub` branch via `left`, and re-merge
-- | both branches into `t` (via `expand`).
class Choice p <= ChoiceVariantToVariant p where
  focusVariant
    :: forall sub sub' rest s t
     . ExclusiveRows sub rest s
    => ExclusiveRows sub' rest t
    => Contractable s sub
    => Contractable s rest
    => p (Variant sub) (Variant sub')
    -> p (Variant s) (Variant t)

instance Choice p => ChoiceVariantToVariant p where
  focusVariant g = dimap splitVariant mergeVariant (left g)

-- Dispatch a wider variant into the focused sub-variant or the complement.
splitVariant
  :: forall sub rest s
   . ExclusiveRows sub rest s
  => Contractable s sub
  => Contractable s rest
  => Variant s
  -> Either (Variant sub) (Variant rest)
splitVariant v = case contract v of
  Just sub -> Left sub
  Nothing -> case contract v of
    Just rest -> Right rest
    Nothing -> unsafeThrow "ChoiceVariantToVariant.focusVariant: case in neither sub nor rest"

-- Re-merge the (possibly transformed) sub-variant and the complement into the wider variant.
mergeVariant
  :: forall sub' rest t
   . ExclusiveRows sub' rest t
  => Either (Variant sub') (Variant rest)
  -> Variant t
mergeVariant = either expand expand

-- | Eliminate the case `l` via a diverging handler `p case Void`, preserving the rest.
-- | Built on `ChoiceVariantToVariant` (`Choice`'s `left`): the routed `Left` case exits
-- | through the `Void` slot, the survivors pass `Right`.
eliminateCase
  :: forall p @l case_ s t
   . IsSymbol l
  => Cons l case_ t s
  => ChoiceVariantToVariant p
  => Optic p (Variant s) (Variant t) case_ Void
eliminateCase handler =
  dimap (on (Proxy @l) Left Right) (either absurd identity) (left handler)

-- | Focus an existing case in place — the standard `Choice` prism.
editCase
  :: forall @l s r a
   . IsSymbol l
  => Cons l a r s
  => Prism (Variant s) (Variant s) a a
editCase = Commons.variant @l
