-- | Single-case row profunctors over `Variant`s — the transpose-duals of
-- | `Data.Profunctor.RowToRow.Property`.
-- |
-- |   * `eliminateCase` — shrink: consume a case via a diverging `p case Void` handler,
-- |     built on `RowChoice` (i.e. `Choice`'s `left`); the survivors pass through.
-- |   * `editCase`       — focus an existing case in place: the standard variant prism
-- |     (`Data.Lens.Extra.Commons.variant`), the value-level single-case convenience.
-- |
-- | (Introducing a *new* case from a spontaneous source is the one operation outside
-- | `Choice`; in this codebase that is built via the `Sum`/`VariantToVariant` composition
-- | path, not a dedicated focus combinator.) For focusing a whole **sub-variant** with a
-- | `Variant → Variant` profunctor, see `RowChoice.focusVariant`.
module Data.Profunctor.RowToRow.Case
  ( eliminateCase
  , editCase
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Lens (Optic, Prism)
import Data.Lens.Extra.Commons (variant) as Commons
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (left)
import Data.Profunctor.RowToRow.RowChoice (class RowChoice)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, on)
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))

-- | Eliminate the case `l` via a diverging handler `p case Void`, preserving the rest.
-- | Built on `RowChoice` (`Choice`'s `left`): the routed `Left` case exits through the
-- | `Void` slot, the survivors pass `Right`.
eliminateCase
  :: forall p @l case_ s t
   . IsSymbol l
  => Cons l case_ t s
  => RowChoice p
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
