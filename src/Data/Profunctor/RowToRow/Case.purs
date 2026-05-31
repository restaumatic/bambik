-- | Single-case row profunctors over `Variant`s — the transpose-duals of
-- | `Data.Profunctor.RowToRow.Property`.
-- |
-- |   * `eliminateCase` — shrink: consume a case via a diverging `p case Void` handler,
-- |     built on `RowChoice` (i.e. `Choice`'s `left`); the survivors pass through.
-- |   * `editCase`       — focus an existing case in place: the standard variant prism
-- |     (`Data.Lens.Extra.Commons.variant`), the value-level single-case convenience.
-- |   * `introduceCase`  — grow: inject a new case from a spontaneous source. This is the
-- |     one operation outside `Choice`, so it rests on `IntroVarP` rather than `RowChoice`.
-- |
-- | For focusing a whole **sub-variant** with a `Variant → Variant` profunctor, see
-- | `RowChoice.focusVariant`.
module Data.Profunctor.RowToRow.Case
  ( introduceCase
  , eliminateCase
  , editCase
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Lens (Optic, Prism)
import Data.Lens.Extra.Commons (variant) as Commons
import Data.Profunctor (dimap, rmap)
import Data.Profunctor.Choice (left)
import Data.Profunctor.RowToRow.IntroVarP (class IntroVarP, liftIntroVar)
import Data.Profunctor.RowToRow.RowChoice (class RowChoice)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, expand, inj, on)
import Prim.Row (class Cons, class Union)
import Type.Proxy (Proxy(..))

-- | Introduce a new case `l` from a spontaneous source, preserving the existing cases.
-- | The only sum operation outside `Choice` — see `IntroVarP`.
introduceCase
  :: forall p @l case_ s t r
   . IsSymbol l
  => Cons l case_ s t
  => Union s r t
  => IntroVarP p
  => Optic p (Variant s) (Variant t) Void case_
introduceCase src =
  rmap
    (case _ of
       Left vars -> expand vars
       Right i -> inj (Proxy @l) i)
    (liftIntroVar src)

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
