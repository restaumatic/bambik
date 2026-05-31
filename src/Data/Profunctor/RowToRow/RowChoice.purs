-- | Row-typed `Choice`: focus a **sub-variant** `sub`, transforming it while carrying the
-- | complement `rest` of the cases unchanged. The coproduct dual of
-- | `Data.Profunctor.RowToRow.RowStrong` — operates on rows on **both sides**:
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
module Data.Profunctor.RowToRow.RowChoice
  ( class RowChoice
  , focusVariant
  ) where

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Variant (class Contractable, Variant, contract, expand)
import Effect.Exception.Unsafe (unsafeThrow)
import Type.Row.Constraints (class ExclusiveRows)

class Choice p <= RowChoice p where
  focusVariant
    :: forall sub sub' rest s t
     . ExclusiveRows sub rest s
    => ExclusiveRows sub' rest t
    => Contractable s sub
    => Contractable s rest
    => p (Variant sub) (Variant sub')
    -> p (Variant s) (Variant t)

instance Choice p => RowChoice p where
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
    Nothing -> unsafeThrow "RowChoice.focusVariant: case in neither sub nor rest"

-- Re-merge the (possibly transformed) sub-variant and the complement into the wider variant.
mergeVariant
  :: forall sub' rest t
   . ExclusiveRows sub' rest t
  => Either (Variant sub') (Variant rest)
  -> Variant t
mergeVariant = either expand expand
