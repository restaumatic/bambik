-- | Row-typed `Choice`: focus a single **named case** `l`, transforming its payload
-- | `a -> b` while carrying the rest of the variant `r` unchanged.
-- |
-- | The labeled analogue of `Choice`'s `left`/`right` (one label-indexed method instead of
-- | two positional ones; the rest-row `r` is the carried complement). Equivalent to
-- | `Choice` — every `Choice` is a `RowChoice` (generic instance below), with `focusCase`
-- | built from `left`. The coproduct dual of `Data.Profunctor.RowToRow.RowStrong`.
module Data.Profunctor.RowToRow.RowChoice
  ( class RowChoice
  , focusCase
  ) where

import Data.Either (Either(..), either)
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, expand, inj, on)
import Prim.Row (class Cons, class Union)
import Type.Proxy (Proxy)

class Choice p <= RowChoice p where
  focusCase
    :: forall l a b r rx s t
     . IsSymbol l
    => Cons l a r s
    => Cons l b r t
    => Union r rx t
    => Proxy l
    -> p a b
    -> p (Variant s) (Variant t)

instance Choice p => RowChoice p where
  focusCase l f = dimap (on l Left Right) (either (inj l) expand) (left f)
