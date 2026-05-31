-- | Row-typed `Strong`: focus a single **named field** `l`, transforming it `a -> b`
-- | while carrying the rest of the row `r` unchanged.
-- |
-- | The labeled analogue of `Strong`'s `first`/`second`: one label-indexed method replaces
-- | the two positional ones, and the rest-row `r` plays the role of the carried complement
-- | `c`. It is *equivalent* to `Strong` — every `Strong` is a `RowStrong` (the generic
-- | instance below) and `focusField` is just the standard record lens
-- | `Data.Lens.Record.prop`. So `RowStrong p` is, as a constraint, interchangeable with
-- | `Strong p`; it exists to give the row-native focus primitive the rest of the
-- | row-profunctor code is written against.
module Data.Profunctor.RowToRow.RowStrong
  ( class RowStrong
  , focusField
  ) where

import Data.Lens.Record (prop)
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)

class Strong p <= RowStrong p where
  focusField
    :: forall l a b r s t
     . IsSymbol l
    => Cons l a r s
    => Cons l b r t
    => Proxy l
    -> p a b
    -> p (Record s) (Record t)

instance Strong p => RowStrong p where
  focusField l = prop l
