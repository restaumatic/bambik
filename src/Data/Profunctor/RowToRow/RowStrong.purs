-- | Row-typed `Strong`: focus a **sub-record** `sub`, transforming it while carrying the
-- | complement `rest` of the row unchanged. Operates on rows on **both sides** — the
-- | argument is itself a `Record → Record` profunctor:
-- |
-- | ```
-- | focusRecord :: p (Record sub) (Record sub') -> p (Record s) (Record t)
-- |              -- where s = sub ∪ rest,  t = sub' ∪ rest   (ExclusiveRows)
-- | ```
-- |
-- | The labeled analogue of `Strong`'s `first`/`second`: instead of carrying a positional
-- | complement `c`, it carries the complement *row* `rest`, split off by `ExclusiveRows`.
-- | Equivalent to `Strong` (generic instance below): split `s` into `(sub, rest)`, run the
-- | argument on `sub` via `first`, and re-merge `sub'` with `rest`.
module Data.Profunctor.RowToRow.RowStrong
  ( class RowStrong
  , focusRecord
  ) where

import Data.Profunctor (dimap)
import Data.Profunctor.Strong (class Strong, first)
import Data.Tuple (Tuple(..))
import Prim.Row (class Union)
import Record (union) as Record
import Type.Row.Constraints (class ExclusiveRows)
import Unsafe.Coerce (unsafeCoerce)

class Strong p <= RowStrong p where
  focusRecord
    :: forall sub sub' rest s t
     . ExclusiveRows sub rest s
    => ExclusiveRows sub' rest t
    => p (Record sub) (Record sub')
    -> p (Record s) (Record t)

instance Strong p => RowStrong p where
  focusRecord g =
    dimap (\s -> Tuple (pick s) (pick s))
          -- `Record.union` is left-biased and does not nub; safe here only because
          -- `ExclusiveRows sub' rest t` guarantees `sub'` and `rest` are disjoint.
          (\(Tuple sub' rest) -> Record.union sub' rest)
          (first g)

-- Project a sub-record out of a wider record. Sound because PureScript records are JS
-- objects and `Union narrow extra wider` witnesses `narrow ⊆ wider`.
pick :: forall narrow extra wider. Union narrow extra wider => Record wider -> Record narrow
pick = unsafeCoerce
