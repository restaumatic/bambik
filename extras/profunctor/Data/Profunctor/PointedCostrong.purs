-- | The **pointed** `×`-trace — `Data.Profunctor.Costrong`'s `unfirst` given
-- | a starting point for its state channel, stated **positionally**
-- | (`Tuple`) with no row in sight. The row form built on it is
-- | `Data.Profunctor.Row.RecordToRecord.feedback`.
-- |
-- | Why a class of its own: on a gated carrier the raw composite
-- | `unfirst (first g)` is dead — `unfirst` withholds every input until the
-- | chain has emitted a state, and the chain cannot emit before it is fed —
-- | so priming it from inside needs a whole `Tuple a0 c0`, i.e. a starting
-- | value for an input the enclosing pipeline already supplies. The point
-- | belongs on the state, not on the input: `unfirstFrom c0` starts the
-- | state channel at `c0`, the first input is joined with it, and every
-- | emission's `c` replaces it from then on. It is not derivable from
-- | `Costrong` plus `Seeding`, since neither can write the state channel
-- | without an emission on the output channel.
-- |
-- | Laws:
-- |
-- | ```
-- | unfirstFrom c (first g) ≈ g                                   -- yanking
-- | unfirstFrom c (lcmap (second f) g)
-- |   ≈ unfirstFrom (f c) (rmap (second f) g)                     -- sliding
-- | ```
-- |
-- | On a timeless carrier the state is never updated (there is no next
-- | input to carry it to), so the instance for `(->)` feeds `c` to every
-- | call — degenerate but lawful, like `Resolving`'s. A **complement of the
-- | ecosystem's own**, hence the `Data.Profunctor.*` name and the separate
-- | `extras/profunctor` source root: nothing here mentions `PUI`, a row, or
-- | a carrier.
module Data.Profunctor.PointedCostrong
  ( class PointedCostrong
  , unfirstFrom
  )
  where

import Data.Tuple (Tuple(..), fst)
import Data.Profunctor (class Profunctor)

-- | Tie the state channel `c` of a chain, starting it at the given value.
class Profunctor p <= PointedCostrong p where
  unfirstFrom :: forall a b c. c -> p (Tuple a c) (Tuple b c) -> p a b

instance PointedCostrong (->) where
  unfirstFrom c g a = fst (g (Tuple a c))
