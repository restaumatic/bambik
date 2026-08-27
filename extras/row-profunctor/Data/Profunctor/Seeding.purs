-- | **Pointedness as carrier structure.** An entity (a record channel) has a
-- | known initial state, available from the very beginning; an event (a
-- | variant channel) merely occurs at some times. The carriers that can act
-- | on this distinction — stateful ones with a registration moment — expose
-- | it as one primitive: the **point**, `announce a :: p {} a`, a map out of
-- | the terminal record that emits `a` exactly once, at registration, and
-- | is fed nothing worth reacting to (`{}` carries no information, so a
-- | feed of it is ignored).
-- |
-- | Laws (with `t=0` the carrier's registration moment):
-- |
-- |   * **point**: at registration, `announce a` emits `a`, exactly once,
-- |     before any input arrives;
-- |   * **closed**: after registration, `announce a` never emits again —
-- |     feeding it `{}` changes nothing.
-- |
-- | The **seeded echo wire** `seeded a :: p a a` — `identity`'s pass-through
-- | plus the point — is *derived*, through `Choice`: the point sits on the
-- | `Left` branch, the wire on the `Right`, and both branches rejoin,
-- |
-- | ```
-- | seeded a = dimap Right (either identity identity) (left (announce a))
-- | ```
-- |
-- | so the pointed wire needs no second primitive (`Choice` is therefore a
-- | superclass, beside `Category` for the wire). It is the initial-state
-- | supply the knot-tying row forms build on (`feedback`/`folding`/
-- | `unfolding` prime their state channels by composing a `seeded` into the
-- | traced chain), while `with a w = announce a >>> w` is the discharge
-- | form (`Data.Profunctor.Row.RecordToRecord`). The merge units are *not*
-- | pointed: `pempty` is the wire at the unit row (`identity @{}`), and the
-- | gates ignore a contribution of zero fields — pointing is this class's
-- | business alone. There is deliberately no `(->)` instance: a timeless
-- | carrier has no registration moment to emit at — pointedness is exactly
-- | what distinguishes a stateful carrier's record channels from bare
-- | functions.
module Data.Profunctor.Seeding
  ( class Seeding
  , announce
  , seeded
  ) where

import Control.Category (class Category, identity)
import Data.Either (Either(..), either)
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (class Choice, left)

class (Category p, Choice p) <= Seeding p where
  announce :: forall a. a -> p {} a

seeded :: forall p a. Seeding p => a -> p a a
seeded a = dimap Right (either identity identity) (left (announce a))
