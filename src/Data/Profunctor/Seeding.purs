-- | **Pointedness as carrier structure.** An entity (a record channel) has a
-- | known initial state, available from the very beginning; an event (a
-- | variant channel) merely occurs at some times. The carriers that can act
-- | on this distinction — stateful ones with a registration moment — expose
-- | it as one primitive: the **seeded echo wire**, `identity`'s pass-through
-- | plus a single emission of the seed at registration.
-- |
-- | Laws (with `t=0` the carrier's registration moment):
-- |
-- |   * **wire**: after registration, `seeded a` forwards every input to the
-- |     output unchanged — `seeded a ≅ identity` from `t>0` on;
-- |   * **point**: at registration, `seeded a` emits `a`, exactly once,
-- |     before any input arrives.
-- |
-- | This is the initial-state supply the knot-tying row forms build on
-- | (`feedback`/`folding`/`unfolding` prime their state channels by
-- | composing a `seeded` into the traced chain), and what `PUI`'s
-- | `announce`/`with` close over (`announce a ≅ lcmap (const {}) (seeded a)`
-- | up to the informationless input, and `with a w = announce a >>> w` is
-- | the discharge form). There is deliberately no
-- | `(->)` instance: a timeless carrier has no registration moment to emit
-- | at — pointedness is exactly what distinguishes a stateful carrier's
-- | record channels from bare functions.
module Data.Profunctor.Seeding
  ( class Seeding
  , seeded
  ) where

import Control.Category (class Category)

class Category p <= Seeding p where
  seeded :: forall a. a -> p a a
