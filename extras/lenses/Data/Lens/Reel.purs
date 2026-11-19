-- | The **`Reel`** — the profunctor-encoded optic of
-- | `Data.Profunctor.Retaining`'s `retain`, for the coined `+ → ×` shape;
-- | both the class and the optic are this library's, its co-optic
-- | `Data.Lens.Coreel`. It has no row form (`Data.Profunctor.Row`, *What
-- | is deliberately not here*). Like the other coined optics, the
-- | existential constructor `reelE` is **sound** and completeness is **not
-- | claimed** (`Data.Lens.Shutter`'s note; here the existential does
-- | contain `identity`, at `c := Void`, but the multi-application
-- | inhabitants are still unaccounted for). An optic over a strength,
-- | mentioning no `PUI`, row or carrier — hence the `Data.Lens.*` name and
-- | the `extras/lenses` root, travelling with its coined class.
-- | Law-tested, reached by no demo.
module Data.Lens.Reel
  ( Reel
  , reel
  , reelE
  )
  where

import Data.Either (Either)
import Data.Profunctor (dimap)
import Data.Profunctor.Retaining (class Retaining, retain)
import Data.Tuple (Tuple(..))

-- | The optic `retain` encodes: the **Reel**. Eliminating the residual `c`
-- | (instantiated to `b → t`) by co-Yoneda collapses `∃c. (s → a + c) × (b × c → t)`
-- | to `s → Either a (b → t)` — a per-input dispatch that either surfaces a focus
-- | `a`, or supplies a *finisher* `b → t` drawn from retained state. Like a film
-- | reel: a wound transport that holds its position and never finishes.
type Reel s t a b = forall p. Retaining p => p a b -> p s t

reel :: forall s t a b. (s -> Either a (b -> t)) -> Reel s t a b
reel dispatch g = reelE dispatch (\(Tuple b f) -> f b) g

-- | Construct a `Reel` straight from its **existential encoding**
-- | `∃c. (s → a + c) × (b × c → t)`: pick the residual `c`, then supply `decon`
-- | (match `s` as a fresh focus `a` or a resumed state `c`) and `recon` (combine
-- | the focus result `b` with the carried state `c` into `t`). The quantified `c`
-- | is exactly the eliminator of that existential; `retain` is the carrier. `reel`
-- | is this at the co-Yoneda witness `c := b → t` (`recon = \(Tuple b f) -> f b`,
-- | i.e. evaluation).
reelE :: forall @a @c s t b. (s -> Either a c) -> (Tuple b c -> t) -> Reel s t a b
reelE decon recon g = dimap decon recon (retain g)
