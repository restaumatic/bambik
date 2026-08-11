-- | The **`Reel`** — the optic `Data.Profunctor.Retaining`'s `retain`
-- | generates by Pastro–Street, for the coined `+ → ×` shape. Both the class
-- | and the optic are this library's. Its co-optic is `Data.Lens.Coreel`, and
-- | nothing here mentions a row: the row form is
-- | `Data.Profunctor.Row.VariantToRecord.subRetaining`.
-- |
-- | It claims a `Data.Lens.*` name and lives under the separate
-- | `extras/lenses` source root because it belongs to that family by
-- | construction — an optic over a strength, mentioning no `PUI`, no row and
-- | no carrier. Unlike `Colens`/`Coprism` it could not be upstreamed alone:
-- | its class is coined too, so the pair travels together.
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

-- | The optic `retain` induces: the **Reel**. Eliminating the residual `c`
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
reelE :: forall s t a b c. (s -> Either a c) -> (Tuple b c -> t) -> Reel s t a b
reelE decon recon g = dimap decon recon (retain g)
