-- | The **`Coreel`** — the optic `Data.Profunctor.Coretaining`'s `coretain`
-- | generates by Pastro–Street, for the coined `+ → ×` shape. Both the class
-- | and the optic are this library's. Its strength-side sibling is
-- | `Data.Lens.Reel`, and nothing here mentions a row: the row form is
-- | `Data.Profunctor.Row.VariantToRecord.unfolding`.
-- |
-- | It claims a `Data.Lens.*` name and lives under the separate
-- | `extras/lenses` source root because it belongs to that family by
-- | construction — an optic over a co-strength, mentioning no `PUI`, no row
-- | and no carrier. Unlike `Colens`/`Coprism` it could not be upstreamed
-- | alone: its class is coined too, so the pair travels together.
module Data.Lens.Coreel
  ( Coreel
  , coreel
  , coreelE
  )
  where

import Data.Either (Either, either)
import Data.Profunctor (dimap)
import Data.Profunctor.Coretaining (class Coretaining, coretain)
import Data.Tuple (Tuple(..))

-- | The optic `coretain` induces: the **Coreel** — the `Shutter` run
-- | backwards (`Coreel s t a b ≅ Shutter b a t s`). Eliminating the residual
-- | `c` (instantiated to `b`) by co-Yoneda collapses
-- | `∃c. (s + c → a) × (b → t × c)` to
-- | `(embed : s → a) × (out : b → t) × (resume : b → a)`: every emission
-- | both leaves as `t` and **re-enters as the next focus input** — a
-- | generator, producing on every step. `unfolding @w` is this optic at row
-- | granularity.
type Coreel s t a b = forall p. Coretaining p => p a b -> p s t

coreel :: forall s t a b. (s -> a) -> (b -> t) -> (b -> a) -> Coreel s t a b
coreel embed out resume = coreelE (either embed resume) (\b -> Tuple (out b) b)

-- | Construct a `Coreel` straight from its **existential encoding**
-- | `∃c. (s + c → a) × (b → t × c)`: pick the resume channel `c`, then supply
-- | `decon` (read a fresh input or a resumed value) and `recon` (split each
-- | emission into the output and the channel's next value). `coreel` is this
-- | at the co-Yoneda witness `c := b`.
coreelE :: forall s t a b c. (Either s c -> a) -> (b -> Tuple t c) -> Coreel s t a b
coreelE decon recon g = coretain (dimap decon recon g)
