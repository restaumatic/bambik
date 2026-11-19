-- | The **`Coreel`** — the profunctor-encoded optic of
-- | `Data.Profunctor.Coretaining`'s `coretain`, for the coined `+ → ×`
-- | shape; both the class and the optic are this library's, its
-- | strength-side sibling `Data.Lens.Reel`. Like the other coined optics,
-- | the existential constructor `coreelE` is **sound** and completeness is
-- | **not claimed** (`Data.Lens.Shutter`'s note); the ∃-to-∃ duality
-- | `Coreel s t a b ≅ Shutter b a t s` (a pair swap) does hold. An optic
-- | over a co-strength, mentioning no `PUI`, row or carrier — hence the
-- | `Data.Lens.*` name and the `extras/lenses` root, travelling with its
-- | coined class. Law-tested, reached by no demo.
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

-- | The optic `coretain` encodes: the **Coreel** — the `Shutter` run
-- | backwards (`Coreel s t a b ≅ Shutter b a t s`, at the existential
-- | encodings: a pair swap). Eliminating the residual
-- | `c` (instantiated to `b`) by co-Yoneda collapses
-- | `∃c. (s + c → a) × (b → t × c)` to
-- | `(embed : s → a) × (out : b → t) × (resume : b → a)`: every emission
-- | both leaves as `t` and **re-enters as the next focus input** — a
-- | generator, producing on every step. It has no row form: an unfold's
-- | state is a model field under `looped` (`Data.Profunctor.Row`, *What
-- | is deliberately not here*).
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
