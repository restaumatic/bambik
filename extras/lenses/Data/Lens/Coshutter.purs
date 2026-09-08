-- | The **`Coshutter`** — the profunctor-encoded optic of
-- | `Data.Profunctor.Coresolving`'s `coresolve`, for the coined `× → +`
-- | shape. Both the class and the optic are this library's. Its
-- | strength-side sibling is `Data.Lens.Shutter`, and nothing here mentions
-- | a row: the row form is `Data.Profunctor.Row.RecordToVariant.folding`.
-- |
-- | Like the other coined optics: the existential constructor `coshutterE`
-- | is **sound**, and completeness is **not claimed** — see
-- | `Data.Lens.Shutter`'s note (the mixed strengths carry no unit or
-- | composition coherence, so Pastro–Street does not apply). The ∃-to-∃
-- | duality `Coshutter s t a b ≅ Reel b a t s` (a pair swap) does hold.
-- |
-- | It claims a `Data.Lens.*` name and lives under the separate
-- | `extras/lenses` source root because it belongs to that family by
-- | construction — an optic over a co-strength, mentioning no `PUI`, no row
-- | and no carrier. Unlike `Colens`/`Coprism` it could not be upstreamed
-- | alone: its class is coined too, so the pair travels together.
module Data.Lens.Coshutter
  ( Coshutter
  , coshutter
  , coshutterE
  )
  where

import Data.Either (Either)
import Data.Profunctor (dimap)
import Data.Profunctor.Coresolving (class Coresolving, coresolve)
import Data.Tuple (Tuple(..))

-- | The optic `coresolve` encodes: the **Coshutter** — the `Reel` run
-- | backwards (`Coshutter s t a b ≅ Reel b a t s`, at the existential
-- | encodings: a pair swap). Eliminating the residual
-- | `c` (instantiated to `s → a`) by co-Yoneda collapses
-- | `∃c. (s × c → a) × (b → t + c)` to a single `step : b → t + (s → a)`:
-- | each emission either exits with `t` or yields a **new way to read
-- | inputs** — the fold state is a reader. The collapsed form has no initial
-- | reader, which is exactly why the `PUI` carrier gates inputs until primed.
-- | `folding @w` is this optic at row granularity.
type Coshutter s t a b = forall p. Coresolving p => p a b -> p s t

coshutter :: forall s t a b. (b -> Either t (s -> a)) -> Coshutter s t a b
coshutter step = coshutterE (\(Tuple s f) -> f s) step

-- | Construct a `Coshutter` straight from its **existential encoding**
-- | `∃c. (s × c → a) × (b → t + c)`: pick the fold channel `c`, then supply
-- | `decon` (read the input joined with the fold state) and `recon` (exit or
-- | continue each emission). `coshutter` is this at the co-Yoneda witness
-- | `c := s → a`.
coshutterE :: forall s t a b c. (Tuple s c -> a) -> (b -> Either t c) -> Coshutter s t a b
coshutterE decon recon g = coresolve (dimap decon recon g)
