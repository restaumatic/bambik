-- | The **`Coprism`** — the optic `Data.Profunctor.Cochoice`'s `unleft`
-- | generates by Pastro–Street. The class is the ecosystem's; the optic is
-- | this library's, because `profunctor-lenses` never built it. Its dual is
-- | `Data.Lens.Prism`, and nothing here mentions a row: the row form is
-- | `Data.Profunctor.Row.VariantToVariant.iterate`.
-- |
-- | This module is a **complement of the ecosystem's own**, not bambik's: it
-- | claims a `Data.Lens.*` name because it belongs in that family beside
-- | `Lens`/`Prism`, and it lives under the separate `extras/lenses` source
-- | root to say so — nothing here mentions `PUI`, a row, or a carrier, so it
-- | could be lifted into `profunctor-lenses` unchanged.
module Data.Lens.Coprism
  ( Coprism
  , coprism
  , coprismE
  )
  where

import Control.Category (identity)
import Data.Either (Either, either)
import Data.Profunctor (dimap)
import Data.Profunctor.Cochoice (class Cochoice, unleft)

-- | The optic `unleft` induces: the **Coprism** — the prism run backwards
-- | (`Coprism s t a b ≅ Prism b a t s`). Eliminating the residual `c`
-- | (instantiated to `a`) by co-Yoneda collapses `∃c. (s + c → a) × (b → t + c)`
-- | to `(embed : s → a) × (step : b → t + a)`: every input becomes a focus,
-- | and every focus result either exits with `t` or **re-enters as the next
-- | focus input** — `tailRec` at the optic level. Where a prism's residual
-- | passes by visibly in the type, a coprism's circulates hidden as control
-- | flow. `iterate` is this optic at row granularity.
type Coprism s t a b = forall p. Cochoice p => p a b -> p s t

coprism :: forall s t a b. (s -> a) -> (b -> Either t a) -> Coprism s t a b
coprism embed step = coprismE (either embed identity) step

-- | Construct a `Coprism` straight from its **existential encoding**
-- | `∃c. (s + c → a) × (b → t + c)`: pick the looped channel `c`, then supply
-- | `decon` (read a fresh input or a looped value) and `recon` (exit or loop
-- | each emission). `coprism` is this at the co-Yoneda witness `c := a`.
coprismE :: forall s t a b c. (Either s c -> a) -> (b -> Either t c) -> Coprism s t a b
coprismE decon recon g = unleft (dimap decon recon g)
