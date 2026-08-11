-- | The **`Colens`** — the optic `Data.Profunctor.Costrong`'s `unfirst`
-- | generates by Pastro–Street. The class is the ecosystem's; the optic is
-- | this library's, because `profunctor-lenses` never built it. Its dual is
-- | `Data.Lens.Lens`, and nothing here mentions a row: the row form is
-- | `Data.Profunctor.Row.RecordToRecord.feedback`.
-- |
-- | This module is a **complement of the ecosystem's own**, not bambik's: it
-- | claims a `Data.Lens.*` name because it belongs in that family beside
-- | `Lens`/`Prism`, and it lives under the separate `extras/lenses` source
-- | root to say so — nothing here mentions `PUI`, a row, or a carrier, so it
-- | could be lifted into `profunctor-lenses` unchanged.
module Data.Lens.Colens
  ( Colens
  , colens
  , colensE
  )
  where

import Data.Profunctor (dimap)
import Data.Profunctor.Costrong (class Costrong, unfirst)
import Data.Tuple (Tuple(..))

-- | The optic `unfirst` induces: the **Colens** — the lens run backwards
-- | (`Colens s t a b ≅ Lens b a t s`). Eliminating the residual `c`
-- | (instantiated to `b`) by co-Yoneda collapses `∃c. (s × c → a) × (b → t × c)`
-- | to `(join : s → b → a) × (out : b → t)`: each input is read **against the
-- | UI component's own last output** — the residual a lens would carry visibly in
-- | the type is hidden, threaded through state instead. The collapsed form
-- | shows why the `PUI` carrier gates it (there is no last output before the
-- | first emission). `feedback` is this optic at row granularity.
type Colens s t a b = forall p. Costrong p => p a b -> p s t

colens :: forall s t a b. (s -> b -> a) -> (b -> t) -> Colens s t a b
colens join out = colensE (\(Tuple s b) -> join s b) (\b -> Tuple (out b) b)

-- | Construct a `Colens` straight from its **existential encoding**
-- | `∃c. (s × c → a) × (b → t × c)`: pick the looped channel `c`, then supply
-- | `decon` (read the input joined with the channel) and `recon` (split each
-- | emission into the output and the channel's next value). `colens` is this
-- | at the co-Yoneda witness `c := b`.
colensE :: forall s t a b c. (Tuple s c -> a) -> (b -> Tuple t c) -> Colens s t a b
colensE decon recon g = unfirst (dimap decon recon g)
