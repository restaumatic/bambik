-- | The **`Colens`** — the optic `Data.Profunctor.Costrong`'s `unfirst`
-- | generates by the dual of Pastro–Street: the class is the ecosystem's,
-- | with a lawful `×`-action carrying unit and composition, so — unlike the
-- | coined mixed optics (`Data.Lens.Shutter`) — the representation is the
-- | standard co-optic result. The optic is this library's, because
-- | `profunctor-lenses` never built it; its dual is `Data.Lens.Lens`, and
-- | the whole-row knot is `PUI`'s `looped`. A complement of the
-- | ecosystem's own, mentioning no `PUI`, row or carrier, liftable into
-- | `profunctor-lenses` unchanged — hence the `extras/lenses` root.
-- | Law-tested, reached by no demo.
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
-- | first emission), so a raw colens needs its state channel primed
-- | before it passes anything. It has no row form: a loop's state
-- | is a model field under `looped` (`Data.Profunctor.Row`, *What is
-- | deliberately not here*).
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
